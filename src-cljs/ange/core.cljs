(ns ange.core
  (:require [reagent.core :as r]
            [reagent.session :as session]
            [reagent.debug :as debug]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [markdown.core :refer [md->html]]
            [ajax.core :refer [GET POST]]

            [clojure.string :as s]
            [cljs-time.core :as ct]
            [cljs-time.periodic :as cp]
            [cljs-time.format :as cf]

            [json-html.core :refer [edn->hiccup edn->html json->html]]
            [reagent-forms.core :as rf :refer [bind-fields init-field value-of]]
            [reagent-modals.modals :as reagent-modals]
            )
  (:import goog.History))

(def chinese2
  {
  "video_like" "视频赞"
  "video_fav" "视频收藏"
  "video_comment" "视频评论"
  "count" "次数"
  "user" "人数"
  "target" "被操作数"
  "all" "all"
  "and" "android"
  "iOS" "iOS"
  "video_play" "视频播放"
  "samples" "照做"
  "create" "视频成功创建"
  "film_create" "大片成功创建"
  "video_create" "短片成功创建"
  "share" "成功分享"
  "newuser_create" "新用户创建"
  "video_share" "视频分享"
  "album_share" "影集分享"
  "album_batch_create" "影集创建"
  "scene_batch_create" "素材创建"
  "video_play_place_digest" "短片热门播放"
  "video_play_place_timeline" "短片最新播放"
  "video_play_place_post" "短片剧组播放"
  "video_play_place_friends" "短片关注播放"
  "comply_create_place_digest" "照做热门"
  "comply_create_place_timeline" "照做最新"
  "comply_create_place_post" "照做剧组"
  "comply_create_place_friends" "照做关注"
  "play" "打开页面"
  "download" "点击下载"
  "ratio" "转化率(下载／打开)"
  "mm" "微信"
  "qq" "QQ"
  "video" "短片"
  "album" "影集"
  "film" "大片"
  "video_batch_create" "短片创建"
  "all_access_api" "访问量"
  "user_follow" "用户关注"
  "user_unfollow" "用户取消关注"
  "video_free" "短片自己成功创作"
  "video_sample" "短片模版成功照做"
  "video_station" "短片广场成功照做"
  "film_free" "大片自己成功创作"
  "film_sample" "大片模版成功照做"
  "film_station" "大片广场成功照做"
  "album_create" "影集成功创建"
  "video_share_2" "短片成功分享"
  "album_share_2" "影集成功分享"
  "film_share" "大片成功分享"
  "login_restart" "用户重启"
  "login_reinstall" "用户重装"
  "login_registered" "用户首次安装"
  "login_upgrade" "用户升级"
  "newbie_video" "新用户／视频总分享"
  "newbie_active" "新用户(3天内的)／活跃用户(人数)"
  "newbie_active_video_free_create" "活跃用户中的新用户(3天内的)／活跃用户(短片自己成功制作数)"
  "newbie_active_video_sample_create" "活跃用户中的新用户(3天内的)／活跃用户(短片模版成功照做数)"
  "newbie_active_video_station_create" "活跃用户中的新用户(3天内的)／活跃用户(短片广场成功照做数)"
  "newbie_active_film_create" "活跃用户中新用户(3天内的)／活跃用户(高清制作数)"
  "newbie_active_album_create" "活跃用户中新用户(3天内的)／活跃用户(影集制作数)"
  "per_video" "短片制作／活跃用户"
  "per_film" "高清制作／活跃用户"
  "per_album" "影集制作／活跃用户"
  "ratio_video_share" "短片分享／短片制作"
  "ratio_album_share" "影集分享／影集制作"
  "ratio_sample" "照做／视频制作"
  "ratio_follow" "关注／活跃用户"
  "ratio_like" "赞／活跃用户"
"active_user" "活跃用户"
"post_create" "创建剧组"
"post_follow" "关注剧组"
"post_upload_video" "上传视频到剧组"
"post_upload_statuses" "上传素材到剧组"
"post_play" "剧组中播放视频"
"post_video_create" "剧组中短片创建"
"post_film_create" "剧组中大片创建"
"post_share" "剧组分享"
"post_upload_photo" "上传照片成功到剧组"
"post_upload_audio" "上传音频成功到剧组"
"post_upload_scene" "上传场景成功到剧组"
"post_upload_video_2" "上传视频成功到剧组"
"not_match" "not_match"
"matched" "matched"
"matched_10001" "matched_旅行"
"matched_10004" "matched_友情聚会"
"matched_10003" "matched_家庭亲子"
"matched_10002" "matched_恋爱表白"
"matched_10005" "matched_生日祝福"
"matched_10000" "matched_自己创作"
"matched_10006" "matched_不开心"
"matched_10007" "matched_中秋节"
"matched_10008" "matched_幸福快乐"
"matched_10009" "matched_生活回忆"
"matched_10010" "matched_舞蹈"
"matched_10011" "matched_海边"
"matched_10012" "matched_教师节"
"matched_10015" "matched_奋斗拼搏"
"bug" "bug"
}
)

(defn chinese [word] ; for date number (i.e "20151023")
  (let [c (chinese2 word)]
    (or c word)))

(defn parse-date [date] (cf/parse (cf/formatter "yyyy-MM-dd") date))
(defn unparse-date [date] (cf/unparse (cf/formatter "yyyy-MM-dd") date))
(defn period-date [from to]
  (mapv unparse-date
        (take (inc (ct/in-days (ct/interval (parse-date from) (parse-date to))))
              (cp/periodic-seq (parse-date from) (ct/days 1)))))

(defn before-n [n]
  (fn [date]
    (unparse-date (ct/plus (parse-date date) (ct/days (* -1 n))))))
(def before1 (before-n 1))
(def before7 (before-n 7))

(def today (unparse-date (ct/now)))
(defn offset-today [n] (unparse-date (ct/plus (ct/now) (ct/days n))))
(def yesterday (offset-today -1))
(def the-day-before-yesterday (offset-today -2))
(def lastweekday-of-yesterday (offset-today -8))
(def lastmonthday-of-yesterday (offset-today -31))

(defn unparse-date2 [date] (cf/unparse (cf/formatter "HH:mm") date))
(defn parse-date2 [date] (cf/parse (cf/formatter "yyyy-MM-dd-HH:mm") date))
(defn period-date2 [from to]
  (mapv unparse-date2
        (take (/ (* 24 60) 10)
              (cp/periodic-seq (parse-date2 from) (ct/minutes 10)))))
(def period-10mins-oneday (period-date2 "2015-09-10-00:00" "2015-09-10-23:50")) ;; static display mins

;;

(defn date-component [value]
  [:input {:type "date"
           :value @value
           :on-change (fn [e] (reset! value (-> e .-target .-value)))}])

(defn select-component
  ([value selects]
   [:select
    {:value @value
     :on-change (fn [e] (reset! value (-> e .-target .-value)))}
    (for [[text v] selects]
      [:option {:value v} text])])
  ([value selects group] ;;TODO auto detect group or not
   [:select
    {:value @value
     :on-change (fn [e] (reset! value (-> e .-target .-value)))}
    (for [[label ss] selects]
      [:optgroup {:label label}
       (for [[text v] ss] 
         [:option {:value v} text])])]))

(def ios-versions (r/atom []))
(def and-versions (r/atom []))

(defn select-versions-component [value]
  (fn []
    [:select
     {:value @value
      :on-change (fn [e] (reset! value (-> e .-target .-value)))}
     [:optgroup {:label "Group1-all"}
      [:option {:value "all"} "all"]]
     [:optgroup {:label "Group2-iOS"}
      [:option {:value "all"} "all"]
      (for [v @ios-versions]
        [:option {:value v} v])]
     [:optgroup {:label "Group3-android"}
      [:option {:value "all"} "all"]
      (for [v @and-versions]
        [:option {:value v} v])]]))

(defn button-component [text f]
  [:button {:on-click f} text])

(defn graph-data [config id]
  (.highcharts (js/$ (str "#" id))
               (clj->js @config))
  [:div])

(defn graph [config id]
  [:div
   [:div {:id id 
          :style {:min-width "310px" :max-width "2000px"
                  :height "400px" :margin "0 auto"}}]
   [graph-data config id]])

(defn chart-unity-component [multi-selects-component getstr graph-id cominit]
  (let [stuff (r/atom [])
        from (r/atom lastweekday-of-yesterday)
        to (r/atom yesterday)
        compound (r/atom cominit)
        ca (r/atom #{})
        config (r/atom
                 {:chart {:type "line"}
                  :title {:text ""
                          :x -20}
                  :subtitle {:text ""
                             :x -20}
                  :xAxis {:categories []}
                  :yAxis {:title {:text "数量"}
                          :plotLines [{:value 0 :width 1 :color "#808080"}]}
                  :tooltip {:valueSuffix ""}
                  :legend {:layout "vertical"
                           :align "right"
                           :verticalAlign "middle"
                           :borderWidth 0}
                  :series [{:name "sample"
                            :data [1 2 4]}]})
        get-stuff (fn [] 
                    (GET (str "/" getstr)
                         {:params {:from @from :to @to :ca (s/join "," @ca)}
                          :handler (fn [response]
                                     (debug/prn response)
                                     (reset! stuff response)
                                     (swap! config assoc :series (vec (for [{name "name" data "data"} @stuff]
                                                                        {:name (s/join "," (map chinese (s/split name #"-")))
                                                                         :data data})))
                                     (swap! config assoc-in [:xAxis :categories] (period-date @from @to))
                                     )})
                    (GET "/sync-ios-versions" {:handler (fn [response] (reset! ios-versions response))})
                    (GET "/sync-and-versions" {:handler (fn [response] (reset! and-versions response))}))
        add  #(swap! ca conj (s/join "-" @compound))
        clear #(reset! ca #{})
        _ (add-watch ca :get get-stuff)
        _ (add-watch from :get get-stuff)
        _ (add-watch to :get get-stuff)
        ]
    (r/create-class
      {:reagent-render
       (fn []
         [:div ;;must
          [date-component from]
          [date-component to]
          [multi-selects-component 
           (mapv #(r/cursor compound [%]) (range (count cominit)))
           add clear]
          [graph config graph-id]])
       :component-did-mount
       (fn [this]
         (add)
         (get-stuff))})))
       
;; before are some public components

(defn nav-link [uri title page collapsed?]
  [:li {:class (when (= page (session/get :page)) "active")}
   [:a {:href uri
        :on-click #(reset! collapsed? true)}
    title]])

(defn navbar []
  (let [collapsed? (r/atom true)]
    (fn []
      [:nav.navbar.navbar-inverse.navbar-fixed-top
       [:div.container
        [:div.navbar-header
         [:button.navbar-toggle
          {:class         (when-not @collapsed? "collapsed")
           :data-toggle   "collapse"
           :aria-expanded @collapsed?
           :aria-controls "navbar"
           :on-click      #(swap! collapsed? not)}
          [:span.sr-only "Toggle Navigation"]
          [:span.icon-bar]
          [:span.icon-bar]
          [:span.icon-bar]]
         [:a.navbar-brand {:href "#/"} "ange"]]
        [:div.navbar-collapse.collapse
         (when-not @collapsed? {:class "in"})
         [:ul.nav.navbar-nav
         ;[:ul.nav.nav-pills
          ;[nav-link "#/" "Home" :home collapsed?]
          ;[nav-link "#/about" "About" :about collapsed?]
          ;[nav-link "#/test" "Test" :test collapsed?]
          [nav-link "#/day" "Day" :day collapsed?]
          [nav-link "#/mins" "Mins" :mins collapsed?]
          [nav-link "#/ratio" "Ratio" :ratio collapsed?]
          [nav-link "#/user" "User" :user collapsed?]
          [nav-link "#/post" "Post" :post collapsed?]
          [nav-link "#/mold" "Mold" :mold collapsed?]
          [nav-link "#/version-table" "VersionTable" :version-table collapsed?]
          [nav-link "#/daily-table" "DailyTable" :daily-table collapsed?]
          [nav-link "#/rank" "Rank" :rank collapsed?]
          [nav-link "#/sample" "Sample" :sample collapsed?]
          [nav-link "#/compare" "Compare" :compare collapsed?]
          [nav-link "#/search" "Search" :search collapsed?]
          [nav-link "#/test" "Test" :test collapsed?]
          ]]]])))

(defn home-page []
  [:div.container
   [:div.jumbotron
    [:h1 "Welcome to ange"]
    [:p "Time to start building your site!"]
    [:p [:a.btn.btn-primary.btn-lg {:href "http://luminusweb.net"} "Learn more »"]]]
   [:div.row
    [:div.col-md-12
     [:h2 "Welcome to ClojureScript"]]]
   (when-let [docs (session/get :docs)]
     [:div.row
      [:div.col-md-12
       [:div {:dangerouslySetInnerHTML
              {:__html (md->html docs)}}]]])])

(defn about-page []
  [:div.container
   [:div.row
    [:div.col-md-12
     "this is the story of ange... work in progress"]]])

;; test

(defn test-page []
  (let [from (r/atom lastweekday-of-yesterday)
        ]
    (fn []
  [:div.container
   [:div.row

    [:table.table.table-bordered
     [:tbody
      [:tr
       [:th {:colspan "2"} "2"]]
      [:tr
       [:th 1]
       [:th 2]]]]

    [date-component from]

   (edn->hiccup {:foo [1 2 @from] :bar "baz"})

   (edn->hiccup {:foo {:a 1 :b 2} :bar "baz"})

    [:div.jh-type-object
     [:table.jh-type-object
      [:tbody
       ;(for [[k v] {:a 1 :b 2 :c 4}]
       (for [[k v] (zipmap (range 4) [40 5 6 7])]
         ^{:key k} [:tr
                    [:th k]
                    [:td v]])]]]

    ]])))

;; day-page

(defn day-selects1-component [[a b c d] + --]
  [:div
   [select-component a 
    [["用户关注" "user_follow"]
     ["用户取消关注" "user_unfollow"]
     ["视频赞" "video_like"]
     ["视频收藏" "video_fav"]
     ["视频评论" "video_comment"]
     ["api访问量" "all_access_api"]]]
   [select-component b
    [["次数" "count"]
     ["人数" "user"]
     ["被操作数" "target"]]]
   [select-component c 
    [["all" "all"]
     ["android" "and"]
     ["iOS" "iOS"]]]
   ;[select-component d 
    ;mutative-versions "group"]
   [select-versions-component d]
   [button-component "+" +]
   [button-component "--" --]])

(defn day-selects2-component [[a b c d] + --]
  [:div
   [select-component a 
    [["Group1" [["视频播放" "video_play"]]]
     ["Group2" [["照做" "samples"]
                ["视频成功创建" "create"]
                ["影集成功创建" "album_create"]
                ["大片成功创建" "film_create"]
                ["短片成功创建" "video_create"]]]
     ["Group3" [["短片自己创作" "video_free"]
                ["短片模版照做" "video_sample"]
                ["短片广场照做" "video_station"]
                ["大片自己创作" "film_free"]
                ["大片模版照做" "film_sample"]
                ["大片广场照做" "film_station"]]]
     ["Group4" [["成功分享" "share"]
                ["短片成功分享" "video_share_2"]
                ["影集成功分享" "album_share_2"]
                ["大片成功分享" "film_share"]]]
     ["Group5" [["新用户创建" "newuser_create"]]]
     ["Group6" [["短片创建" "video_batch_create"]
                ["短片分享" "video_share"]
                ["影集分享" "album_share"]
                ["影集创建" "album_batch_create"]
                ["素材创建" "scene_batch_create"]]]
     ["Group7" [["短片热门播放" "video_play_place_digest"]
                ["短片最新播放" "video_play_place_timeline"]
                ["短片剧组播放" "video_play_place_post"]
                ["短片关注播放" "video_play_place_friends"]]]
     ["Group8" [["照做热门" "comply_create_place_digest"]
                ["照做最新" "comply_create_place_timeline"]
                ["照做剧组" "comply_create_place_post"]
                ["照做关注" "comply_create_place_friends"]]]] "group"]
   [select-component b 
    [["次数" "count"]
     ["人数" "user"]
     ["被操作数" "target"]]]
   [select-component c 
    [["all" "all"]
     ["android" "and"]
     ["iOS" "iOS"]]]
   [select-versions-component d]
   [button-component "+" +]
   [button-component "--" --]])

(defn day-selects3-component [[a b c d] + --]
  [:div
   [select-component a 
    [["打开页面" "play"]
     ["点击下载" "download"]
     ["转化率(下载／打开)" "ratio"]]]
   [select-component b 
    [["all" "all"]
     ["iOS" "ios"]
     ["android" "android"]]]
   [select-component c
    [["all" "all"]
     ["微信" "mm"]
     ["QQ" "qq"]]]
   [select-component d 
    [["all" "all"]
     ["短片" "video"]
     ["影集" "album"]
     ["大片" "film"]]]
   [button-component "+" +]
   [button-component "--" --]])

(defn day-page []
  [:div.container
   [:div.row
    [:label "1 . 社区活跃度:"]
    [chart-unity-component day-selects1-component "day" "day-graph1" ["user_follow" "count" "all" "all"]]
    [:label "2 . 视频播放分享创建数量:"]
    [chart-unity-component day-selects2-component "day" "day-graph2" ["video_play" "count" "all" "all"]]
    [:label "3 . 渠道:"]
    [chart-unity-component day-selects3-component "access" "day-graph3" ["play" "all" "all" "all"]]]])

;; mins

(defn mins-selects-component [date bi os]
  [:div
   [date-component date]
   [select-component bi 
    [["Group1" [["视频分享" "video_share"]
                ["视频赞" "video_like"]
                ["视频收藏" "video_fav"]
                ["视频评论" "video_comment"]
                ["视频播放" "video_play"]
                ["视频创建" "video_batch_create"]
                ["访问量" "all_access_api"]]]
     ["Group2" [["影集分享" "album_share"]
                ["影集创建" "album_batch_create"]
                ["素材创建" "scene_batch_create"]]]
     ["Group3" [["用户关注" "user_follow"]
                ["用户取消关注" "user_unfollow"]]]
     ["Group4" [["短片自己成功创作" "video_free"]
                ["短片模版成功照做" "video_sample"]
                ["短片广场成功照做" "video_station"]
                ["大片自己成功创作" "film_free"]
                ["大片模版成功照做" "film_sample"]
                ["大片广场成功照做" "film_station"]
                ["影集成功创建" "album_create"]]]
     ["Group5" [["短片成功分享" "video_share_2"]
                ["影集成功分享" "album_share_2"]
                ["大片成功分享" "film_share"]]]
     ["Group6" [["用户重启" "login_restart"]
                ["用户重装" "login_reinstall"]
                ["用户首次安装" "login_registered"]
                ["用户升级" "login_upgrade"]]]
     ["Group7" [["短片热门播放" "video_play_place_digest"]
                ["短片最新播放" "video_play_place_timeline"]
                ["短片剧组播放" "video_play_place_post"]
                ["短片关注播放" "video_play_place_friends"]]]] "group"]
   [select-component os 
    [["all" "all"]
     ["android" "and"]
     ["iOS" "iOS"]]]])

(def mins-radio-component-head
  [:div
   [:input {:field :radio :value today :name :foo} "今天"]
   [:input {:field :radio :value yesterday :name :foo} "昨天"]])

(def mins-radio-component-bottom
  [:div {:style {:text-align "center"}}
   [:label "对比："]
   [:input {:field :radio :value before1 :name :baz} "前一日"]
   [:input {:field :radio :value before7 :name :baz} "上周同期"]])

(defn mins-page []
  (let [stuff (r/atom nil)
        radio1 (r/atom {:foo today})
        radio2 (r/atom {:baz before1}) ;; a toggle fn
        date1 (r/cursor radio1 [:foo])
        date2 (r/atom yesterday)
        bi1 (r/atom "video_share")
        bi2 (r/atom "video_share")
        os1 (r/atom "all")
        os2 (r/atom "all")
        config (r/atom
                 {:chart {:type "line"}
                  :title {:text "十分钟统计量"
                          :x -20}
                  :subtitle {:text ""
                             :x -20}
                  :xAxis {:categories period-10mins-oneday}
                  :yAxis {:title {:text "数量"}
                          :plotLines [{:value 0 :width 1 :color "#808080"}]}
                  :tooltip {:valueSuffix "个"}
                  :legend {:layout "vertical"
                           :align "right"
                           :verticalAlign "middle"
                           :borderWidth 0}
                  :series [{:name "sample"
                            :data [1 2 3]}]})
        get-stuff #(GET "/mins"
                        {:params {:ca (s/join "," [(s/join "-" [(s/replace (:foo @radio1) "-" "") @bi1 @os1])
                                                   (s/join "-" [(s/replace @date2 "-" "") @bi2 @os2])])}
                         :handler (fn [response]
                                    (reset! stuff response)
                                    (swap! config assoc :series (vec (for [{name "name" data "data"} @stuff]
                                                                        {:name (s/join "," (map chinese (s/split name #"-")))
                                                                        :data data})))
                                    )})
        syn-date (fn [] (reset! date2 ((:baz @radio2) (:foo @radio1))))
        _ (add-watch radio1 :sync syn-date)
        _ (add-watch radio2 :sync syn-date)
        _ (add-watch date2 :get get-stuff)
        _ (add-watch bi1 :sync (fn [] (reset! bi2 @bi1)))
        _ (add-watch os1 :sync (fn [] (reset! os2 @os1)))
        _ (add-watch bi2 :get get-stuff)
        _ (add-watch os2 :get get-stuff)
        ]
       (fn []
         (get-stuff)
         (js/setInterval get-stuff (* 10 60 1000))
         [:div.container
          [:div.row
           [rf/bind-fields mins-radio-component-head radio1]
           [graph config "mins-graph"]
           [rf/bind-fields mins-radio-component-bottom radio2]
           [mins-selects-component date1 bi1 os1] 
           [mins-selects-component date2 bi2 os2]]])))

;; ratio

(defn ratio-selects-component [[a b c] + --]
  [:div
   [select-component a
    [["Group1" [["新用户／视频总分享" "newbie_video"]
                ["新用户(3天内的)／活跃用户(人数)" "newbie_active"]
                ["活跃用户中的新用户(3天内的)／活跃用户(短片自己成功制作数)" "newbie_active_video_free_create"]
                ["活跃用户中的新用户(3天内的)／活跃用户(短片模版成功照做数)" "newbie_active_video_sample_create"]
                ["活跃用户中的新用户(3天内的)／活跃用户(短片广场成功照做数)" "newbie_active_video_station_create"]
                ["活跃用户中新用户(3天内的)／活跃用户(高清制作数)" "newbie_active_film_create"]
                ["活跃用户中新用户(3天内的)／活跃用户(影集制作数)" "newbie_active_album_create"]]]
     ["Group2" [["短片制作／活跃用户" "per_video"]
                ["高清制作／活跃用户" "per_film"]
                ["影集制作／活跃用户" "per_album"]]]
     ["Group3" [["短片分享／短片制作" "ratio_video_share"]
                ["影集分享／影集制作" "ratio_album_share"]
                ["照做／视频制作" "ratio_sample"]
                ["关注／活跃用户" "ratio_follow"]
                ["赞／活跃用户" "ratio_like"]]]] "group"]
   [select-component b
    [["all" "all"]
     ["android" "and"]
     ["iOS" "iOS"]]]
   [select-versions-component c]
   [button-component "+" +]
   [button-component "--" --]])

(defn ratio-page []
  [:div.container
   [:div.row
    [chart-unity-component ratio-selects-component "ratio" "ratio-graph" ["newbie_video" "all" "all"]]]])

;user 

(defn user-selects1-component [[a b c d] + --]
  [:div
   [select-component a
    [["用户首装" "login_registered"]
     ["用户重启" "login_restart"]
     ["用户重装" "login_reinstall"]
     ["用户升级" "login_upgrade"]]]
   [select-component b
    [["次数" "count"]
     ["人数" "user"]]]
   [select-component c
    [["all" "all"]
     ["android" "and"]
     ["iOS" "iOS"]]]
   [select-versions-component d]
   [button-component "+" +]
   [button-component "--" --]])

(defn user-selects2-component [[a b c d] + --]
  [:div
   [select-component a
    [["活跃用户" "active_user"]]]
   [select-component b
    [["人数" "user"]]]
   [select-component c
    [["all" "all"]
     ["android" "and"]
     ["iOS" "iOS"]]]
   [select-versions-component d]
   [button-component "+" +]
   [button-component "--" --]])

(defn user-page []
  [:div.container
   [:div.row
    [:label "1 . 每日login用户数:"]
    [chart-unity-component user-selects1-component "day" "user-graph1" ["login_registered" "count" "all" "all"]]
    [:label "2 . 每日活跃用户数:"]
    [chart-unity-component user-selects2-component "day" "user-graph2" ["active_user" "user" "all" "all"]]]])

;; post

(defn post-selects-component [[a b c] + --]
  [:div
   [select-component a
    [["创建剧组" "post_create"]
     ["关注剧组" "post_follow"]
     ["上传视频到剧组" "post_upload_video"]
     ["上传素材到剧组" "post_upload_statuses"]
     ["剧组中播放视频" "post_play"]
     ["剧组中短片创建" "post_video_create"]
     ["剧组中大片创建" "post_film_create"]
     ["剧组分享" "post_share"]
     ["上传照片成功到剧组" "post_upload_photo"]
     ["上传音频成功到剧组" "post_upload_audio"]
     ["上传场景成功到剧组" "post_upload_scene"]
     ["上传视频成功到剧组" "post_upload_video_2"]]]
   [select-component b
    [["次数" "count"]
     ["人数" "user"]
     ["被操作数" "target"]]]
   [select-component c
    [["all" "all"]
     ["android" "and"]
     ["iOS" "iOS"]]]
   [button-component "+" +]
   [button-component "--" --]])

(defn post-page []
  [:div.container
   [:div.row
    [:label "剧组:"]
    [chart-unity-component post-selects-component "day" "post-graph" ["post_create" "count" "all"]]]])

;; mold

(def mold-selects
  [["not_match" "not_match"]
   ["matched" "matched"]
   ["matched_旅行" "matched_10001"]
   ["matched_友情聚会" "matched_10004"]
   ["matched_家庭亲子" "matched_10003"]
   ["matched_恋爱表白" "matched_10002"]
   ["matched_生日祝福" "matched_10005"]
   ["matched_自己创作" "matched_10000"]
   ["matched_不开心" "matched_10006"]
   ["matched_中秋节" "matched_10007"]
   ["matched_幸福快乐" "matched_10008"]
   ["matched_生活回忆" "matched_10009"]
   ["matched_舞蹈" "matched_10010"]
   ["matched_海边" "matched_10011"]
   ["matched_教师节" "matched_10012"]
   ["matched_奋斗拼搏" "matched_10015"]
   ["bug" "bug"]])

(defn mold-selects-component [[a] + --]
  [:div
   [select-component a mold-selects]
   [button-component "+" +]
   [button-component "--" --]])

(defn mold-list-component []
  (let [stuff (r/atom [])
        date (r/atom yesterday)
        cate (r/atom "not_match")
        get-stuff #(GET "/moldlist"
                        {:params {:date @date :cate @cate}
                         :handler (fn [response] 
                                    (reset! stuff response))})
        _ (add-watch date :get get-stuff)
        _ (add-watch cate :get get-stuff)
        ]
    (fn []
      [:div
       [date-component date]
       [select-component cate mold-selects]
       [:ul
        (for [e @stuff]
          [:li e])]])))

(defn mold-page []
  [:div.container
   [:div.row
    [:label "每日mold数"]
    [chart-unity-component mold-selects-component "mold" "mold-graph" ["not_match"]]
    [:label "mold词"]
    [mold-list-component]
    ]])

;; version-table

(defn version-table-tr [os]
  [:tr
   [:th (str os "版本号")]
   [:th "活跃用户数"] 
   [:th "短片自己创作数"]
   [:th "短片模版照做数"]
   [:th "短片广场照做数"]
   [:th "短片数"]
   [:th "短片数／活跃用户数"]
   [:th "大片创建数"] 
   [:th "大片创建数／活跃用户数"] 
   [:th "影集创建数"]
   [:th "影集创建数／活跃用户数"]
   [:th "创作总数"]
   [:th "创作总数／活跃用户数"]])

(defn version-table-page []
  (let [stuff-and (r/atom nil)
        stuff-ios (r/atom nil)
        date (r/atom yesterday)
        get-stuff (fn [] 
                    (GET "/version-table"
                         {:params {:date @date :os "and"}
                          :handler (fn [response] (reset! stuff-and response))})
                    (GET "/version-table"
                         {:params {:date @date :os "iOS"}
                          :handler (fn [response] (reset! stuff-ios response))}))
        _ (add-watch date :get get-stuff)
        ]
    (r/create-class
      {:reagent-render
       (fn []
         [:div.container
          [:div.row
           [date-component date]
           [:div
            [:table.table.table-bordered.table-striped
             [:tbody
              (version-table-tr "android")
              (for [e @stuff-and]
                [:tr
                 (for [u e]
                   [:td u])])]]]
           [:div
            [:table.table.table-bordered.table-striped
             [:tbody
              (version-table-tr "ios")
              (for [e @stuff-ios]
                [:tr
                 (for [u e]
                   [:td u])])]]]]])
       :component-did-mount
       (fn [this]
         (get-stuff))})))

;; daily-table

(def daily-th2
  [:tr
   [:th "日期"]
   [:th "新用户数"]
   [:th "新用户数／总视频分享数(转化率)"]
   [:th "活跃用户数"]

   [:th {:colspan "6"} "视频创建"]
   [:th {:colspan "6"} "视频分享"]
   [:th {:colspan "2"} "照做"]
   [:th {:colspan "2"} "关注"]
   [:th "收藏数"]
   [:th {:colspan "2"} "赞"]
   [:th {:colspan "5"} "剧组"]])

(def daily-th
  [:tr
   [:th "日期"]
   [:th "新用户数"]
   [:th "新用户数／总视频分享数(转化率)"]
   [:th "活跃用户数"]

   [:th "短片创作数"]
   [:th "高清创作数"]
   [:th "影集创作数"]
   [:th "短片创作数／活跃用户数"]
   [:th "高清创作数／活跃用户数"]
   [:th "影集创作数／活跃用户数"]

   [:th "短片分享数"]
   [:th "高清分享数"]
   [:th "影集分享数"]
   [:th "短片分享数／短片创作数"]
   [:th "高清分享数／高清创作数"]
   [:th "影集分享数／影集创作数"]

   [:th "照做数"]
   [:th "（短片创作数＋高清创作数）／照做数"]

   [:th "关注数"]
   [:th "关注数／活跃用户数"]

   [:th "收藏数"]

   [:th "赞数"]
   [:th "赞数／活跃用户数"]

   [:th "加入剧组数"]
   [:th "新建剧组数"]
   [:th "上传素材数"]
   [:th "剧组短片数"]
   [:th "剧组大片数"]])

(defn render-compare-color [data] 
  (for [j (range (count data))]
    (let [oneday (data j)
          lastday (when-not (= j (dec (count data)))
                    (data (inc j)))]
      (if (= j (dec (count data))) ;; lastest day no color
        [:tr
         (for [u oneday]
           [:td u])]
        [:tr
         (for [i (range (count oneday))]
           (let [u (oneday i)
                 ul (lastday i)
                 ret (compare u ul)]
             (if (= i 0) ;; date no color
               [:td u]
               (case ret
                 1  [:td {:style {:color "red"}} u]
                 -1 [:td {:style {:color "green"}} u]
                 0  [:td u]))))]))))

(defn daily-table-page []
  (let [stuff (r/atom [])
        from (r/atom lastweekday-of-yesterday)
        to (r/atom yesterday)
        get-stuff #(GET "/daily-table"
                        {:params {:from @from
                                  :to @to}
                         :handler (fn [response]
                                    (debug/prn "daily-table-response=" response)
                                    (reset! stuff response)
                                    )})
        _ (add-watch from :get get-stuff)
        _ (add-watch to :get get-stuff)
        ]
    (r/create-class
      {:reagent-render
       (fn []
         [:div
          [date-component from]
          [date-component to]

          [:div
           [:label "iOS"]
           [:table.table.table-bordered.table-striped {:border 19}
            [:tbody
             [:tr [:th {:colspan 4} "ssss"]]
             daily-th
             (render-compare-color (first @stuff))
             ]]]

          ;[:div
           ;[:label "Android"]
           ;[:table.table.table-bordered.table-striped
            ;[:tbody
             ;daily-th
             ;;daily-th2
             ;(render-compare-color (second @stuff))]]]
          ;[:div
           ;[:label "iOS + Android"]
           ;[:table.table.table-bordered.table-striped
            ;[:tbody
             ;daily-th
             ;;daily-th2
             ;(render-compare-color (last @stuff))]]]

          ;(edn->hiccup (first @stuff))


          ])
       :component-did-mount
       (fn [this]
         (get-stuff))})))

;; rank 

(defn rank-page []
  (let [stuff (r/atom [])
        date (r/atom yesterday)
        cate (r/atom "rank200_video_station_create")
        get-stuff #(GET "/rank" 
                        {:params {:date @date :cate @cate}
                         :handler (fn [response] (reset! stuff response))})
        _ (add-watch date :get get-stuff)
        _ (add-watch cate :get get-stuff)
        ]
    (r/create-class
      {:reagent-render
       (fn []
         [:div.container
          [:div.row
           [date-component date]
           [select-component cate 
            [["被照做的前200名" "rank200_video_station_create"]
             ["被收藏的前200名" "rank200_video_fav"]
             ["网页端播放的前200名" "rank200_video_play_onweb"]]]
           [:div
            [:table.table.table-bordered.table-striped
             [:tbody
              [:tr
               [:th "数量"]
               [:th "视频链接"]
               [:th "后台链接"]
               [:th "删除并封禁"]]
              (for [e @stuff]
                (let [video-id (str (first e))]
                [:tr
                 [:td (second e)]
                 [:td [:a {:target "_blank"
                           :href (str "http://video.colorv.cn/play/" video-id)}
                       [:img {:src (last e) :height 100 :width 200}]]]
                 [:td [:a {:target "_black"
                           :href (str "http://120.26.123.32/mng/video/" video-id)}
                       (str "http://120.26.123.32/mng/video/" video-id)]]
                 [:td [:a {:target "_blank"
                           :on-click #(js/confirm "确定删？")
                           :href (str "http://120.26.123.32/mng/video/removetrue/video," video-id "?freeze=1")}
                       "删除并封禁"]]
                 ]))]]]]])
       :component-did-mount
       (fn [this]
         (get-stuff))})))

;; sample

(defn save-div [a b]
  (let [save-b #(if (zero? %) 1 %)
        res (/ a (save-b b))]
    (.toFixed res 4)))

(defn sample-page []
  (let [stuff (r/atom nil)
        date (r/atom yesterday)
        os (r/atom "iOS")
        platform (r/atom "qq")
        age_zone (r/atom "1")
        gender (r/atom "male")
        get-stuff #(GET "/sample"
                        {:params {:date @date 
                                  :os @os 
                                  :platform @platform 
                                  :age_zone @age_zone 
                                  :gender @gender}
                         :handler (fn [response]
                                    (debug/prn "sample-response=" response)
                                    (if (empty? response) ;; weibo usually empty
                                      (reset! stuff response)
                                      (let [sum-vec (apply (partial mapv +) (map rest response))
                                            sum-vec (vec (cons "sum" sum-vec)) ;; cons & conj
                                            vecs (conj response sum-vec)
                                            ;; add ratio to third element
                                            vecs (map (fn [v]
                                                        (assoc-in v [2]
                                                                  (str (v 2)
                                                                       " ("
                                                                       (save-div (v 2) (v 1))
                                                                       ")")))
                                                      vecs)]
                                        (reset! stuff vecs))))})
        _ (add-watch date :get get-stuff)
        _ (add-watch os :get get-stuff)
        _ (add-watch platform :get get-stuff)
        _ (add-watch age_zone :get get-stuff)
        _ (add-watch gender :get get-stuff)
        ]
    (r/create-class
      {:reagent-render
       (fn []
         [:div.container
          [:div.row
           [date-component date]
           [:div
            [select-component os
             [["iOS" "iOS"]
              ["android" "and"]
              ["all" "all"]]]
            [select-component platform
             [["QQ" "qq"]
              ["微信" "weixin"]
              ["微博" "weibo"]
              ["all" "all"]]]
            [select-component age_zone
             [["00后" "1"]
              ["90后" "2"]
              ["80后" "3"]
              ["70后" "4"]
              ["60后" "5"]
              ["无" "0"]
              ["all" "all"]]]
            [select-component gender
             [["男" "male"]
              ["女" "female"]
              ["无" ""]
              ["all" "all"]]]]
           [:div
            [:table.table.table-bordered.table-striped
             [:tbody
              [:tr
               [:th "版本"]
               [:th "活跃用户数"]
               [:th "照做总数 (照做总数／活跃用户数)"]
               [:th "热门"]
               [:th "最新"]
               [:th "剧组"]
               [:th "关注"]
               [:th "用户页"]
               [:th "推荐"]
               [:th "用户播放视频数"]]
              (for [e @stuff]
                [:tr
                 (for [u e]
                   [:td u])])]]]]])
       :component-did-mount
       (fn [this]
         (get-stuff))})))

;; compare

(defn compare-page []
  (let [stuff (r/atom nil)
        date (r/atom the-day-before-yesterday)
        get-stuff #(GET "/compare"
                        {:params {:date @date}
                         :handler (fn [response] (reset! stuff response))})
        _ (add-watch date :get get-stuff)
        ]
    (r/create-class
      {:reagent-render
       (fn []
         [:div.container
          [:div.row
           [date-component date]
           [:div
            [:table.table.table-bordered.table-striped
             [:tbody ;;must
              [:tr
               [:th "用户类别"]
               [:th "用户数 (/总数)"]
               [:th "播放 (/用户数)"]
               [:th "短片制作 (/用户数)"]
               [:th "短片自己创建 (/用户数)"]
               [:th "短片模版照做 (/用户数)"]
               [:th "短片广场照做 (/用户数)"]
               [:th "影集制作 (/用户数)"]
               [:th "大片制作 (/用户数)"]
               [:th "点赞 (/用户数)"]
               [:th "关注 (/用户数)"]]
              (for [e @stuff]
                [:tr
                 (for [u e]
                   [:td u])])]]]]])
       :component-did-mount
       (fn [this]
         (get-stuff))})))

;; search

(defn search-page []
  (let [stuff (r/atom nil)
        date (r/atom yesterday)
        cate (r/atom "scene")
        get-stuff #(GET "/search"
                        {:params {:date @date :cate @cate}
                         :handler (fn [response] 
                                    (reset! stuff response))})
        _ (add-watch date :get get-stuff)
        _ (add-watch cate :get get-stuff)
        ]
    (r/create-class
      {:reagent-render
       (fn []
         ;(get-stuff)
         [:div.container
          [:div.row
           [date-component date]
           [select-component cate 
            [["搜索视频" "video"]
             ["搜索视频素材" "scene"]
             ["搜索标准素材库" "stdscene"]]]
           [:div
            [:table.table.table-bordered.table-striped.table-condensed.table-responsive
             [:tbody
              [:tr
               [:th "数量"]
               [:th "搜索词"]]
              (for [e @stuff]
                [:tr
                 [:td (second e)]
                 [:td (first e)]])]]]]])
       :component-did-mount
       (fn [this]
         (get-stuff))})))

;;;

(def pages
  {
   ;:home #'home-page
   ;:about #'about-page
   :day #'day-page
   :mins #'mins-page
   :ratio #'ratio-page
   :user #'user-page
   :post #'post-page
   :mold #'mold-page
   :version-table #'version-table-page
   :daily-table #'daily-table-page
   :sample #'sample-page
   :rank #'rank-page
   :compare #'compare-page
   :search #'search-page
   :test #'test-page
   })

(defn page []
  [(pages (session/get :page))])

;; -------------------------
;; Routes
(secretary/set-config! :prefix "#")

;(secretary/defroute "/" [] (session/put! :page :home))
;(secretary/defroute "/about" [] (session/put! :page :about))
(secretary/defroute "/day" [] (session/put! :page :day))
(secretary/defroute "/mins" [] (session/put! :page :mins))
(secretary/defroute "/ratio" [] (session/put! :page :ratio))
(secretary/defroute "/user" [] (session/put! :page :user))
(secretary/defroute "/post" [] (session/put! :page :post))
(secretary/defroute "/mold" [] (session/put! :page :mold))
(secretary/defroute "/version-table" [] (session/put! :page :version-table))
(secretary/defroute "/daily-table" [] (session/put! :page :daily-table))
(secretary/defroute "/rank" [] (session/put! :page :rank))
(secretary/defroute "/sample" [] (session/put! :page :sample))
(secretary/defroute "/compare" [] (session/put! :page :compare))
(secretary/defroute "/search" [] (session/put! :page :search))
(secretary/defroute "/test" [] (session/put! :page :test))

;; -------------------------
;; History
;; must be called after routes have been defined
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
      EventType/NAVIGATE
      (fn [event]
        (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

;; -------------------------
;; Initialize app
(defn fetch-docs! []
  (GET (str js/context "/docs") {:handler #(session/put! :docs %)}))

(defn mount-components []
  (r/render [#'navbar] (.getElementById js/document "navbar"))
  (r/render [#'page] (.getElementById js/document "app")))

(defn init! []
  (fetch-docs!)
  (hook-browser-navigation!)
  (mount-components))
