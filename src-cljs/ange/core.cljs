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
            [reagent-forms.core :as rf]

            )
  (:import goog.History))

(defn parse-date [date] (cf/parse (cf/formatter "yyyy-MM-dd") date))
(defn unparse-date [date] (cf/unparse (cf/formatter "yyyy-MM-dd") date))
(defn period-date [from to]
  (mapv unparse-date
        (take (inc (ct/in-days (ct/interval (parse-date from) (parse-date to))))
              (cp/periodic-seq (parse-date from) (ct/days 1)))))
(def today (unparse-date (ct/now)))
(def yesterday (unparse-date (ct/plus (ct/now) (ct/days -1))))
(def ylastweek (unparse-date (ct/plus (ct/now) (ct/days -8))))

(defn unparse-date2 [date] (cf/unparse (cf/formatter "HH:mm") date))
(defn parse-date2 [date] (cf/parse (cf/formatter "yyyy-MM-dd-HH:mm") date))
(defn period-date2 [from to]
  (mapv unparse-date2
        (take (/ (* 24 60) 10)
              (cp/periodic-seq (parse-date2 from) (ct/minutes 10)))))
(def period-10mins-oneday (period-date2 "2015-09-10-00:00" "2015-09-10-23:50"))

(defn date-component [title value callback]
  [:input {:type "date"
           :value @value
           :on-change (fn [e] 
                        (reset! value (-> e .-target .-value))
                        (callback))}])

(defn select-component
  ([value selects]
   [:select
    {:value @value
     :on-change (fn [e] (reset! value (-> e .-target .-value)))}
    (for [[text v] selects]
      [:option {:value v} text])])
  ([value selects group]
   [:select
    {:value @value
     :on-change (fn [e] (reset! value (-> e .-target .-value)))}
    (for [[label ss] selects]
      [:optgroup {:label label}
       (for [[text v] ss] 
         [:option {:value v} text])])]))

(defn select-callback-component
  ([value selects callback]
   [:select
    {:value @value
     :on-change (fn [e] (reset! value (-> e .-target .-value)) (callback))}
    (for [[text v] selects]
      [:option {:value v} text])])
  ([value selects callback group]
   [:select
    {:value @value
     :on-change (fn [e] (reset! value (-> e .-target .-value)) (callback))}
    (for [[label ss] selects]
      [:optgroup {:label label}
       (for [[text v] ss] 
         [:option {:value v} text])])]))

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
          [nav-link "#/" "Home" :home collapsed?]
          [nav-link "#/about" "About" :about collapsed?]
          [nav-link "#/rank" "Rank" :rank collapsed?]
          [nav-link "#/search" "Search" :search collapsed?]
          [nav-link "#/day" "Day" :day collapsed?]
          [nav-link "#/mins" "Mins" :mins collapsed?]
          [nav-link "#/test" "Test" :test collapsed?]
          ;[nav-link "#/todos" "Todos" :todos collapsed?]
          ]]]])))

(defn about-page []
  [:div.container
   [:div.row
    [:div.col-md-12
     "this is the story of ange... work in progress"]]])

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

(defn rank-page []
  (let [stuff (r/atom [])
        date (r/atom yesterday)
        cate (r/atom "rank200_video_station_create")
        get-stuff (fn [] 
                    (GET "/rank" 
                         {:handler (fn [response] (reset! stuff response))
                          :params {:date @date :cate @cate}}))]
    (fn []
      [:div.container
       [:div.row

        [date-component "Date: " date get-stuff]

        [select-callback-component 
         cate 
         [["被照做的前200名" "rank200_video_station_create"]
          ["被收藏的前200名" "rank200_video_fav"]
          ["网页端播放的前200名" "rank200_video_play_onweb"]]
         get-stuff]

        [:div
         [:table#rank-table.table.table-bordered.table-striped.table-condensed.table-responsive
          [:tr
           [:th "数量"]
           [:th "链接"]]
          (doall ;; must!!
            (for [i (range 200)]
              (let [e (get @stuff i)]
                [:tr
                 [:td (second e)]
                 [:td [:a {:href (str "http://video.colorv.cn/play/" (str (first e)))}
                       [:img {:src (last e) :height 100 :width 200}]]]])))]]
        ]])))

(defn search-page []
  (let [stuff (r/atom nil)
        date (r/atom yesterday)
        cate (r/atom "scene")
        get-stuff (fn []
                    (GET "/search"
                         {:handler (fn [response] 
                                     (reset! stuff response)
                                     (.log js/console "search-stuff=" (first @stuff)))
                          :params {:date @date :cate @cate}}
                         ))]
    (fn []
      [:div.container
       [:div.row

        [date-component "Date: " date get-stuff]

        [select-callback-component 
         cate 
         [["搜索视频" "video"]
          ["搜索视频素材" "scene"]
          ["搜索标准素材库" "stdscene"]] 
         get-stuff]

        [:div
         [:table#rank-table.table.table-bordered.table-striped.table-condensed.table-responsive
          [:tr
           [:th "数量"]
           [:th "搜索词"]] 
          (doall
            (for [i (range 100)]
              (let [e (get @stuff i)]
                [:tr
                 [:td (second e)]
                 [:td (first e)]])))]]
        ]])))

;; day-page
(defn button-component [text f]
  [:button {:on-click (fn [e] (f))} text])

(defn day-selects1-component [a b c d + --]
  [:div
   [select-component 
    a 
    [["用户关注" "user_follow"]
     ["用户取消关注" "user_unfollow"]
     ["视频赞" "video_like"]
     ["视频收藏" "video_fav"]
     ["视频评论" "video_comment"]
     ["api访问量" "all_access_api"]]]
   [select-component 
    b
    [["次数" "count"]
     ["人数" "user"]
     ["被操作数" "target"]]]
   [select-component 
    c 
    [["all" "all"]
     ["android" "and"]
     ["iOS" "iOS"]]]
   [select-component 
    d 
    [["Group1-all" [["all" "all"]]]
     ["Group2-iOS" [["all" "all"]
                    ["3.2.0" "3.2.0"]
                    ["3.1.9" "3.1.9"]]]
     ["Group3-android" [["all" "all"]
                        ["3.6.9" "3.6.9"]
                        ["3.6.8" "3.6.8"]]]]
    "group"]
   [button-component "+" +]
   [button-component "--" --]
   ])

(defn day-selects2-component [a b c d + --]
  [:div
   [select-component 
    a 
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
                ["照做关注" "comply_create_place_friends"]]]]
    "group"]
   [select-component
    b 
    [["次数" "count"]
     ["人数" "user"]
     ["被操作数" "target"]]]
   [select-component
    c 
    [["all" "all"]
     ["android" "and"]
     ["iOS" "iOS"]]]
   [select-component 
    d 
    [["Group1-all" [["all" "all"]]]
     ["Group2-iOS" [["all" "all"]
                    ["3.2.0" "3.2.0"]
                    ["3.1.9" "3.1.9"]]]
     ["Group3-android" [["all" "all"]
                        ["3.6.9" "3.6.9"]
                        ["3.6.8" "3.6.8"]]]]
    "group"]
   [button-component "+" +]
   [button-component "--" --]
   ])

(defn day-selects3-component [a b c d + --]
  [:div
   [select-component
    a 
    [["打开页面" "play"]
     ["点击下载" "download"]
     ["转化率(下载／打开)" "ratio"]]]
   [select-component 
    b 
    [["all" "all"]
     ["iOS" "ios"]
     ["android" "android"]]]
   [select-component 
    c
    [["all" "all"]
     ["微信" "mm"]
     ["QQ" "qq"]]]
   [select-component 
    d 
    [["all" "all"]
     ["短片" "video"]
     ["影集" "album"]
     ["大片" "film"]]]
   [button-component "+" +]
   [button-component "--" --]
   ])

(defn graph-data [config id]
  (js/$ (fn []
          (.highcharts (js/$ (str "#" id))
                       (clj->js @config))))
  [:p @config])

(defn graph [config id]
  [:div
   [:div {:id id 
          :style {:min-width "310px" :max-width "2000px"
                  :height "400px" :margin "0 auto"}}]
   [graph-data config id]])

(defn chart-unity-component [selects-component getstring graph-id graph-title ia ib ic id]
  (let [stuff (r/atom [])

        from (r/atom ylastweek)
        to (r/atom yesterday)
        a (r/atom ia)
        b (r/atom ib)
        c (r/atom ic)
        d (r/atom id)

        s (r/atom #{})

        config (r/atom
                 {:chart {:type "line"}
                  :title {:text graph-title
                          :x -20}
                  :subtitle {:text ""
                             :x -20}
                  :xAxis {:categories []}
                  :yAxis {:title {:text "数量"}
                          :plotLines [{:value 0 :width 1 :color "#808080"}]}
                  :tooltip {:valueSuffix "个"}
                  :legend {:layout "vertical"
                           :align "right"
                           :verticalAlign "middle"
                           :borderWidth 0}
                  :series [{:name "sample"
                            :data [1 2 3]}]})

        get-stuff (fn []
                    (GET (str "/" getstring)
                         {:params {:from @from :to @to :ca (s/join "," @s)}
                          :handler (fn [response]
                                     (reset! stuff response)
                                     (debug/dbg @stuff)
                                     (swap! config assoc :series (vec (for [{name "name" data "data"} @stuff]
                                                                        {:name name :data data})))
                                     (swap! config assoc-in [:xAxis :categories] (period-date @from @to))
                                     )}))

        add (fn []
              (let [ca (s/join "-" [@a @b @c @d])]
                (when-not (@s ca)
                  (debug/dbg @s)
                  (swap! s conj ca)
                  (get-stuff))))

        clear (fn []
                (reset! s #{})
                (get-stuff))
        ]
    (fn []
      [:div ;;must
       [date-component "Date: " from get-stuff]
       [date-component "Date: " to get-stuff]
       [selects-component a b c d add clear]
       [graph config graph-id]])))

(defn day-page []
  (fn []
    [:div.container
     [:div.row
      [chart-unity-component day-selects1-component "day" "day-graph1" "社区活跃度" "user_follow" "count" "all" "all"]
      [chart-unity-component day-selects2-component "day" "day-graph2" "视频播放分享创建量" "video_play" "count" "all" "all"]
      [chart-unity-component day-selects3-component "access" "day-graph3" "渠道" "play" "all" "all" "all"]
      ]]))

;; mins

(defn mins-selects-component [date bi os date-callback bi-callback os-callback]
  [:div
   [date-component "" date date-callback]
   [select-callback-component 
    bi 
    [["Group1" [["短片分享" "video_share"]
                ["短片赞" "video_like"]
                ["短片收藏" "video_fav"]
                ["短片评论" "video_comment"]
                ["短片播放" "video_play"]
                ["短片创建" "video_batch_create"]
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
                ["短片关注播放" "video_play_place_friends"]]]]
    bi-callback "group"]
   [select-callback-component
    os 
    [["all" "all"]
     ["android" "and"]
     ["iOS" "iOS"]]
    os-callback]
   ])

(def mins-radio-component-head
  [:div
   [:input {:field :radio :value today :name :foo} "今天"]
   [:input {:field :radio :value yesterday :name :foo} "昨天"]])

(defn s1 [date]
  (unparse-date (ct/plus (parse-date date) (ct/days -1))))

(defn s7 [date]
  (unparse-date (ct/plus (parse-date date) (ct/days -7))))

(def mins-radio-component-bottom
  [:div {:style {:text-align "center"}}
   [:label "对比："]
   [:input {:field :radio :value "s1" :name :baz} "前一日"]
   [:input {:field :radio :value "s7" :name :baz} "上周同期"]])

(defn mins-page []
  (let [stuff (r/atom nil)

        radio (r/atom {:foo today})
        radio2 (r/atom {:baz "s1"})

        ;date1 (r/atom today)
        date1 (r/cursor radio [:foo])
        date2 (r/atom yesterday)
        ;date2 (r/cursor radio2 [:baz])

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

        get-stuff (fn []
                    (debug/dbg (str "radio=" @radio))
                    (debug/dbg (str "date1=" @date1)) ;; TODO date1 not sync with radio!
                    (GET "/mins"
                         {:params {:ca (s/join "," [(s/join "-" [(s/replace (:foo @radio) "-" "") @bi1 @os1])
                                                    (s/join "-" [(s/replace @date2 "-" "") @bi2 @os2])])}
                          :handler (fn [response]
                                     (reset! stuff response)
                                     ;(debug/dbg @stuff)
                                     (swap! config assoc :series (vec (for [{name "name" data "data"} @stuff]
                                                                        {:name name :data data})))
                                     )}))

        _ (add-watch radio :get get-stuff)
        _ (add-watch date2 :get get-stuff)
        _ (add-watch radio2 :get (fn []
                                   (case (:baz @radio2)
                                     "s1" (reset! date2 (s1 @date1))
                                     "s7" (reset! date2 (s7 @date1)))))
        ]
    (fn []
      [:div.container
       [:div.row
        [rf/bind-fields mins-radio-component-head radio]
        [:label (str @radio)]

        [graph config "mins-graph"]

        [rf/bind-fields mins-radio-component-bottom radio2]
        [:label (str @radio2)]
        
        [mins-selects-component date1 bi1 os1 
         get-stuff
         (fn [] (reset! bi2 @bi1) (get-stuff))
         (fn [] (reset! os2 @os1) (get-stuff))]

        [mins-selects-component date2 bi2 os2 get-stuff get-stuff get-stuff]

        ]])))

;; test

;(defn atom-input [value]
  ;[:input {:type "text"
           ;:value @value
           ;:on-change #(reset! value (-> % .-target .-value))}])

;(defn test-page []
  ;(let [val (r/atom "2015-10-11")]
    ;(fn []
      ;[:div
       ;[atom-input val]
       ;[atom-input val]
       ;[date-component "" val inc]
       ;[select-component val [["display1" "2015-10-11"]
                              ;["display2" "2015-10-12"]]]
       ;[select-component val [["group1" [["display1" "2015-10-11"]
                                         ;["display2" "2015-10-12"]]]
                              ;["group2" [["display3" "2015-10-12"]
                                         ;["display4" "2015-10-14"]]]] "group"]
       ;])))

;(defn radio [label name value]
  ;[:div.radio
   ;[:label
    ;[:input {:field :radio :name name :value value}]
    ;label]])

;(defn test-page []
  ;(let [doc (atom {})]
    ;(fn []
      ;[:h3 "single-select buttons"]
      ;[:div.btn-group {:field :single-select :id :unique.position}
       ;[:button.btn.btn-default {:key :left} "Left"]
       ;[:button.btn.btn-default {:key :middle} "Middle"]
       ;[:button.btn.btn-default {:key :right} "Right"]]

      ;[:h3 "single-select list"]
      ;[:div.list-group {:field :single-select :id :pick-one}
       ;[:div.list-group-item {:key :foo} "foo"]
       ;[:div.list-group-item {:key :bar} "bar"]
       ;[:div.list-group-item {:key :baz} "baz"]]
      ;)))

(def form
  [:div
   [:input {:field :radio :value :a :name :foo :id :radioselection} "foo"]
   [:input {:field :radio :value :b :name :foo :id :radioselection} "bar"]
   [:input {:field :radio :value :c :name :foo :id :radioselection} "baz"]])

(defn test-page []
  (let [doc (r/atom {:radioselection :b})]
    (fn []
      [:div
       [rf/bind-fields form doc]
       [:label (str @doc)]])))

;; todos

;(defonce todos (r/atom (sorted-map)))

;(defonce counter (r/atom 0))

;(defn add-todo [text]
  ;(let [id (swap! counter inc)]
    ;(swap! todos assoc id {:id id :title text :done false})))

;(defn toggle [id] (swap! todos update-in [id :done] not))
;(defn save [id title] (swap! todos assoc-in [id :title] title))
;(defn delete [id] (swap! todos dissoc id))

;(defn mmap [m f a] (->> m (f a) (into (empty m))))
;(defn complete-all [v] (swap! todos mmap map #(assoc-in % [1 :done] v)))
;(defn clear-done [] (swap! todos mmap remove #(get-in % [1 :done])))

;(defonce init (do
                ;(add-todo "Rename Cloact to Reagent")
                ;(add-todo "Add undo demo")
                ;(add-todo "Make all rendering async")
                ;(add-todo "Allow any arguments to component functions")
                ;(complete-all true)))

;(defn todo-input [{:keys [title on-save on-stop]}]
  ;(let [val (r/atom title)
        ;stop #(do (reset! val "")
                  ;(if on-stop (on-stop)))
        ;save #(let [v (-> @val str clojure.string/trim)]
                ;(if-not (empty? v) (on-save v))
                ;(stop))]
    ;(fn [props]
      ;[:input (merge props
                     ;{:type "text" :value @val :on-blur save
                      ;:on-change #(reset! val (-> % .-target .-value))
                      ;:on-key-down #(case (.-which %)
                                      ;13 (save)
                                      ;27 (stop)
                                      ;nil)})])))

;(def todo-edit (with-meta todo-input
                 ;{:component-did-mount #(.focus (r/dom-node %))}))

;(defn todo-stats [{:keys [filt active done]}]
  ;(let [props-for (fn [name]
                    ;{:class (if (= name @filt) "selected")
                     ;:on-click #(reset! filt name)})]
    ;[:div
     ;[:span#todo-count
      ;[:strong active] " " (case active 1 "item" "items") " left"]
     ;[:ul#filters
      ;[:li [:a (props-for :all) "All"]]
      ;[:li [:a (props-for :active) "Active"]]
      ;[:li [:a (props-for :done) "Completed"]]]
     ;(when (pos? done)
       ;[:button#clear-completed {:on-click clear-done}
        ;"Clear completed " done])]))

;(defn todo-item []
  ;(let [editing (r/atom false)]
    ;(fn [{:keys [id done title]}]
      ;[:li {:class (str (if done "completed ")
                        ;(if @editing "editing"))}
       ;[:div.view
        ;[:input.toggle {:type "checkbox" :checked done
                        ;:on-change #(toggle id)}]
        ;[:label {:on-double-click #(reset! editing true)} title]
        ;[:button.destroy {:on-click #(delete id)}]]
       ;(when @editing
         ;[todo-edit {:class "edit" :title title
                     ;:on-save #(save id %)
                     ;:on-stop #(reset! editing false)}])])))

;(defn todos-page [props]
  ;(let [filt (r/atom :all)]
    ;(fn []
      ;(let [items (vals @todos)
            ;done (->> items (filter :done) count)
            ;active (- (count items) done)]
        ;[:div
         ;[:section#todoapp
          ;[:header#header
           ;[:h1 "todos"]
           ;[todo-input {:id "new-todo"
                        ;:placeholder "What needs to be done?"
                        ;:on-save add-todo}]]
          ;(when (-> items count pos?)
            ;[:div
             ;[:section#main
              ;[:input#toggle-all {:type "checkbox" :checked (zero? active)
                                  ;:on-change #(complete-all (pos? active))}]
              ;[:label {:for "toggle-all"} "Mark all as complete"]
              ;[:ul#todo-list
               ;(for [todo (filter (case @filt
                                    ;:active (complement :done)
                                    ;:done :done
                                    ;:all identity) items)]
                 ;^{:key (:id todo)} [todo-item todo])]]
             ;[:footer#footer
              ;[todo-stats {:active active :done done :filt filt}]]])]
         ;[:footer#info
          ;[:p "Double-click to edit a todo"]]]))))

;;;

(def pages
  {:home #'home-page
   :about #'about-page
   :rank #'rank-page
   :search #'search-page
   :day #'day-page
   :mins #'mins-page
   :test #'test-page
   ;:todos #'todos-page
   })

(defn page []
  [(pages (session/get :page))])

;; -------------------------
;; Routes
(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (session/put! :page :home))

(secretary/defroute "/about" []
  (session/put! :page :about))

(secretary/defroute "/rank" []
  (session/put! :page :rank))

(secretary/defroute "/search" []
  (session/put! :page :search))

(secretary/defroute "/day" []
  (session/put! :page :day))

(secretary/defroute "/mins" []
  (session/put! :page :mins))

(secretary/defroute "/test" []
  (session/put! :page :test))

;(secretary/defroute "/todos" []
  ;(session/put! :page :todos))

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
