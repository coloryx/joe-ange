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
            )
  (:import goog.History))

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
          [nav-link "#/ext" "Ext" :ext collapsed?]
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

(defn date-component [title value f]
  [:input {:type "date"
           :value @value
           :on-change (fn [e] 
                        (reset! value (-> e .-target .-value))
                        (f))}])

(defn alone-select-component [value f arr]
  [:select
   {:on-change (fn [e]
                 (reset! value (-> e .-target .-value))
                 (f))}
   (for [[val text] arr]
     [:option {:value val} text])])

(defn linked-select-component [value arr]
  [:select
   {:on-change (fn [e]
                 (reset! value (-> e .-target .-value)))}
   (for [[text val] arr]
     [:option {:value val} text])])

(defn linked-group-select-component [value arr]
  [:select
   {:on-change (fn [e]
                 (reset! value (-> e .-target .-value)))}
   (for [[label a] arr]
     [:optgroup {:label label}
      (for [[text val] a]
        [:option {:value val} text])])])

(defn rank-page []
  (let [stuff (r/atom [])
        ;date (r/atom (f/unparse (f/formatters :year-month-day) (l/local-now))); TODO
        date (r/atom "2015-10-11")
        cate (r/atom "video")
        get-stuff (fn [] 
                    (GET "/rank" 
                         {:handler (fn [response] (reset! stuff response))
                          :params {:date @date :cate @cate}}))]
    (fn []
      [:div.container
       [:div.row

        [date-component "Date: " date get-stuff]

        [alone-select-component cate get-stuff [["rank200_video_station_create" "被照做的前200名"]
                                                ["rank200_video_fav" "被收藏的前200名"]
                                                ["rank200_video_play_onweb" "网页端播放的前200名"]]]

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
        date (r/atom "2015-10-11")
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

        [alone-select-component cate get-stuff [["video" "搜索视频"]
                                                ["scene" "搜索视频素材"]
                                                ["stdscene" "搜索标准素材库"]]]

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
   [linked-select-component a [["用户关注" "user_follow"]
                               ["用户取消关注" "user_unfollow"]
                               ["视频赞" "video_like"]
                               ["视频收藏" "video_fav"]
                               ["视频评论" "video_comment"]
                               ["api访问量" "all_access_api"]]]
   [linked-select-component b [["次数" "count"]
                               ["人数" "user"]
                               ["被操作数" "target"]]]
   [linked-select-component c [["all" "all"]
                               ["android" "and"]
                               ["iOS" "iOS"]]]
   [linked-group-select-component d [["Group1-all" [["all" "all"]]]
                                     ["Group2-iOS" [["all" "all"]
                                                    ["3.2.0" "3.2.0"]
                                                    ["3.1.9" "3.1.9"]]]
                                     ["Group3-android" [["all" "all"]
                                                        ["3.6.9" "3.6.9"]
                                                        ["3.6.8" "3.6.8"]]]]]
   [button-component "+" +]
   [button-component "--" --]
   ])

(defn day-selects2-component [a b c d + --]
  [:div
   [linked-group-select-component a [["Group1" [["视频播放" "video_play"]]]
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
                                                ["照做关注" "comply_create_place_friends"]]]]]
   [linked-select-component b [["次数" "count"]
                               ["人数" "user"]
                               ["被操作数" "target"]]]
   [linked-select-component c [["all" "all"]
                               ["android" "and"]
                               ["iOS" "iOS"]]]
   [linked-group-select-component d [["Group1-all" [["all" "all"]]]
                                     ["Group2-iOS" [["all" "all"]
                                                    ["3.2.0" "3.2.0"]
                                                    ["3.1.9" "3.1.9"]]]
                                     ["Group3-android" [["all" "all"]
                                                        ["3.6.9" "3.6.9"]
                                                        ["3.6.8" "3.6.8"]]]]]
   [button-component "+" +]
   [button-component "--" --]
   ])

(defn day-selects3-component [a b c d + --]
  [:div
   [linked-select-component a [["打开页面" "play"]
                               ["点击下载" "download"]
                               ["转化率(下载／打开)" "ratio"]]]
   [linked-select-component b [["all" "all"]
                               ["iOS" "ios"]
                               ["android" "android"]]]
   [linked-select-component c [["all" "all"]
                               ["微信" "mm"]
                               ["QQ" "qq"]]]
   [linked-select-component d [["all" "all"]
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
   [:div {:id id :style {:min-width "310px" :max-width "800px"
                         :height "400px" :margin "0 auto"}}]
   [graph-data config id]])

(defn chart-unity-component [selects-component getstring graph-id ia ib ic id]
  (let [stuff (r/atom [])

        from (r/atom "2015-10-09")
        to (r/atom "2015-10-14")
        a (r/atom ia)
        b (r/atom ib)
        c (r/atom ic)
        d (r/atom id)

        s (r/atom #{})

        config (r/atom
                 {:chart {:type "line"}
                  :title {:text "xx-yy-zz"
                          :x -20}
                  :subtitle {:text "aa-bb-cc"
                             :x -20}
                  :xAxis {:categories ["day1" "day2" "day3"]}
                  :yAxis {:title {:text "数量"}
                          :plotLines [{:value 0 :width 1 :color "#808080"}]}
                  :tooltip {:valueSuffix ""}
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
                                     (swap! config assoc :series 
                                            (vec (for [{name "name" data "data"} @stuff]
                                                   {:name name :data data}))))}))

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
      [chart-unity-component day-selects1-component "day" "graph1" "user_follow" "count" "all" "all"]
      [chart-unity-component day-selects2-component "day" "graph2" "video_play" "count" "all" "all"]
      [chart-unity-component day-selects3-component "access" "graph3" "play" "all" "all" "all"]
      ]]))

;ext
;(defonce chart-config 
;(r/atom
;{:chart {:type "line"}
;:title {:text "Monthly Average Temperature"
;:x -20}
;:subtitle {:text "Source: WorldClimate.com"
;:x -20}
;:xAxis {:categories ["Jan" "Feb" "Mar" "Apr" "May" "Jun"
;"Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]}
;:yAxis {:title {:text "Temperature (°C)"}
;:plotLines [{:value 0 :width 1 :color "#808080"}]}
;:tooltip {:valueSuffix "°C"}
;:legend {:layout "vertical"
;:align "right"
;:verticalAlign "middle"
;:borderWidth 0}
;:series [{:name "Tokyo"
;:data [7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6]}
;{:name "New York"
;:data [-0.2, 0.8, 5.7, 11.3, 17.0, 22.0, 24.8, 24.1, 20.1, 14.1, 8.6, 2.5]}
;{:name "Berlin"
;:data [-0.9, 0.6, 3.5, 8.4, 13.5, 17.0, 18.6, 17.9, 14.3, 9.0, 3.9, 1.0]}
;{:name "London"
;:data [5.9, 4.2, 5.7, 8.5, 11.9, 15.2, 17.0, 16.6, 14.2, 10.3, 6.6, 4.8]}]}))

;(def all (r/atom []))
;(def new (r/atom ""))

;(defn input []
;[:div
;[:input {:type "text"
;:value @new
;:on-change #(reset! new (-> % .-target .-value))}]
;[:input {:type "button"
;:value "add"
;:on-click #((swap! all conj (int @new))
;(reset! new "")
;(swap! chart-config assoc :series [{:name "what" :data @all}]))}]])

;(defn lister []
;[:ul
;(for [item @all]
;[:li item])])

;(defn graph-data []
;(js/$ (fn []
;(debug/println "called graph! gaocong")
;(.highcharts (js/$ "#example")
;(clj->js @chart-config))))
;[:p @chart-config])

;(defn graph []
;[:div [:h1 "The Graph..."]
;[:div#example {:style {:min-width "310px" :max-width "800px"
;:height "400px" :margin "0 auto"}}]
;[graph-data]])

;(defn ext-page []
;[:div
;[input]
;[lister]
;[graph]])

(defn ext-page []
  [:div "This is ext-page"])

;;;

(def pages
  {:home #'home-page
   :about #'about-page
   :rank #'rank-page
   :search #'search-page
   :day #'day-page
   :ext #'ext-page
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

(secretary/defroute "/ext" []
  (session/put! :page :ext))

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
