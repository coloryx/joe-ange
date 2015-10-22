(ns ange.db.core
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.query :as mq]
            [monger.joda-time] ;;;must
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-time.format :as tf]
            [clj-time.periodic :as tp]
            [clojure.string :as s]
            [taoensso.timbre :as log]
     ))

;; Tries to get the Mongo URI from the environment variable
(defonce db (let [uri (:database-url env)
                  {:keys [db]} (mg/connect-via-uri uri)]
              db))

;(defn create-user [user]
  ;(mc/insert db "users" user))

;(defn update-user [id first-name last-name email]
  ;(mc/update db "users" {:_id id}
             ;{$set {:first_name first-name
                    ;:last_name last-name
                    ;:email email}}))

;(defn get-user [id]
  ;(mc/find-one-as-map db "users" {:_id id}))

;(defn get-hello []
  ;(let [res (mc/find-one-as-map db "stat_film_create" {})
        ;_ (println "resd=" (:sum-user-ios res))]
    ;(:sum-user-ios res)))

(defn to-int [s]
  (Integer/parseInt s))

(defn trand [os] 
  (case os
    "iOS" "ios"
    os))

(defn pcs [a b]
  (let [save-b #(if (zero? %) 1 %)]
    (->> (/ a (save-b b))
      (bigdec)
      (with-precision 4)
      (double))))

(defn parse-date [date] ;; "2015-10-15" => Date(2015-10-15)  +08timezone
  (let [format-date #(mapv to-int (rest (re-matches #"(\d{4})-(\d{2})-(\d{2})" %)))]
    (t/plus (apply t/date-time (format-date date))
            (t/hours -8)
            (t/days 1))))

(defn tran-version [version]
  (case version
    "all" nil
    nil nil
    (s/replace version "." "_")))

(defn date-array [from to]
  (take (inc (t/in-days (t/interval from to))) 
        (tp/periodic-seq from (t/days 1))))

(defn qsa [coll field from to default]
  (mapv
    (fn [date]
      (let [res (mc/find-one-as-map db coll {:day date})]
        (if (and res (res field))
          (res field)
          default)))
    (date-array from to)))

(defn qs- [coll field from to]
  (qsa coll field from to "-"))

(defn qs [coll field from to]
  (qsa coll field from to 0))

(defn qs2 [coll field from to]
  (qsa coll field from to {}))

;;;;;;;
(defn day-query [ca from to]
  (println (format "day-query, ca=%s from=%s to=%s" ca from to))
  (let [[cate st os version] (s/split ca #"-")
        version (tran-version version)
        [from to] (mapv parse-date [from to])
        ke (if version
             (s/join "-" ["sum" st os version])
             (s/join "-" ["sum" st (trand os)]))
        data (qs- (str "stat_daily_" cate) (keyword ke) from to)
        ]
    {:name ca :data data}))

(defn rank-query [date cate]
  (println "rank-query," date cate)
  (let [date (parse-date date)
        ret (mc/find-one-as-map db (str "stat_daily_" cate) {:day date})
        _ (log/info "rank-query, ret=completed" (first ret))
        ret2 (->> ret
               (remove #(#{:_id :day} (key %)))
               (sort #(> (val %1) (val %2))))
        _ (log/info "rank-query, ret2=completed" (first ret2))
        ret3 (map (fn [e]
                     (let [vid (s/replace (str (first e)) ":" "")]
                       (conj e
                             (str "http://cdn-web-qn.colorv.cn/"
                                  (get-in (mc/find-one-as-map db "video_logo_path" {:video_id vid}) [:info :logo_path])))))
                   ret2)
        _ (log/info "rank-query, ret3=completed" (first ret3))
        ]
    ret3))

(defn search-query [date cate]
  (println "search-query," date cate)
  (let [date (parse-date date)
        ret (mc/find-one-as-map db (str "stat_daily_rank_query_word_" cate) {:day date})
        ret (:result ret)
        ret (seq (subvec ret 0 100)) ;;response is a seq
        ]
    ret))

(defn day-access [ca from to]
  (println (format "day-access, ca=%s from=%s to=%s" ca from to))
  (let [[from to] (mapv parse-date [from to])
        data (qs- "stat_daily_access" (keyword (str "sum-" ca)) from to)
        ]
    {:name ca :data data}))

(defn mins-query [ca]
  (println (format "mins-query, ca=%s" ca))
  (let [[date cate os] (s/split ca #"-")
        [y1 m1 d1] (rest (re-matches #"(\d{4})(\d{2})(\d{2})" date))
        [y m d] (mapv to-int [y1 m1 d1])
        cate (str "stat_" cate)
        tran (fn [cate]
               (case cate
                 "iOS" :sum-ios
                 "and" :sum-and
                 "all" :sum))
        begin (t/plus (t/date-time y m d 0 0 0) (t/hours -8)) ;08:00date
        end (t/plus begin (t/days 1))
        data (mq/with-collection db cate
               (mq/find {:from {$gte begin $lt end}})
               (mq/sort (array-map :from 1)))
        data (mapv (tran os) data)]
    {:name ca :data data}))

(defn ratio-query [ca from to]
  (println (format "ratio-query, ca=%s, from=%s, to=%s" ca from to))
  (let [[cate os version] (s/split ca #"-")
        version (tran-version version)
        [from to] (mapv parse-date [from to])
        ke (if version
             (s/join "-" ["sum" "count" os version])
             (s/join "-" ["sum" "count" (trand os)]))
        k (keyword ke)
        ke2 (if version
              (s/join "-" ["sum" "user" os version])
              (s/join "-" ["sum" "user" (trand os)]))
        k2 (keyword ke2)
        ]
    (case cate
      "newbie_video" (let [data1 (qs "stat_daily_login_registered" k from to)
                           data2 (qs "stat_daily_video_create" k from to)
                           data3 (qs "stat_daily_film_create" k from to)
                           data4 (map + data2 data3)
                           d (mapv pcs data1 data4)
                           ]
                       {:name ca :data d})
      "newbie_active" (let [d1 (qs "stat_daily_active_newuser" k2 from to)
                            d2 (qs "stat_daily_active_user" k2 from to)
                            d (mapv pcs d1 d2)]
                        {:name ca :data d})
      "newbie_active_video_free_create" (let [d1 (qs "stat_daily_active_newuser_video_free_create" k2 from to)
                                              d2 (qs "stat_daily_video_free" k from to)
                                              d (mapv pcs d1 d2)]
                                          {:name ca :data d})
      "newbie_active_video_sample_create" (let [d1 (qs "stat_daily_active_newuser_video_sample_create" k2 from to)
                                              d2 (qs "stat_daily_video_sample" k from to)
                                              d (mapv pcs d1 d2)]
                                          {:name ca :data d})
      "newbie_active_video_station_create" (let [d1 (qs "stat_daily_active_newuser_video_station_create" k2 from to)
                                              d2 (qs "stat_daily_video_station" k from to)
                                              d (mapv pcs d1 d2)]
                                          {:name ca :data d})
      "newbie_active_film_create" (let [d1 (qs "stat_daily_active_newuser_film_create" k2 from to)
                                              d2 (qs "stat_daily_film_create" k from to)
                                              d (mapv pcs d1 d2)]
                                          {:name ca :data d})
      "newbie_active_album_create" (let [d1 (qs "stat_daily_active_newuser_album_create" k2 from to)
                                              d2 (qs "stat_daily_album_create" k from to)
                                              d (mapv pcs d1 d2)]
                                          {:name ca :data d})
      "per_video" (let [d1 (qs "stat_daily_video_create" k from to)
                        d2 (qs "stat_daily_active_user" k2 from to)
                        d (mapv pcs d1 d2)]
                    {:name ca :data d})
      "per_film" (let [data1 (qs "stat_daily_film_create" k from to)
                       data2 (qs "stat_daily_active_user" k2 from to)
                       d (mapv pcs data1 data2)]
                   {:name ca :data d})
      "per_album" (let [data1 (qs "stat_daily_album_create" k from to)
                        data2 (qs "stat_daily_active_user" k2 from to)
                        d (mapv pcs data1 data2)]
                    {:name ca :data d})
      "ratio_video_share" (let [data1 (qs "stat_daily_video_share_2" k from to)
                                data2 (qs "stat_daily_video_create"  k from to)
                                d (mapv pcs data1 data2)]
                            {:name ca :data d})
      "ratio_album_share" (let [data1 (qs "stat_daily_album_share_2" k from to)
                                data2 (qs "stat_daily_album_create"  k from to)
                                d (mapv pcs data1 data2)]
                            {:name ca :data d})
      "ratio_sample" (let [data1 (qs "stat_daily_samples" k from to)
                           data2 (qs "stat_daily_video_create" k from to)
                           data3 (qs "stat_daily_film_create" k from to)
                           data4 (map + data2 data3)
                           d (mapv pcs data1 data4)]
                       {:name ca :data d})
      "ratio_follow" (let [data1 (qs "stat_daily_user_follow" k from to)
                           data2 (qs "stat_daily_active_user" k2 from to)
                           d (mapv pcs data1 data2)]
                       {:name ca :data d})
      "ratio_like" (let [data1 (qs "stat_daily_video_like" k from to)
                         data2 (qs "stat_daily_active_user" k2 from to)
                         d (mapv pcs data1 data2)]
                     {:name ca :data d})
      {})))

(defn mold-query [ca from to]
  (println (format "querymold, ca=%s from=%s to=%s" ca from to))
  (let [match (keyword ca)
        [from to] (mapv parse-date [from to])
        data (qs2 "stat_daily_get_mold" match from to)
        data (mapv count data)]
    {:name (name match) :data data}))

(defn mold-list-query [date cate]
  (println (format "mold-list-query date=%s cate=%s" date cate))
  (let [cate (keyword cate)
        date (parse-date date)
        res (mc/find-one-as-map db "stat_daily_get_mold" {:day date})]
    (seq (cate res))))

(defn table-query [date]
  (println (format "table-query date=%s" date))
  (let  [date (parse-date date)
         active-user-ret (mc/find-one-as-map db "stat_daily_active_user" {:day date}) ;;把数据先缓存下来，后面慢慢取
         video-free-ret (mc/find-one-as-map db "stat_daily_video_free" {:day date})
         video-sample-ret (mc/find-one-as-map db "stat_daily_video_sample" {:day date})
         video-station-ret (mc/find-one-as-map db "stat_daily_video_station" {:day date})
         film-create-ret (mc/find-one-as-map db "stat_daily_film_create" {:day date})
         album-create-ret (mc/find-one-as-map db "stat_daily_album_create" {:day date})
         gf (fn [ret v pre] (get
                              ret
                              (keyword (str pre (s/replace v "." "_")))
                              0))
         cf (fn [v]
              (let [active-user-count (gf active-user-ret v "sum-user-and-")
                    video-free-count (gf video-free-ret v "sum-count-and-")
                    video-sample-count (gf video-sample-ret v "sum-count-and-")
                    video-station-count (gf video-station-ret v "sum-count-and-")
                    video-count (+ video-free-count video-sample-count video-station-count)
                    video-ratio (pcs video-count active-user-count)
                    film-count (gf film-create-ret v "sum-count-and-")
                    film-ratio (pcs film-count active-user-count)
                    album-count (gf album-create-ret v "sum-count-and-")
                    album-ratio (pcs album-count active-user-count)
                    create-count (+ video-count film-count album-count)
                    create-ratio (pcs create-count active-user-count)]
                [v
                 active-user-count
                 video-free-count
                 video-sample-count
                 video-station-count
                 video-count
                 video-ratio
                 film-count
                 film-ratio
                 album-count
                 album-ratio
                 create-count
                 create-ratio
                 ]))
         ;_ (println "vs=" @and-versions)
         ;res (map cf @and-versions)
         res (map cf ["3.6.7" "3.6.6"]) ;; TODO
         ]
    res))

(defn sample-query [date os platform age_zone gender]
  (println (format "sssss sample-query, date=%s, os=%s, platform=%s, age_zone=%s gender=%s"
                   date os platform age_zone gender))
  (let [date (parse-date date)
        af (fn [e c d] (if (= "all" c)
                         true
                         (= c (get-in e [:user_info d]))))
        bf (fn [e c d] (if (= "all" c)
                         true
                         (= (to-int c) (get-in e [:user_info d]))))
        pf (fn [v place] (->> (filter #(= place (get-in % [:user_info :place])) v)
                           (map :count)
                           (apply +)))
        qf (fn [v] (->> v (map :count) (apply +)))
        res (filter
              (fn [e] (and (af e os :os)
                           (af e platform :platform)
                           (bf e age_zone :age_zone)
                           (af e gender :gender)))
              (:detail (mc/find-one-as-map db "stat_daily_active_user_detail" {:day date})))
        ret (->> (:detail (mc/find-one-as-map db "stat_daily_sample_detail" {:day date}))
              (filter (fn [e] (and (af e os :os)
                                   (af e platform :platform)
                                   (bf e age_zone :age_zone)
                                   (af e gender :gender))))
              (group-by #(get-in % [:user_info :version]))
              (map (fn [[k v]]
                     (let [active-user (->> res
                                         (filter (fn [x] (= k (get-in x [:user_info :version]))))
                                         (map (comp (fnil long 0) :count))
                                         (apply +))
                           sample-all (qf v)]
                       [k
                        active-user
                        (str sample-all " (" (pcs sample-all active-user) ")")
                        (pf v "digest")
                        (pf v "timeline")
                        (pf v "post")
                        (pf v "friends")
                        (pf v "user_detail")
                        (pf v "recommend")
                        ]))))
        ]
    ret))

(defn compare-query [date]
  (println (format "compare-query date=%s" date))
  (let [date (parse-date date)
        ret (mc/find-one-as-map db "stat_daily_loss_and_stay_newuser_action" {:day date})
        u (fn [a]
            (fn [b]
              (fn [x]
                (get-in x [a b] 0))))
        f1 (fn [f]
             ((juxt (f :user_count) (f :play_count) (f :video_create)
                    (f :video_create_free) (f :video_create_sample) (f :video_create_station)
                    (f :album_create) (f :film_create) (f :like) (f :fav))
                ret))
        [li1 & lis] (f1 (u :loss_ios))
        [si1 & sis] (f1 (u :stay_ios))
        [la1 & las] (f1 (u :loss_android))
        [sa1 & sas] (f1 (u :stay_android))
        vc (comp vec concat)
        pf (fn [a b] (str a " (" (pcs a b) ")"))
        ]
    (list (vc ["loss_ios" (pf li1 (+ li1 si1))] (map #(pf % li1) lis))
          (vc ["stay_ios" (pf si1 (+ li1 si1))] (map #(pf % si1) sis))
          (vc ["loss_android" (pf la1 (+ la1 sa1))] (map #(pf % la1) las))
          (vc ["stay_android" (pf sa1 (+ la1 sa1))] (map #(pf % sa1) sas)))))

(defn get-field [db coll select field]
  ((keyword field) (mc/find-one-as-map db coll select [field])))

(defn get-data [os date]
  (let [field (str "sum-count-" os)
        new-user (get-field db "stat_daily_login_registered" {:day date} (str "sum-user-" os))
        all-video-share (+ (get-field db "stat_daily_video_share" {:day date} "sum-count-all")
                           (get-field db "stat_daily_film_share" {:day date} "sum-count-all")
                           (get-field db "stat_daily_album_share_2" {:day date} "sum-count-all"))
        active-user (get-field db "stat_daily_active_user" {:day date} "sum-user-all")
        video-create (get-field db "stat_daily_video_create" {:day date} field)
        film-create (get-field db "stat_daily_film_create" {:day date} field)
        album-create (get-field db "stat_daily_album_create" {:day date} field)
        video-share (get-field db "stat_daily_video_share_2" {:day date} field)
        film-share (get-field db "stat_daily_film_share" {:day date} field)
        album-share (get-field db "stat_daily_album_share_2" {:day date} field)
        sample (get-field db "stat_daily_samples" {:day date} field) ;;照做
        follow (get-field db "stat_daily_user_follow" {:day date} field) ;;关注
        fav (get-field db "stat_daily_video_fav" {:day date} field) ;;收藏
        like (get-field db "stat_daily_video_like" {:day date} field) ;;赞
        ]
    [(tf/unparse (tf/formatter "yyyy-MM-dd") date)
     new-user
     (pcs new-user all-video-share)
     active-user
     video-create
     film-create
     album-create
     (pcs video-create active-user)
     (pcs film-create active-user)
     (pcs album-create active-user)
     video-share
     film-share
     album-share
     (pcs video-share video-create)
     (pcs film-share film-create)
     (pcs album-share album-create)
     sample
     (pcs (+ video-create film-create) sample)
     follow
     (pcs follow active-user)
     fav
     like
     (pcs like active-user)
     (get-field db "stat_daily_post_follow" {:day date} field)
     (get-field db "stat_daily_post_create" {:day date} field)
     (get-field db "stat_daily_post_upload_statuses" {:day date} field)
     (get-field db "stat_daily_post_video_create" {:day date} field)
     (get-field db "stat_daily_post_film_create" {:day date} field)
     ]
  ))

(defn daily-table-query [from to]
  (log/info (format "daily-table-query from=%s to=%s" from to))
  (let [[from to] (map parse-date [from to])
        date-arr (date-array from to)]
    (list (mapv (partial get-data "ios") date-arr) 
          (mapv (partial get-data "and") date-arr)
          (mapv (partial get-data "all") date-arr))))
