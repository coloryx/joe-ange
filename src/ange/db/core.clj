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
            ))

;; Tries to get the Mongo URI from the environment variable
(defonce db (let [uri (:database-url env)
                  {:keys [db]} (mg/connect-via-uri uri)]
              db))

(defn create-user [user]
  (mc/insert db "users" user))

(defn update-user [id first-name last-name email]
  (mc/update db "users" {:_id id}
             {$set {:first_name first-name
                    :last_name last-name
                    :email email}}))

(defn get-user [id]
  (mc/find-one-as-map db "users" {:_id id}))

(defn get-hello []
  (let [res (mc/find-one-as-map db "stat_film_create" {})
        _ (println "resd=" (:sum-user-ios res))]
    (:sum-user-ios res)))

(defn ->int [s]
  (Integer/parseInt s))

(defn trand [os]
  (case os
    "iOS" "ios"
    os))

(defn save-b [b]     
  (if (zero? b) 1 b))

(defn pcs [a b]
  (->> (/ a (save-b b))
    (bigdec)
    (with-precision 4)
    (double)))

(defn format-date [t]
  (mapv ->int (rest (re-matches #"(\d{4})-(\d{2})-(\d{2})" t))))

(defn tran-version [version]
  (case version
    "all" nil
    nil nil
    (s/replace version "." "_")))

(defn qsa [coll field from to default]
  (mapv
    (fn [day]
      (let [res (mc/find-one-as-map db coll {:day day})]
        (if (and res (res field))
          (res field)
          default)))
    (take (inc (t/in-days (t/interval from to))) (tp/periodic-seq from (t/days 1)))))

(defn qs- [coll field from to]
  (qsa coll field from to "-"))

(defn qs [coll field from to]
  (qsa coll field from to 0))

(defn qs2 [coll field from to]
  (qsa coll field from to {}))

;;;;;;;

(defn rank-query [date cate]
  (println "rank-query," date cate)
  (let [[yf mf df] (format-date date)
        ;_ (println "ymd: " yf mf df)
        day (t/plus (t/date-time yf mf df 0 0 0) (t/hours -8) (t/days 1))
        ;_ (println "day: " day)
        ret (mc/find-one-as-map db (str "stat_daily_" cate) {:day day})
        ;_ (println "rank-query->" ret)
        ret2 (->> ret
               (remove #(#{:_id :day} (key %)))
               (sort #(> (val %1) (val %2))))
        ret3 (map (fn [e]
                    (let [vid (s/replace (str (first e)) ":" "")]
                      (conj e
                            (str "http://cdn-web-qn.colorv.cn/"
                                 (get-in (mc/find-one-as-map db "video_logo_path" {:video_id vid}) [:info :logo_path])))))
                  ret2)
        ;_ (println "rank-query2->" ret2)
        ;_ (println "rank-query3->" ret3)
        ]
    ret3))

(defn search-query [date cate]
  (println "search-query," date cate)
  (let [[yf mf df] (format-date date)
        day (t/plus (t/date-time yf mf df 0 0 0) (t/hours -8) (t/days 1))
        ret (mc/find-one-as-map db (str "stat_daily_rank_query_word_" cate) {:day day})
        ret (:result ret)
        ret (seq (subvec ret 0 100)) ;;response is a seq
        ;_ (println "ret=" ret)
        ]
    ret))

(defn day-query [ca from to]
  (println (format "day-query, ca=%s from=%s to=%s" ca from to))
  (let [[cate st os version] (s/split ca #"-")
        version (tran-version version)
        [yf mf df] (format-date from)
        [yt mt dt] (format-date to)
        from (t/plus (t/date-time yf mf df 0 0 0) (t/hours -8) (t/days 1))
        to (t/plus (t/date-time yt mt dt 0 0 0) (t/hours -8) (t/days 1))
        ke (if version
             (s/join "-" ["sum" st os version])
             (s/join "-" ["sum" st (trand os)]))
        kk (keyword ke)
        ;_ (println "kk=" kk)
        data (qs- (str "stat_daily_" cate) kk from to)
        ;_ (println "data=" data)
        ]
    {:name ca :data data}))

(defn day-access [ca from to]
  (println (format "day-access, ca=%s from=%s to=%s" ca from to))
  (let [[yf mf df] (format-date from)
        [yt mt dt] (format-date to)
        from (t/plus (t/date-time yf mf df 0 0 0) (t/hours -8) (t/days 1))
        to (t/plus (t/date-time yt mt dt 0 0 0) (t/hours -8) (t/days 1))
        data (qs- "stat_daily_access" (keyword (str "sum-" ca)) from to)
        _ (println "data=" data)
        ]
    {:name ca :data data}))

(defn mins-query [ca]
  (println (format "mins-query, ca=%s" ca))
  (let [[date cate os] (s/split ca #"-")
        [y1 m1 d1] (rest (re-matches #"(\d{4})(\d{2})(\d{2})" date))
        [y m d] (mapv ->int [y1 m1 d1])
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
