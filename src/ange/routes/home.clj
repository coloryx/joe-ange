(ns ange.routes.home
  (:require [ange.layout :as layout]
            [compojure.core :refer [defroutes GET POST]]
            [ring.util.response :refer [response status]]
            [ring.util.http-response :refer [ok]]
            [clojure.java.io :as io]
            [ange.db.core :as db]
            [clojure.string :as s]
            [clojure.data.json :as json]
            ))

(defn home-page []
  (layout/render "home.html"))

(defroutes home-routes ;; TODO lein run not auto rerun this code, must restart!?
  (GET "/" [] (home-page))
  (GET "/docs" [] (ok (-> "docs/docs.md" io/resource slurp)))
  (GET "/rank" [date cate] (db/rank-query date cate))
  (GET "/rank-delete" [vid] (db/rank-delete vid))
  (GET "/search" [date cate] (db/search-query date cate))
  (GET "/day" [ca from to] (map #(db/day-query % from to) (s/split ca #",")))
  (GET "/access" [ca from to] (map #(db/day-access % from to) (s/split ca #",")))
  (GET "/mins" [ca] (map db/mins-query (s/split ca #",")))
  (GET "/ratio" [ca from to] (map #(db/ratio-query % from to) (s/split ca #",")))
  (GET "/mold" [ca from to] (map #(db/mold-query % from to) (s/split ca #",")))
  (GET "/moldlist" [date cate] (db/mold-list-query date cate))
  (GET "/version-table" [date os] (db/table-query date os))
  (GET "/sample" [date os platform age_zone gender] (db/sample-query date os platform age_zone gender))
  (GET "/compare" [date] (db/compare-query date))
  (GET "/daily-table" [from to] (db/daily-table-query from to))
  (GET "/sync-ios-versions" [] (db/sync-ios-versions))
  (GET "/sync-and-versions" [] (db/sync-and-versions))
  )
