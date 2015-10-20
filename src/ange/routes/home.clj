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

(defn save-message! [{:keys [params]}]
  (println params)
  (response {:status :success}))

(defn res-json [data]
  {:body (json/write-str data)
   :headers {"Content-Type" "application/json"}
   :status 200})

(defroutes home-routes
  (GET "/" [] (home-page))
  (GET "/docs" [] (ok (-> "docs/docs.md" io/resource slurp)))
  (GET "/rank" [date cate] (db/rank-query date cate))
  (GET "/search" [date cate] (db/search-query date cate))
  (GET "/day" [ca from to] (map #(db/day-query % from to) (s/split ca #",")))
  (GET "/access" [ca from to] (map #(db/day-access % from to) (s/split ca #",")))
  (GET "/mins" [ca] (map db/mins-query (s/split ca #",")))
  )
