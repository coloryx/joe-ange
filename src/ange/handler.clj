(ns ange.handler
  (:require [compojure.core :refer [defroutes routes wrap-routes]]
            [ange.layout :refer [error-page]]
            [ange.routes.home :refer [home-routes]]
            [ange.middleware :as middleware]
            [ange.db.core :as db]
            [compojure.route :as route]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.3rd-party.rotor :as rotor]
            [selmer.parser :as parser]
            [environ.core :refer [env]]

            [clj-time.core :as t]
            ))

(defn schedule [interval f]
  (future
    (while true
      (Thread/sleep interval)
      (f))))

(defn init
  "init will be called once when
   app is deployed as a servlet on
   an app server such as Tomcat
   put any initialization code here"
  []

  (timbre/merge-config!
    {:level     (if (env :dev) :trace :info)
     :appenders {:rotor (rotor/rotor-appender
                          {:path "ange.log"
                           :max-size (* 512 1024)
                           :backlog 10})}})

  ;(if (env :dev) (parser/cache-off!))
  (parser/cache-off!)
  ;(db/connect!)
  (timbre/info (str
                 "\n-=[ange started successfully"
                 (when (env :dev) " using the development profile")
                 "]=-"))
  (schedule (* 1000 60) #(db/update-ios-versions (t/today)))
  (schedule (* 1000 60) #(db/update-and-versions (t/today)))
  )

(defn destroy
  "destroy will be called when your application
   shuts down, put any clean up code here"
  []
  (timbre/info "ange is shutting down...")
  ;(db/disconnect!)
  (timbre/info "shutdown complete!"))

(def app-routes
  (routes
    (wrap-routes #'home-routes middleware/wrap-csrf)
    (route/not-found
      (:body
        (error-page {:status 404
                     :title "page not found"})))))

(def app 
  (middleware/wrap-base #'app-routes))
