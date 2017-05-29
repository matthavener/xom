(ns user
  (:require
   [figwheel-sidecar.repl-api :as f]
   [aleph.http :as http]
   [xom.server :as server]
   [datomic.api :as d]
   ))

(def server (atom nil))
(defn start
  []
  (f/start-figwheel! {:builds [{:id :dev
                                :source-paths ["src"]
                                :figwheel true
                                :compiler {:main 'xom.ui
                                           :asset-path "js/compiled/out"
                                           :output-to "resources/public/js/compiled/xom.js"
                                           :output-dir "resources/public/js/compiled/out"
                                           :source-map-timestamp true
                                           }
                                }]})
  (reset! server (http/start-server server/my-app
                     {:port 3000}))
  (when (d/create-database "datomic:mem://xom")
    (d/transact (xom.server/conn) xom.server/schema))
  )

(defn stop
  []
  (f/stop-figwheel!)
  (.close @server)
  (reset! server nil)
  (d/delete-database "datomic:mem//xom"))

(comment
  (start)
  (stop)
  )
