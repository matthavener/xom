(ns user
  (:require
   [figwheel-sidecar.repl-api :as f]
   [aleph.http :as http]
   [xom.server :as server]
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
  )

(defn stop
  []
  (f/stop-figwheel!)
  (.close @server)
  (reset! server nil))

(comment
  (start)
  (stop)
  )
