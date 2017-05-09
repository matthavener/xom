(ns user
  (:require
   [figwheel-sidecar.repl-api :as f]))

(defn start
  []
  (f/start-figwheel! {:builds [{:id :dev
                                :source-paths ["src"]
                                :compiler {:main 'xom.ui
                                           :asset-path "js/compiled/out"
                                           :output-to "resources/public/js/compiled/xom.js"
                                           :output-dir "resources/public/js/compiled/out"
                                           :source-map-timestamp true
                                           }
                                }]}))

