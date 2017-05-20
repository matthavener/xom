(defproject xom "0.1.0-SNAPSHOT"
  :description "Demo om.next app"
  :url "http://localhost:3000"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.229"]
                 [cljsjs/react "15.4.2-1"]
                 [cljsjs/react-dom "15.4.2-1"]
                 [aleph "0.4.3"]
                 [compojure "1.5.1"]
                 [com.taoensso/sente "1.11.0"]
                 [com.datomic/datomic-free "0.9.5561"]
                 [org.omcljs/om "1.0.0-alpha48"]
                 [ring "1.5.0"]
                 [figwheel-sidecar "0.5.8"]]

  :source-paths ["src"]
  :profiles {:dev {:source-paths ["dev" "src"]}}
  )
