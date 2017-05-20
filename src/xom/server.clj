(ns xom.server
  (:require
    [taoensso.sente :as sente]
    [taoensso.sente.server-adapters.aleph :refer (get-sch-adapter)]
    [compojure.core :refer [defroutes GET POST]]
    [ring.middleware.resource]
    [ring.middleware.keyword-params]
    [ring.middleware.params]
    )
  )

(defn log [& m] (spit "server.log" (apply str m "\n") :append true))

(defmulti event-msg-handler :id)

(defmethod event-msg-handler :xom/query
  [{:keys [?reply-fn] :as e}]
  (log "got " e)
  (?reply-fn {:message "hello world"}))

(defmethod event-msg-handler :default
  [e]
  (log "handle-ws :default"(select-keys e [:id :event])))

(let [{:keys [ch-recv send-fn connected-uids
              ajax-post-fn ajax-get-or-ws-handshake-fn]}
       (sente/make-channel-socket!
         (get-sch-adapter)
         {:user-id-fn (fn [_] (str (java.util.UUID/randomUUID)))})]

  (def ring-ajax-post                ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk                       ch-recv) ; ChannelSocket's receive channel
  (def chsk-send!                    send-fn) ; ChannelSocket's send API fn
  (def connected-uids                connected-uids) ; Watchable, read-only atom)
)

(sente/start-server-chsk-router! ch-chsk event-msg-handler)

(defn render-index []
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "<html><body><div id=\"app\">Loading...</div>
         <script src=\"/js/compiled/xom.js\"></script></body></html>"
   }
  )
(defroutes my-app-routes
  (GET "/" req (render-index) )
  (GET  "/chsk" req (ring-ajax-get-or-ws-handshake req))
  (POST "/chsk" req (ring-ajax-post                req))
  )

(def my-app
  (-> my-app-routes
      ;; Add necessary Ring middleware:
      ring.middleware.keyword-params/wrap-keyword-params
      ring.middleware.params/wrap-params
      (ring.middleware.resource/wrap-resource "public")))
