(ns xom.server
  (:require
    [taoensso.sente :as sente]
    [taoensso.sente.server-adapters.aleph :refer (get-sch-adapter)]
    [compojure.core :refer [defroutes GET POST]]
    [ring.middleware.resource]
    [ring.middleware.keyword-params]
    [ring.middleware.params]
    [datomic.api :as d]
    )
  )

(defn log [& m] (spit "server.log" (apply str m "\n") :append true))

(def schema [{:db/ident :xom/positions
              :db/valueType :db.type/ref
              :db/cardinality :db.cardinality/many} ; coll-of :pos/{row,col,mark}
             {:db/ident :xom/player-x
              :db/valueType :db.type/uuid
              :db/cardinality :db.cardinality/one}
             {:db/ident :xom/player-y
              :db/valueType :db.type/uuid
              :db/cardinality :db.cardinality/one}
             {:db/ident :pos/row
              :db/valueType :db.type/long
              :db/cardinality :db.cardinality/one}
             {:db/ident :pos/col
              :db/valueType :db.type/long
              :db/cardinality :db.cardinality/one}
             {:db/ident :pos/mark
              :db/valueType :db.type/keyword
              :db/cardinality :db.cardinality/one} ; :x or :y
             ])

(defn conn [] (d/connect "datomic:mem://xom"))

(comment (d/q '[:find ?e :where [?e :db/type ]] (d/db (conn))))

(defmulti event-msg-handler :id)

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

(remove-all-methods event-msg-handler)

(defn broadcast-players [e]
  (log "notifying " @connected-uids)
  (doseq [uid (:ws @connected-uids)]
    (chsk-send! uid [:xom/data {:xom/connected-users (into [] (:ws @connected-uids))}])
    )
  )

(defmethod event-msg-handler :chsk/uidport-open
  [e]
  (broadcast-players e))

(defmethod event-msg-handler :chsk/uidport-close
  [e]
  (broadcast-players e))

(defmethod event-msg-handler :xom/game
  [{:keys [?reply-fn] :as e}]
  (log "got " (keys e))
  (?reply-fn {:message "hello world"}))

(defmethod event-msg-handler 'xom/new-game
  [{:keys [?reply-fn] :as e}]
  (log "got " (keys e))
  (?reply-fn {:xom/game []}))

(defmethod event-msg-handler :default
  [{:keys [?reply-fn] :as e}]
  (when ?reply-fn
    (?reply-fn {}))
  (log "handle-ws :default" (select-keys e [:id :event])))


(sente/start-server-chsk-router! ch-chsk event-msg-handler)

(defn render-index []
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "<html><body><div id=\"app\">Loading...</div>
         <script src=\"/js/compiled/xom.js\"></script></body></html>"
   })

(defroutes my-app-routes
  (GET "/" req (render-index))
  (GET  "/chsk" req (ring-ajax-get-or-ws-handshake req))
  (POST "/chsk" req (ring-ajax-post req)))

(def my-app
  (-> my-app-routes
      ring.middleware.keyword-params/wrap-keyword-params
      ring.middleware.params/wrap-params
      (ring.middleware.resource/wrap-resource "public")))
