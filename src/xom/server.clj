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

(def schema [{:db/ident :xom/game-id
              :db/valueType :db.type/uuid
              :db/cardinality :db.cardinality/one
              :db/unique :db.unique/identity}
             {:db/ident :xom/positions
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

(defn create-game
  "Create a new game"
  [player-x player-y]
  {:xom/game-id (java.util.UUID/randomUUID)
   :xom/player-x player-x
   :xom/player-y player-y
   :xom/positions []})

(defn player-turn
  "Return whose turn it is (x goes first)."
  [game]
  (let [turns (count (:xom/positions game))]
    (if (even? turns)
      :x
      :y)))

(defn board
  "Return a 2d vector representing the board"
  [game]
  (reduce (fn [b {:keys [pos/row pos/col pos/mark]}]
            (assoc-in b [row col] mark)) [[nil nil nil] [nil nil nil] [nil nil nil]] (:xom/positions game)))

(defn winner
  "Return the winner for the given board Returns :none, :cat, :x or :y"
  [board]
  (let [check-win (fn [[p1 p2 p3]]
                    (if (= (get-in board p1) (get-in board p2) (get-in board p3))
                      (get-in board p1)))]
    (or
     (some check-win [[[0 0] [0 1] [0 2]] ; top
                      [[1 0] [1 1] [1 2]] ; middle
                      [[2 0] [2 1] [2 2]] ; bottom
                      [[0 0] [1 0] [2 0]] ; left
                      [[0 1] [1 1] [2 1]] ; center
                      [[0 2] [1 2] [2 2]] ; right
                      [[0 0] [1 1] [2 2]] ; diag
                      [[2 0] [1 1] [0 2]] ; diag
                      ])
     (if (= 9 (->> board flatten (remove nil?) count))
       :cat)
     :none)))

(defn valid-move?
  [game {:keys [pos/row pos/col pos/mark]}]
  (and game
       (= mark (player-turn game))
       (< row 3)
       (< col 3)
       (empty? (filter #(and (= row (:pos/row %))
                             (= col (:pos/col %))) (:xom/positions game)))))

(defn game
  "return the game entity for the game id or nil if the game does not exist"
  [db id]
  (d/entity db [:xom/game-id id]))

(defn move
  "Return a transaction to apply this move or nil if the move is invalid"
  [db [game-id row col mark]]
  (let [g (game db game-id)
        m {:pos/row row :pos/col col :pos/mark mark}]
    (if (valid-move? g m)
      [{:xom/game-id game-id
        :xom/positions [m]}])))

(comment
 (even? 0)

 (player-turn (create-game "x" "y"))

 (valid-move? (create-game "x" "y") {:pos/row 0 :pos/col 0 :pos/mark :y})
 (valid-move? (create-game "x" "y") {:pos/row 0 :pos/col 0 :pos/mark :x})
 (valid-move? (create-game "x" "y") {:pos/row 4 :pos/col 3 :pos/mark :x})

 (winner [[:x :x :y] [:y :x :x] [:x :y :y]])
 (winner [[:x :x :x]  [:x :x :y]  [:y :y :y]] )

 (winner (board {:xom/positions [{:pos/row 0 :pos/col 0 :pos/mark :x}]}))
(->> (board {:xom/positions [{:pos/row 0 :pos/col 0 :pos/mark :x}]}) flatten (remove nil?) count)
 )

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
