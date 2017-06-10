(ns xom.server
  (:require
    [taoensso.sente :as sente]
    [taoensso.sente.server-adapters.aleph :refer (get-sch-adapter)]
    [compojure.core :refer [defroutes GET POST]]
    [ring.middleware.resource]
    [ring.middleware.keyword-params]
    [ring.middleware.params]
    [datomic.api :as d]
    [om.next.server :as om]
    )
  )

(defn log [& m] (spit "server.log" (apply str m "\n") :append true))

(def schema [{:db/ident :xom/game-id
              :db/valueType :db.type/uuid
              :db/cardinality :db.cardinality/one
              :db/unique :db.unique/identity}
             {:db/ident :xom/winner
              :db/valueType :db.type/keyword
              :db/cardinality :db.cardinality/one}
             {:db/ident :xom/positions
              :db/valueType :db.type/ref
              :db/isComponent true
              :db/cardinality :db.cardinality/many} ; coll-of :pos/{row,col,mark}
             {:db/ident :pos/row
              :db/valueType :db.type/long
              :db/cardinality :db.cardinality/one}
             {:db/ident :pos/col
              :db/valueType :db.type/long
              :db/cardinality :db.cardinality/one}
             {:db/ident :pos/mark
              :db/valueType :db.type/keyword
              :db/cardinality :db.cardinality/one} ; :x or :o
             ])

(defn create-game
  "Create a new game"
  []
  {:xom/game-id (java.util.UUID/randomUUID)
   :xom/winner :none
   :xom/positions (for [col (range 0 3)
                        row (range 0 3)]
                    {:pos/col col :pos/row row})})

(defn player-turn
  "Return whose turn it is (x goes first)."
  [game]
  (let [turns (count (keep :pos/mark (:xom/positions game)))]
    (if (even? turns)
      :x
      :o)))

(defn board
  "Return a 2d vector representing the board"
  [game]
  (reduce (fn [b {:keys [pos/row pos/col pos/mark]}]
            (assoc-in b [row col] mark)) [[nil nil nil] [nil nil nil] [nil nil nil]] (:xom/positions game)))

(defn winner
  "Return the winner for the given board Returns :none, :cat, :x or :o"
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
  (log "valid-move? " row col (player-turn game) mark)
  (and game
       (= mark (player-turn game))
       (< row 3)
       (< col 3)
       (empty? (filter #(and
                          (:pos/mark %)
                          (= row (:pos/row %))
                          (= col (:pos/col %)))
                       (:xom/positions game)))))

(defn game
  "return the game entity for the game id or nil if the game does not exist"
  [db id]
  (d/entity db [:xom/game-id id]))

(defn move
  "Return a transaction to apply this move or nil if the move is invalid"
  [db new-pos]
  (let [m (d/entity db (:db/id new-pos))
        g (:xom/_positions m) ]
    (log "move " m g new-pos)
    (if (and m g (valid-move? g new-pos))
      (let [new-winner (-> (d/with db [new-pos])
                           :db-after
                           (d/entity (:db/id g))
                           board
                           winner)]
        (log "new-winner " new-winner)
        [{:db/id (:db/id g) :xom/winner new-winner}
         new-pos])
      (log "illegal move " new-pos))))

(comment
 (even? 0)

 (player-turn (create-game "x" "o"))

 (valid-move? (create-game "x" "o") {:pos/row 0 :pos/col 0 :pos/mark :o})
 (valid-move? (create-game "x" "o") {:pos/row 0 :pos/col 0 :pos/mark :x})
 (valid-move? (create-game "x" "o") {:pos/row 4 :pos/col 3 :pos/mark :x})

 (winner [[:x :x :o] [:o :x :x] [:x :o :o]])
 (winner [[:x :x :x]  [:x :x :o]  [:o :o :o]] )

 (winner (board {:xom/positions [{:pos/row 0 :pos/col 0 :pos/mark :x}]}))
(->> (board {:xom/positions [{:pos/row 0 :pos/col 0 :pos/mark :x}]}) flatten (remove nil?) count)
 )

(defmulti om-read om/dispatch)
(defmulti om-mutate om/dispatch)

(defmethod om-read :default
  [e k p]
  (log "default read: " k (keys e) p))

(defmethod om-read :xom/my-uid
  [{:keys [uid] :as e} k p]
  {:value uid}
  )

(defmethod om-read :xom/game
  [{:keys [conn query]} k p]
  (let [game (d/q '[:find (pull ?e query) .
                    :in $ query
                    :where
                    [?e :xom/game-id]
                    [?e :xom/winner :none]] (d/db conn) query)]
    (log "got game" game)
    {:value game}))

(defmethod om-mutate 'xom/new-game
  [{:keys [conn uid send-fn connected-uids]} k p]
  (log "xom/new-game " p uid)
  {:action (fn []
             @(d/transact conn [(create-game)])
             nil)})

(defmethod om-mutate 'xom/mark
  [{:keys [conn uid send-fn connected-uids]} k p]
  (log "xom/mark " p uid)
  {:action
   (fn []
     (try
       (if-let [tx (move (d/db conn) (assoc p :pos/mark uid))]
         (let [{:keys [db-after]} @(d/transact conn tx)]
           (doseq [uid (:ws @connected-uids)]
             (send-fn uid
                      [:xom/data
                       (into {} (map (fn [m] [[:db/id (:db/id m)] m])) tx)]))))
       (catch Exception e (log e))))})

(defmethod om-mutate :default
  [e k p]
  (log "default mutate: " k (keys e) p))

(def om-parser (om/parser {:read om-read :mutate om-mutate}))
(defn conn [] (d/connect "datomic:mem://xom"))

(comment (d/q '[:find ?e :where [?e :db/type ]] (d/db (conn))))

(defmulti event-msg-handler :id)

(declare uid-fn)

(let [{:keys [ch-recv send-fn connected-uids
              ajax-post-fn ajax-get-or-ws-handshake-fn]}
       (sente/make-channel-socket!
         (get-sch-adapter)
         {:user-id-fn #'uid-fn})]

  (def ring-ajax-post                ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk                       ch-recv) ; ChannelSocket's receive channel
  (def connected-uids                connected-uids)
)

(defn uid-fn [_]
  (let [uids (:ws @connected-uids)]
    (log "user-id-fn " (count uids) uids)
    (cond
     (empty? uids)
     :x
     (uids :o)
     :x
     (uids :x)
     :o
     :else
     nil)))
(comment
  (deref connected-uids)
  )

(defmethod event-msg-handler :xom/query
  [{:keys [?reply-fn ?data ring-req] :as e}]
  (log "'xom/query got: " (keys e) ?data)
  (try
    (?reply-fn (om-parser (merge (select-keys e [:uid :connected-uids :send-fn])
                               (select-keys ring-req [:conn])) ?data))
    (catch Exception ex
      (log ex))))

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

(defn wrap-datomic
  [handler]
  (fn [request]
    (handler (assoc request :conn (conn)))))

(def my-app
  (-> my-app-routes
      ring.middleware.keyword-params/wrap-keyword-params
      ring.middleware.params/wrap-params
      wrap-datomic
      (ring.middleware.resource/wrap-resource "public")))
