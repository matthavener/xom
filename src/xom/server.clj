(ns xom.server
  (:require
    [taoensso.sente :as sente]
    [taoensso.sente.server-adapters.aleph :refer (get-sch-adapter)]
    [compojure.core :refer [defroutes GET POST]]
    [ring.middleware.resource]
    [ring.middleware.keyword-params]
    [ring.middleware.params]
    [ring.middleware.session]
    [datomic.api :as d]
    [om.next.server :as om]))

(defn log
  "Extremely simple logging funtion"
  [& m]
  (spit "server.log" (apply str m "\n") :append true))

(def schema
  "The database schema for the game. Positions of :x and :o are recorded flatten"
  [{:db/ident :player/id
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :xom/game-id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :xom/player-x
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}
   {:db/ident :xom/player-o
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}
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
  [player-x player-o]
  (cond->
    {:xom/game-id (java.util.UUID/randomUUID)
     :xom/winner :none
     :xom/positions (for [col (range 0 3)
                          row (range 0 3)]
                      {:pos/col col :pos/row row})}
    player-x
    (assoc :xom/player-x [:player/id player-x])

    player-o
    (assoc :xom/player-o [:player/id player-o])))

(defn player-turn
  "Return whose turn it is (x goes first)."
  [game]
  (let [turns (count (keep :pos/mark (:xom/positions game)))]
    (if (even? turns)
      :x
      :o)))

(defn player-mark
  [game uid]
  (cond
   (= uid (-> game :xom/player-x :player/id))
   :x

   (= uid (-> game :xom/player-o :player/id))
   :o))

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
  "Return true if the new position is valid given the current game"
  [game {:keys [pos/row pos/col pos/mark]}]
  (and game
       (= mark (player-turn game))
       (< row 3)
       (< col 3)
       (empty? (filter #(and
                          (:pos/mark %)
                          (= row (:pos/row %))
                          (= col (:pos/col %)))
                       (:xom/positions game)))))

(defn move
  "Return a transaction to apply this move or nil if the move is invalid"
  [db new-pos user]
  (let [m (d/entity db (:db/id new-pos))
        g (:xom/_positions m)
        mark (player-mark g user)
        new-pos (assoc new-pos :pos/mark mark)]
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

(defn player
  "lookup player"
  [db id]
  (d/entity db [:player/id id]))

(defn ensure-player
  "Ensure the player with id exists, return the player entity"
  [conn id]
  (if-let [p (d/entity (d/db conn) [:player/id id])]
    p
    (let [{:keys [db-after]} (d/transact conn [{:player/id id}])]
      (d/entity db-after [:player/id id]))))

(defn active-games
  [db query]
  (d/q '[:find (pull ?g query)
         :in $ query
         :where
         [?g :xom/game-id]
         [?g :xom/winner :none]] db query))

(comment
 (active-games (d/db (conn)) '[*])

 (d/q '[:find (pull ?g [:xom/game-id :xom/winner {:xom/positions [:pos/row :pos/col :pos/mark]}])
        :where
        [?g :xom/game-id]] (d/db (conn)))

 (player-turn (create-game "x" "o"))

 (valid-move? (create-game) {:pos/row 0 :pos/col 0 :pos/mark :o})
 (valid-move? (create-game) {:pos/row 0 :pos/col 0 :pos/mark :x})
 (valid-move? (create-game) {:pos/row 4 :pos/col 3 :pos/mark :x})

 (winner [[:x :x :o] [:o :x :x] [:x :o :o]])
 (winner [[:x :x :x]  [:x :x :o]  [:o :o :o]] )

 (winner (board {:xom/positions [{:pos/row 0 :pos/col 0 :pos/mark :x}]}))
 (->> (board {:xom/positions [{:pos/row 0 :pos/col 0 :pos/mark :x}]})
      flatten (remove nil?) count))

(defmulti om-read om/dispatch)
(defmulti om-mutate om/dispatch)

(defmethod om-read :default
  [e k p]
  (log "default read: " k (keys e) p))

(defmethod om-read :xom/my-uid
  [{:keys [uid] :as e} k p]
  {:value uid})

(defmethod om-read :xom/game
  [{:keys [conn db query uid]} k p]
  (log "uid: " uid)
  (let [game (d/q '[:find (pull ?e query) .
                    :in $ ?uid query
                    :where
                    [?p :player/id ?uid]
                    [?e :xom/game-id]
                    [?e :xom/winner :none]
                    (or [?e :xom/player-x ?p]
                        [?e :xom/player-o ?p])]
                  (or db (d/db conn))
                  uid
                  (or query ['* {:xom/positions ['*]}]))]
    (log "got game" game)
    {:value game}))

(defmethod om-read :xom/active-games
  [{:keys [conn query]} k p]
  (let [games (d/q '[:find [(pull ?g query) ...]
                     :in $ query
                     :where
                     [?g :xom/game-id]
                     [?g :xom/winner :none]] (d/db conn) query)]
    {:value (into [] games)}))

(defmethod om-read :xom/my-games
  [{:keys [conn query uid]} k p]
  (log ":xom/my-games: " uid)
  (let [games (d/q '[:find [(pull ?g query) ...]
                     :in $ ?uid query
                     :where
                     [?p :player/id ?uid]
                     [?g :xom/game-id]
                     (or [?g :xom/player-x ?p]
                         [?g :xom/player-o ?p])] (d/db conn) uid query)]
    (log "found games: " games)
    {:value (into [] games)}))

(defmethod om-read :xom/waiting-games
  [{:keys [conn query uid]} k p]
  (log ":xom/waiting-games: " uid)
  (let [games (d/q '[:find [(pull ?g query) ...]
                     :in $ ?uid query
                     :where
                     [?p :player/id ?uid]
                     [?g :xom/game-id]
                     [?g :xom/player-x]
                     (not [?g :xom/player-o])
                     (not [?g :xom/player-x ?p])] (d/db conn) uid query)]
    {:value (into [] games)}))

;; this is unused now
(defmethod om-mutate 'xom/new-game
  [{:keys [conn uid send-fn connected-uids parser] :as env} k p]
  (log "xom/new-game " p uid)
  {:action (fn []
             (let [{:keys [db-after]} @(d/transact conn [(create-game nil nil)])]
               (doseq [uid (:ws @connected-uids)]
                 (send-fn uid [:xom/data
                               (parser (assoc env :db db-after) [:xom/game])]))))})

(defmethod om-mutate 'xom/begin-game
  [{:keys [conn uid]} k p]
  {:action (fn []
             @(d/transact conn [(create-game uid nil)])
             nil)})

(defmethod om-mutate 'xom/join-game
  [{:keys [conn uid send-fn connected-uids]} k {game-id :xom/game-id}]
  {:action (fn []
             (let [db (d/db conn)
                   pid (:db/id (d/entity db [:player/id uid]))
                   g (d/entity db [:xom/game-id game-id])
                   ]
               @(d/transact conn [{:db/id game-id :xom/player-o pid}])
               (doseq [u (:ws @connected-uids)]
                 (send-fn u
                          [:xom/data {[:db/id game-id] {:xom/player-o [:db/id pid]}
                                      [:db/id pid] {:db/id pid :player/id uid}}])))
             nil)})

(defmethod om-mutate 'xom/mark
  [{:keys [conn uid send-fn connected-uids]} k p]
  (log "xom/mark " p uid)
  {:action
   (fn []
     (if-let [tx (move (d/db conn) p uid)]
       (let [{:keys [db-after]} @(d/transact conn tx)]
         (doseq [uid (:ws @connected-uids)]
           (send-fn
             uid
             [:xom/data (into {} (map (fn [m] [[:db/id (:db/id m)] m])) tx)])))))})

(defmethod om-mutate :default
  [e k p]
  (log "default mutate: " k (keys e) p))

(def om-parser (om/parser {:read om-read :mutate om-mutate}))

(defn conn [] (d/connect "datomic:mem://xom"))

(comment
 (d/q '[:find ?e :where [?e :db/type ]] (d/db (conn)))

 (d/q '[:find (pull ?g [:xom/game-id {:xom/player-x [:player/id]}  {:xom/player-o [:player/id]} :xom/winner]) :where [?g :xom/game-id ]] (d/db (conn)))

 (om-parser {:conn (conn)} [{:xom/active-games [:xom/game-id {:xom/player-x [:player/id]} {:xom/player-o [:player/id]}]}])
 (d/q '[:find [(pull ?g query) ...]
        :in $ ?uid query
        :where
        [?p :player/id ?uid]
        [?g :xom/game-id]
        [?g :xom/player-x]
        (not [?g :xom/player-o])
        (not [?g :xom/player-x ?p]) ] (d/db (conn)) "bob" '[*])

 (d/q '[:find ?id
        :where [_ :player/id ?id]] (d/db (conn)))
 )

(defmulti event-msg-handler :id)

(let [{:keys [ch-recv send-fn connected-uids
              ajax-post-fn ajax-get-or-ws-handshake-fn]}
       (sente/make-channel-socket!
         (get-sch-adapter))]

  (def ring-ajax-post                ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk                       ch-recv) ; ChannelSocket's receive channel
  (def connected-uids                connected-uids))

(comment
  (deref connected-uids))

; this is the websocket entry point for all om.next queries and transactions
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

(defn render-index
  "Render the empty index page that om.next will attach to"
  []
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "<html><body><div id=\"app\">Loading...</div>
         <script src=\"/js/compiled/xom.js\"></script></body></html>"})

(defroutes my-app-routes
  (GET "/" req
    (log "index session: " (:session req))
    (render-index))
  (GET  "/chsk" req (ring-ajax-get-or-ws-handshake req))
  (POST "/chsk" req (ring-ajax-post req))
  (POST "/login" [user-id :as req]
        (let [{:keys [session conn]} req
              player (ensure-player conn user-id)]
          (log "conn: " conn)
          (log "session: " session " userid: " user-id " player: " (d/touch player))
          {:status 200
           :body user-id
           :session (assoc session :uid user-id)})))

(defn wrap-datomic
  [handler]
  (fn [request]
    (handler (assoc request :conn (conn)))))

(def my-app
  (-> #'my-app-routes
      ring.middleware.keyword-params/wrap-keyword-params
      ring.middleware.params/wrap-params
      ring.middleware.session/wrap-session
      wrap-datomic
      (ring.middleware.resource/wrap-resource "public")))
