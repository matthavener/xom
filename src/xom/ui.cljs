(ns xom.ui
  (:require
    [taoensso.sente :as sente]
    [goog.dom :as gdom]
    [om.next :as om :refer-macros [defui]]
    [om.dom :as dom]
    [clojure.pprint :refer [pprint]]))

(enable-console-print!)

(declare login!)

(def mark->symbol
  {:o "⭕"
   :x "❌"
   :e "◻"
   }
  )

(defui ^:once Pos
  static om/Ident
  (ident [this {:db/keys [id]}]
    [:db/id id])
  static om/IQuery
  (query [this]
    [:db/id :pos/row :pos/col :pos/mark])
  Object
  (render
    [this]
    (let [{:pos/keys [mark] :as p} (om/props this)]
      (dom/span
        #js {:style #js {:fontSize "32pt"}
             :onClick #(om/transact! this [(list 'xom/mark p)])}
        (mark->symbol (or mark :e))))))

(def pos-render (om/factory Pos {:keyfn :pos/col}))

(defui ^:once Player
  static om/Ident
  (ident [this {:db/keys [id]}]
    [:db/id id])
  static om/IQuery
  (query [this]
    [:db/id :player/id]
    )
  )

(defn my-uid
  [c]
  (-> c om/shared :uid deref))

(defn player-turn
  "Return whose turn it is (x goes first)."
  [game]
  (let [turns (count (keep :pos/mark (:xom/positions game)))]
    (if (even? turns)
      (-> game :xom/player-x :player/id)
      (-> game :xom/player-o :player/id))))

(defui ^:once Game
  static om/Ident
  (ident [this {:db/keys [id]}]
    [:db/id id])
  static om/IQuery
  (query [this]
    [:db/id
     :xom/game-id
     {:xom/player-x (om/get-query Player)}
     {:xom/player-o (om/get-query Player)}
     {:xom/positions (om/get-query Pos)}
     :xom/winner])
  Object
  (render
    [this]
    (let [{:xom/keys [winner positions player-x player-o game-id] :as game} (om/props this)
          turn-id (player-turn game)
          my-id (my-uid this)
          indexed (group-by (juxt :pos/row :pos/col) positions)]
      (dom/div nil
               (dom/div nil
                 (str "player: " (:player/id player-x "Waiting") "(X) vs " (:player/id player-o "Waiting") "(O)")
                 )
               (if (not= winner :none)
                 (dom/div nil (str winner " wins!"))
                 (dom/div nil
                   "Its "
                   (if (= my-id turn-id)
                     (dom/strong nil "YOUR")
                     (str turn-id "'s"))
                   " turn!"))
               (mapv
                 (fn [row]
                   (dom/div
                     #js {:key row}
                     (mapv
                       (fn [col]
                         (let [pos (first (indexed [row col]))]
                           (pos-render pos)))
                       (range 0 3))))
                 (range 0 3))))))

(def game-render (om/factory Game))

(defui ^:once Xom
  static om/IQuery
  (query [this]
    [:xom/my-uid
     {:xom/active-games (om/get-query Game)}
     {:xom/my-games (om/get-query Game)}
     {:xom/waiting-games (om/get-query Game)}
     ])
  Object
  (render [this]
    (let [{:xom/keys [my-uid active-games my-games waiting-games]} (om/props this)]
      (if (= my-uid :taoensso.sente/nil-uid)
        (dom/div nil
          (dom/div nil
            (dom/p nil "Login")
            (dom/input #js {:type "text"
                            :value (or (om/get-state this :login-id) "")
                            :onChange (fn [e]
                                         (om/update-state! this assoc :login-id (.. e -target -value)))})
            (dom/button #js {:onClick (fn [_]
                                         (println "Click!")
                                        (login! (om/get-state this :login-id)
                                                #(om/transact! this [(list 'login {:uid %}) :xom/active-games :xom/my-games :xom/waiting-games])
                                                )
                                        )} "Login")))
        (dom/div nil
          (dom/div nil (str "playing as: " my-uid))

          (dom/div nil
            (dom/h2 nil "My Games")
            (apply dom/ul nil
                   (map (fn [g]
                          (dom/li #js {:key (str (:xom/game-id g))}
                                  (game-render g))) my-games)))

          (dom/div nil
            (dom/h2 nil "Active Games")
            (apply dom/ul nil
                    (map (fn [g]
                           (dom/li #js {:key (str (:xom/game-id g))}
                                   (game-render g))) active-games)))

          (dom/div nil
            (dom/h2 nil "Waiting Games")
            (apply dom/ul nil
                   (map (fn [g]
                          (dom/li #js {:key (str (:xom/game-id g))}
                                  (dom/p nil (str (-> g :xom/player-x :player/id) " " (:xom/game-id g))
                                         (dom/button #js {:onClick (fn [_]
                                                                     (om/transact! this [(list 'xom/join-game {:game/dbid (:db/id g)})]))} "Join")))) waiting-games)))

          (dom/div nil
            (dom/button #js {:onClick (fn [_]
                                        (om/transact! this [(list 'xom/begin-game) :xom/my-games])
                                        )} "Create new game")
            )
          )))))

(defmulti read (fn [env key params] key))

(defmethod read :default
  [{:keys [query state] :as env} key params]
  (let [st @state]
    (if-let [[_ value] (find st key)]
      {:value (if (vector? value)
                (om/db->tree query value st)
                value)}
      {:remote true})))

(defmulti mutate (fn [env key params] key))

(defmethod mutate :default
  [{:keys [state] :as env} key params]
  (let [st @state]
    {:remote true}))

(defmethod mutate 'login
  [{:keys [state] :as env} key {:keys [uid]}]
  {:action
   (fn []
     (swap! state (fn [s]
                    (-> s
                        (assoc :xom/my-uid uid)
                        ;; force re-read of these
                        (dissoc :xom/active-games :xom/waiting-games :xom/my-games)))))})

(defmethod mutate 'server/add-new-game
  [{:keys [state]} _ game]
  {:action
   (fn []
     (swap! state (fn [s]
                    (let [me (:xom/my-uid s)
                          uid-x (-> game :xom/player-x :player/id)
                          db (om/tree->db Game game)]
                      (cond-> (-> s
                                  (update :db/id merge (:db/id (meta db)))
                                  (update :db/id assoc (:db/id db) db))
                        (= me uid-x)
                        (->
                         (update :xom/my-games conj [:db/id (:db/id game)])
                         (update :xom/active-games conj [:db/id (:db/id game)]))

                        (not= me uid-x)
                        (update :xom/waiting-games conj [:db/id (:db/id game)])

                        )
                      ))))})

(defmethod mutate 'server/game-joined
  [{:keys [state]} _ {game-id :game/dbid {player-id :player/id playerdbid :db/id :as player} :player}]
  {:action
   (fn []
     (println "Joining game: " game-id " player: " player-id)
     (swap! state (fn [s]
                    (cond-> (-> s
                                (update :xom/waiting-games (fn [games] (into [] (remove #(= (second %) game-id)) games)))
                                (update-in [:db/id game-id] assoc :xom/player-o [:db/id playerdbid])
                                (update :db/id assoc playerdbid player))
                      (= player-id (:xom/my-uid s))
                      (update :xom/my-games conj [:db/id game-id])

                      (not= player-id (:xom/my-uid s))
                      (update :xom/active-games conj [:db/id game-id])))))})

(defmulti handle-ws :id)

(defonce uid-atom (atom nil))

(defonce setup-ws
  (do
    (let [{:keys [chsk ch-recv send-fn state]}
          (sente/make-channel-socket! "/chsk"
                                      {:type :auto
                                       :packer :edn})]
      (def ch-chsk    ch-recv)
      (def chsk-send! send-fn)
      (def chsk-state state)
      (defn login!
        [username after]
        (sente/ajax-lite "/login"
                         {:method :post
                          :headers {:X-CSRF-Token (:csrf-token @chsk-state)}
                          :params {:user-id username}}
                         (fn [resp]
                           (println "login-response:" (pr-str resp))
                           (when (= 200 (:?status resp))
                             (reset! uid-atom (:?content resp))
                             (sente/chsk-reconnect! chsk)
                             (after (:?content resp)))))))

    (sente/start-client-chsk-router!
            ch-chsk handle-ws)))

(defonce reconciler
  (om/reconciler
    {:parser (om/parser {:read read :mutate mutate})
     :shared {:uid uid-atom}
     :send (fn [{:keys [remote]} cb]
             (println "sending " remote)
             (chsk-send! [:xom/query remote]
                         5000
                         (fn [r]
                           (println "received reply " r)
                           (cb r))))}))

; wait until websocket is established before mounting root, otherwise we have to buffer
; queries or re-run them
(defmethod handle-ws :chsk/handshake
  [e]
  (println "Adding root" (->  e :state deref :uid))
  (reset! uid-atom (->  e :state deref :uid))
  (om/add-root! reconciler Xom (gdom/getElement "app")))

; for figwheel
(when (:open? @chsk-state)
  (om/add-root! reconciler Xom (gdom/getElement "app")))

(defmethod handle-ws :chsk/recv
  [{:keys [event] :as e}]
  (println "received async " event)
  (let [m (second (second event))]
    (if (map? m)
      (om/merge! reconciler m)
      (om/transact! reconciler m))))

(defmethod handle-ws :default
  [e]
  (println (select-keys e [:id :event])))

(comment

 (def bob (deref reconciler))

 (-> reconciler :config :shared)

 (pprint bob)
 (pprint (deref reconciler))

 (reset! (-> reconciler :config :state) bob)

(om/transact! reconciler '[(server/game-joined {:player/id "alice", :game/dbid 17592186045420})])

{[:db/id 17592186045420] {:xom/player-o {:player/id "alice", :db/id 17592186045431}, :xom/game-id #uuid "ec49ab8b-a3a2-4ac6-b1a6-0eaaa0ba0d9c", :xom/player-x {:player/id "bob", :db/id 17592186045418}, :xom/positions [{:pos/row 0, :pos/col 0, :db/id 17592186045421} {:pos/row 1, :pos/col 0, :db/id 17592186045422} {:pos/row 2, :pos/col 0, :db/id 17592186045423} {:pos/row 0, :pos/col 1, :db/id 17592186045424} {:pos/row 1, :pos/col 1, :db/id 17592186045425} {:pos/row 2, :pos/col 1, :db/id 17592186045426} {:pos/row 0, :pos/col 2, :db/id 17592186045427} {:pos/row 1, :pos/col 2, :db/id 17592186045428} {:pos/row 2, :pos/col 2, :db/id 17592186045429}], :db/id 17592186045420, :xom/winner :none}}


 (:db/id (meta (om/tree->db Game {:db/id 17592186045422, :xom/game-id #uuid "724cf0c4-d2e1-4495-bb11-6994ba2888f4", :xom/player-x {:db/id 17592186045418, :player/id "chris"}, :xom/positions [{:db/id 17592186045423, :pos/row 0, :pos/col 0} {:db/id 17592186045424, :pos/row 1, :pos/col 0} {:db/id 17592186045425, :pos/row 2, :pos/col 0} {:db/id 17592186045426, :pos/row 0, :pos/col 1} {:db/id 17592186045427, :pos/row 1, :pos/col 1} {:db/id 17592186045428, :pos/row 2, :pos/col 1} {:db/id 17592186045429, :pos/row 0, :pos/col 2} {:db/id 17592186045430, :pos/row 1, :pos/col 2} {:db/id 17592186045431, :pos/row 2, :pos/col 2}], :xom/winner :none})))

  (om/transact! reconciler '[(server/game-joined {:player/id alice, :game/dbid 17592186045488})])

  (remove #(= (second %) 17592186045455) (:xom/waiting-games (deref reconciler)))
  (pprint (om/db->tree (om/get-query Xom) (deref reconciler) (deref reconciler)))
  (om/merge! reconciler {})
  (om/merge! reconciler  {[:db/id 17592186045444] {:xom/player-o {:player/id "bob", :db/id 17592186045420}}})
  )
