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
      (println mark)
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
    (let [{:xom/keys [winner positions player-x player-o]} (om/props this)
          indexed (group-by (juxt :pos/row :pos/col) positions)]
      (dom/div nil
               (dom/div nil
                 (str "player: " (:player/id player-x "Waiting") " vs " (:player/id player-o "Waiting") )
                 )
               (when (not= winner :none)
                 (dom/div nil (str winner " wins!")))
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
     {:xom/game (om/get-query Game)}
     {:xom/active-games (om/get-query Game)}
     {:xom/my-games (om/get-query Game)}
     {:xom/waiting-games (om/get-query Game)}
     ])
  Object
  (render [this]
    (let [{:xom/keys [my-uid game active-games my-games waiting-games]} (om/props this)]
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
                                                #(om/transact! this [(list 'login {:uid (keyword %)})])
                                                )
                                        )} "Login")))
        (dom/div nil
          (dom/div nil (str "playing as: " my-uid))
          (when game
            (game-render game))

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
                                  (dom/p nil (-> g :xom/player-x :player/id)
                                         (dom/button #js {:onClick (fn [_]
                                                                     (om/transact! this [(list 'xom/join-game {:xom/game-id (:db/id g)}) :xom/game]))} "Join")))) waiting-games)))

          (dom/div nil
            (dom/button #js {:onClick (fn [_]
                                        (om/transact! this [(list 'xom/begin-game) :xom/my-games :xom/game])
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
     (swap! state assoc :xom/my-uid uid))})

(defmulti handle-ws :id)

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
                             (sente/chsk-reconnect! chsk)
                             (after (:?content resp)))))))

    (sente/start-client-chsk-router!
            ch-chsk handle-ws)))

(defonce reconciler
  (om/reconciler
    {:parser (om/parser {:read read :mutate mutate})
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
  (println "Adding root: " (select-keys @chsk-state [:id :event]))
  (om/add-root! reconciler Xom (gdom/getElement "app")))

; for figwheel
(when (:open? @chsk-state)
  (om/add-root! reconciler Xom (gdom/getElement "app")))

(defmethod handle-ws :chsk/recv
  [{:keys [event] :as e}]
  (println "received async " event)
  (let [m (second (second event))]
    (om/merge! reconciler m)))

(defmethod handle-ws :default
  [e]
  (println (select-keys e [:id :event])))

(comment
  (pprint (deref reconciler))
  (pprint (om/db->tree (om/get-query Xom) (deref reconciler) (deref reconciler)))
  (om/merge! reconciler {})
  )

(comment
 (deref reconciler)
 )
