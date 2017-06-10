(ns xom.ui
  (:require
    [taoensso.sente :as sente]
    [goog.dom :as gdom]
    [om.next :as om :refer-macros [defui]]
    [om.dom :as dom]
    [clojure.pprint :refer [pprint]]))

(enable-console-print!)

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
        (name (or mark :e))))))

(def pos-render (om/factory Pos {:keyfn :pos/col}))

(defui ^:once Game
  static om/Ident
  (ident [this {:db/keys [id]}]
    [:db/id id])
  static om/IQuery
  (query [this]
    [:db/id
     {:xom/positions (om/get-query Pos)}
     :xom/winner])
  Object
  (render
    [this]
    (let [{:xom/keys [winner positions]} (om/props this)
          indexed (group-by (juxt :pos/row :pos/col) positions)]
      (dom/div nil
               (when (not= winner :none)
                 (dom/div nil (str winner " wins!")
                 (dom/button
                   #js {:onClick #(om/transact! this [(list 'xom/new-game {}) :xom/game])}
                   "click for new game")))
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
     {:xom/game (om/get-query Game)}])
  Object
  (render [this]
    (let [{:xom/keys [my-uid game]} (om/props this)]
      (dom/div nil
               (dom/div nil (str "playing as: " my-uid))
               (when game
                 (game-render game)
                 )
               ))))

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

(defmulti handle-ws :id)

(defonce setup-ws
  (do
    (let [{:keys [chsk ch-recv send-fn state]}
          (sente/make-channel-socket! "/chsk"
                                      {:type :auto
                                       :packer :edn})]
      (def ch-chsk    ch-recv)
      (def chsk-send! send-fn)
      (def chsk-state state))

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
                           (cb r ))))}))

; wait until websocket is established before mounting root, otherwise we have to buffer
; queries or re-run them
(defmethod handle-ws :chsk/handshake
  [e]
  (println "Adding root")
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

