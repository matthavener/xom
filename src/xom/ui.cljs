(ns xom.ui
  (:require
    [taoensso.sente :as sente]
    [goog.dom :as gdom]
    [om.next :as om :refer-macros [defui]]
    [sablono.core :refer-macros [html]]
    [om.dom :as dom]
    [clojure.pprint :refer [pprint]]
    [clojure.string :as str]
    ))

(enable-console-print!)

(def mark-style {:width "100px"
                 :height "100px"
                 :display "flex"
                 :align-items "center"
                 :border "1px solid gray"
                 :justify-content "center"})

(def pos-style {:font-size "32pt"
                :cursor "pointer"})

(def flex {:display "flex"
           :justify-content "center"
           :align-items "center"})

(def xom-style {:display "flex"
                :flex-direction "column"
                :align-items "center"
                :justify-content "center"
                :width "100%"})


(defui ^:once Pos
  static om/Ident
  (ident [this {:db/keys [id]}]
    [:db/id id])
  static om/IQuery
  (query [this]
    [:db/id :pos/row :pos/col :pos/mark])
  Object
  (render [this]
    (let [{:pos/keys [mark] :as p} (om/props this)
          is-empty? (nil? mark)]
      (println mark)
      (html
        [:div {:style pos-style
               :on-click #(om/transact! this [(list 'xom/mark p)])}
         [:div {:style mark-style}
          (when-not is-empty? (-> mark name str/capitalize))]]
        ))))

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
  (render [this]
    (let [{:xom/keys [winner positions]} (om/props this)
          indexed (group-by (juxt :pos/row :pos/col) positions)]
      (html
        [:div
         (when (not= winner :none)
           [:div {:style (merge flex {:justify-content "space-between"})}
            [:h2 (str (-> winner name str/capitalize) " wins!")]
            [:button {:style {:font-size "12pt"
                              :padding "1rem 0.5rem"}
                      :on-click #(om/transact! this [(list 'xom/new-game {}) :xom/game])}
             "click for new game"]])
                 (mapv
                   (fn [row]
                     [:div {:key row :style flex}
                      (mapv (fn [col]
                              (let [pos (first (indexed [row col]))]
                                (pos-render pos)))
                            (range 0 3))])
                   (range 0 3))]))))

(def game-render (om/factory Game))


(defui ^:once Xom
  static om/IQuery
  (query [this]
    [:xom/my-uid
     {:xom/game (om/get-query Game)}])
  Object
  (render [this]
    (let [{:xom/keys [my-uid game]} (om/props this)]
      (html
        [:div {:style xom-style}
         [:h1 (str "Playing as " (str/capitalize (name my-uid)))]
         (when game
           (game-render game))]))))

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

