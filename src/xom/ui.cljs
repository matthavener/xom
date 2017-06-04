(ns xom.ui
  (:require
    [taoensso.sente :as sente]
    [goog.dom :as gdom]
    [om.next :as om :refer-macros [defui]]
    [om.dom :as dom]
    [clojure.pprint :refer [pprint]]))

(enable-console-print!)

(defn render-positions [this positions]
  (let [indexed (group-by (juxt :pos/row :pos/col) positions)]
    (dom/div nil
      (mapv
        (fn [row]
          (dom/div
            #js {:key row}
            (mapv
              (fn [col]
                (let [pos (first (indexed [row col]))]
                  (dom/span
                    #js {:key col
                         :onClick (fn [_] (om/transact! this [(list 'xom/mark pos)]))}
                    (name (:pos/mark pos :e)))))
              (range 0 3))))
        (range 0 3)))))

(defui ^:once HelloWorld
  static om/IQuery
  (query [this]
    [:xom/my-uid
     {:xom/game [:db/id
                 {:xom/positions [:db/id :pos/row :pos/col :pos/mark]}
                 :xom/winner]}])
  Object
  (render [this]
    (let [{:xom/keys [my-uid game]} (om/props this)]
      (dom/div nil
               (dom/div nil (str "me: " my-uid))
               (dom/pre nil (with-out-str (pprint game)))
               (render-positions this (:xom/positions game))
               ))))

(defmulti read (fn [env key params] key))

(defmethod read :default
  [{:keys [state] :as env} key params]
  (let [st @state]
    (if-let [[_ value] (find st key)]
      {:value value}
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
      (def chsk       chsk)
      (def ch-chsk    ch-recv)
      (def chsk-send! send-fn)
      (def chsk-state state))

    (sente/start-client-chsk-router!
            ch-chsk handle-ws)))

(defonce reconciler
  (om/reconciler
    {:state (atom {} #_{:message "hello world"})
     :parser (om/parser {:read read :mutate mutate})
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
  (om/add-root! reconciler
                  HelloWorld (gdom/getElement "app")))

(when (:open? @chsk-state)
  (om/add-root! reconciler
                HelloWorld (gdom/getElement "app"))) 

(defmethod handle-ws :chsk/recv
  [{:keys [event] :as e}]
  (println "received async " event)
  (om/merge! reconciler (second (second event))))

(defmethod handle-ws :default
  [e]
  (println (select-keys e [:id :event]))
  )

