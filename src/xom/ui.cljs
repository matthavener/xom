(ns xom.ui
  (:require
    [taoensso.sente :as sente]
    [goog.dom :as gdom]
    [om.next :as om :refer-macros [defui]]
    [om.dom :as dom]))

(enable-console-print!)

(defui HelloWorld
  static om/IQuery
  (query [this]
    '[:message])
  Object
  (render [this]
    (let [{:keys [message]} (om/props this)]
      (dom/div nil
        (dom/h2 nil message)))))

(defmulti read (fn [env key params] key))

(defmethod read :default
  [{:keys [state] :as env} key params]
  (let [st @state]
    (if-let [[_ value] (find st key)]
      {:value value}
      {:remote true})))

(defmulti handle-ws :id)

(defmethod handle-ws :default
  [e]
  (println (select-keys e [:id :event]))
  )

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
     :parser (om/parser {:read read})
     :send (fn [query cb]
             (println "sending " query)
             (chsk-send! [:xom/query query]
                         5000
                         (fn [r]
                           (println "received " r)
                           (cb r ))))
     }))

(defmethod handle-ws :chsk/handshake
  [e]
  (println "Adding root")
  (om/add-root! reconciler
                  HelloWorld (gdom/getElement "app")))
