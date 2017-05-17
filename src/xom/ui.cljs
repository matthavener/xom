(ns xom.ui
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(enable-console-print!)

(defmulti read (fn [env key params] key))

(defmethod read :default
  [{:keys [state] :as env} key params]
  (let [st @state]
    (if-let [[_ value] (find st key)]
      {:value value}
      {:value :not-found})))

(defui HelloWorld
  static om/IQuery
  (query [this]
    '[:message])
  Object
  (render [this]
    (let [{:keys [message]} (om/props this)]
      (dom/div nil
        (dom/h2 nil message)))))

(defonce reconciler
  (om/reconciler
    {:state (atom {:message "hello world"})
     :parser (om/parser {:read read})}))

(om/add-root! reconciler
  HelloWorld (gdom/getElement "app"))
