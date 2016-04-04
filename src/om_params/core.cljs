(ns om-params.core  
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [goog.dom :as gdom]
            [cljs.core.async :as async :refer [<! >! put! chan]]
            [clojure.string :as string]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom])
  (:import [goog Uri]
           [goog.net Jsonp]))

(enable-console-print!)

(def base-url
  "http://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=")

(defn jsonp
  ([uri] (jsonp (chan) uri))
  ([c uri]
   (let [gjsonp (Jsonp. (Uri. uri))]
     (.send gjsonp nil #(put! c %))
     c)))

(defmulti read om/dispatch)

(defmethod read :search/results
  [{:keys [state ast] :as env} k {:keys [query]}]
  (merge
    {:value (get @state k [])}
    (when-not (or (string/blank? query)
                  (< (count query) 3))
      {:search ast})))

(defmethod read :root-query
  [{:keys [query target parser ast state] :as env} k params]
  (let [val (parser env query target)]
    (if (and (= target :search)
          (not (empty? val)))
      {:value (select-keys @state [:search/results])
       :search (om.next.impl.parser/expr->ast (first val))}
      {:value val})))

(defn send-to-chan [c]
  (fn [{:keys [search]} cb]
    (when search
      (let [{[search] :children} (om/query->ast search)
            query (get-in search [:params :query])]
        (put! c [query cb])))))

(defn search-loop [c]
  (go
    (loop [[query cb] (<! c)]
      (let [[_ results] (<! (jsonp (str base-url query)))]
        (cb {:search/results results}))
      (recur (<! c)))))
(defn result-list [results]
  (dom/ul #js {:key "result-list"}
    (map-indexed #(dom/li #js {:key %} %2) results)))

(defn search-field [ac query]
  (dom/input
    #js {:key "search-field"
         :value query
         :onChange
         (fn [e]
           (om/set-query! ac
             {:params {:query (.. e -target -value)}}))}))

(defui AutoCompleter
  static om/IQueryParams
  (params [_]
    {:query ""})
  static om/IQuery
  (query [_]
    '[(:search/results {:query ?query})])
  Object
  (render [this]
    (let [{:keys [search/results]} (om/props this)]
      (dom/div nil
        (dom/h2 nil "Autocompleter")
        (cond->
          [(search-field this (:query (om/get-params this)))]
          (not (empty? results)) (conj (result-list results)))))))

(def auto-completer (om/factory AutoCompleter))

(defui Root
  static om/IQuery
  (query [_]
    [{:root-query (om/get-query AutoCompleter)}])
  Object
  (render [this]
    (let [{:keys [root-query]} (om/props this)]
      (dom/div nil
               (auto-completer root-query)))))

(def send-chan (chan))

(def reconciler
  (om/reconciler
    {:state   {:search/results []}
     :parser  (om/parser {:read read})
     :send    (send-to-chan send-chan)
     :remotes [:remote :search]}))

(search-loop send-chan)

(om/add-root! reconciler Root
  (gdom/getElement "app"))
