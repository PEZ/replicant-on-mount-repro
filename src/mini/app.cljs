(ns mini.app
  (:require [clojure.walk :as walk]
            [clojure.core.match :refer [match]]
            [gadget.inspector :as inspector]
            [replicant.dom :as r]))

(defonce ^:private !db (atom {:ui/banner-text "An annoying banner"}))

(defn banner-view [{:ui/keys [banner-text]}]
  [:div#banner {:style {:top 0
                        :transition "top 0.25s"}
                :replicant/mounting {:style {:top "-100px"}}
                :replicant/unmounting {:style {:top "-100px"}}}
   [:p banner-text]
   [:button {:on {:click [[:ui/ax-dismiss-banner]]}} "Dismiss"]])

(defn- edit-view []
  [:form {:on {:submit [[:dom/ax-prevent-default]
                        [:db/ax-assoc :something/saved [:db/get :something/draft]]]}}
   [:input#draft {:replicant/on-mount [[:db/ax-assoc :something/draft-input-element :dom/node]]
                  :on {:input [[:db/ax-assoc :something/draft :event/target.value]]}}]
   [:button {:type :submit} "Save draft"]
   [:button {:on {:click [[:db/ax-assoc :something/draft ""]
                          [:dom/ax-set-text [:db/get :something/draft-input-element] ""]
                          [:dom/ax-focus-element [:db/get :something/draft-input-element]]]}} "Clear draft"]])

(defn- display-view [{:something/keys [draft saved dom-node]}]
  [:div
   [:h2 "On display"]
   [:ul
    [:li {:replicant/key "draft"} "Draft: " draft]
    [:li {:replicant/key "saved"} "Saved: " saved]
    (when dom-node
      [:li {:replicant/key "dom-node"} "ID of something dom-node: " [:code (.-id dom-node)]])]])

(defn- something-view []
  [:div#something-something {:replicant/on-mount [[:something/ax-init-something :dom/node]]}
   [:p "Something, something"]])

(defn- main-view [state]
  [:div {:style {:position "relative"}}
   (when (:ui/banner-text state)
     (banner-view state))
   [:h1 "A tiny Replicant example"]
   (edit-view)
   (display-view state)
   (something-view)])

(defn- enrich-action-from-event [{:replicant/keys [js-event node]} actions]
  (letfn [(process [x]
            (cond
              (keyword? x)
              (case x
                :event/target.value (-> js-event .-target .-value)
                :dom/node node
                x)

              (coll? x) (into (empty x) (map process x))

              :else x))]
    (process actions)))

(defn- enrich-action-from-state [state action]
  (walk/postwalk
   (fn [x]
     (cond
       (and (vector? x)
            (= :db/get (first x))) (get state (second x))
       :else x))
   action))

(defn handle-action [state action]
  (js/console.debug "ax:" (str (first action)) action)
  (prn "Triggered action" action)
  (match action
    [:dom/ax-prevent-default]
    {:effects [[:dom/fx-prevent-default]]}

    [:something/ax-init-something element]
    (do
      (js/console.debug "Init something, dom-node:" element)
      {:new-state (assoc state :something/dom-node element)})

    [:db/ax-assoc & kvs]
    {:new-state (apply assoc state kvs)}

    [:ui/ax-dismiss-banner]
    {:new-state (dissoc state :ui/banner-text)}

    [:dom/ax-set-text element text]
    {:effects [[:dom/fx-set-text element text]]}

    [:dom/ax-focus-element element]
    {:effects [[:dom/fx-focus-element element]]}))

(defn- perform-effects! [action-name e effects]
  (js/console.debug "fxs:" (clj->js [(str action-name) "->" (mapv (comp str first) effects)]))
  (doseq [effect effects]
    (js/console.debug "fx" (str (first effect)) effect)
    (try
      (match effect
        [:dom/fx-prevent-default]
        (.preventDefault e)

        [:dom/fx-set-text element text]
        (when element
          (set! (.-value element) text))

        [:dom/fx-focus-element element]
        (when element
          (.focus element)))
      (catch :default e
        (js/console.error "Error performing effect:" effect e)))))

(defn event-handler [!state]
  (fn [{:replicant/keys [js-event] :as replicant-data} actions]
    (when (seq actions)
      (try
        (loop [state @!state
               [action & remaining-actions] (remove nil? actions)]
          (let [enriched (->> action
                              (enrich-action-from-event replicant-data)
                              (enrich-action-from-state state))]
            (try
              (let [{:keys [new-state effects]} (handle-action state enriched)]
                (when new-state
                  (reset! !state new-state))
                (perform-effects! (first enriched) js-event (remove nil? effects)))
              (catch :default e
                (js/console.error "Error handling action:" action e)))
            (when remaining-actions
              (recur @!state remaining-actions))))
        (catch :default e
          (js/console.error "Error in event-handler" e))))))

(defn- render! [state]
  (r/render
   (js/document.getElementById "app")
   (#'main-view state)))

(defn ^{:dev/after-load true :export true} start! []
  (render! @!db))

(defn ^:export init! []
  (inspector/inspect "App state" !db)
  (r/set-dispatch! (#'event-handler !db))
  (add-watch !db :render (fn [_k _r o n]
                           (when (not= o n)
                             (render! @!db))))
  (start!))