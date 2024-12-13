(ns tabcyclestack.core
  (:require
   [re-pressed.core      :as rp]
   [reagent.dom          :as rdom]
   [tabcyclestack.views  :as views]
   [tabcyclestack.config :as config]
   [tabcyclestack.events :as events]
   [re-frame.core        :as re-frame]
   ))


(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/app] root-el)))

(defn init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (re-frame/dispatch-sync [::events/review-all-tabs])
  ;; (re-frame/dispatch-sync [::rp/add-keyboard-event-listener "keydown"])
  (dev-setup)
  (mount-root))
