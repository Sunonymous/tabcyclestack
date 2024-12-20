(ns tabcyclestack.subs
  (:require
   [tabcyclestack.constants :refer [constants]]
   [tabcyclestack.tabs      :refer [get-tab get-tab-index visible-tabs]]
   [re-frame.core           :as     re-frame]
   [clojure.string          :as     string]))

(re-frame/reg-sub
 ::re-pressed-example
 (fn [db _]
   (:re-pressed-example db)))

(re-frame/reg-sub
 ::requested-value
 (fn [db _]
   (:requested-value db)))

(re-frame/reg-sub
 ::mode
 (fn [db]
   (:mode db)))

;; Since tasks are keyed by name (task-str), each name must be unique. This function returns a set of all
;; the task names in the database, to query against and disallow duplicates.
(re-frame/reg-sub
  ::unavailable-tab-labels
 (fn [db]
   (let [tabs (flatten (vals (:tabs db)))
         names (set (map :label tabs))]
     names)))

(re-frame/reg-sub
 ::valid-tab-label?
 (fn [query-name]
   [(re-frame/subscribe [::unavailable-tab-labels])])
 (fn [[unavailable-names] [_ query-name]]
   (and (> (count (.trim query-name)) 0)
        (not (some #{query-name} unavailable-names)))))

(re-frame/reg-sub
 ::indexed-tab
 (fn [db [_ stack index]]
   (nth (get-in db [:tabs stack]) index))
)

(re-frame/reg-sub
 ::all-tabs
 (fn [db]
   (->> db
        :tabs
        (mapcat (fn [[_ tabs]] tabs))
        (clj->js))))

(re-frame/reg-sub
 ::starred-tab-addresses
 (fn [db]
   (:starred-tabs db)))

(re-frame/reg-sub
 ::starred-tab-objs
 (fn [db]
  ;;  (into [] (map (fn [[stack label]] (get-tab (get-in db [:tabs stack]) label)) (:starred-tabs db)))
   (into []
         (map (fn [[stack label]] (get-tab (get-in db [:tabs stack]) label))
              (:starred-tabs db)))))

(re-frame/reg-sub
 ::tab
 (fn [db [_ tab-address]]
   (let [[stack label] tab-address]
     (some #(when (= label (:label %)) %) (-> db :tabs stack)))))

(re-frame/reg-sub
 ::tabs-in-stack
 (fn [db [_ stack]]
   (get-in db [:tabs stack])))

(re-frame/reg-sub
 ::visible-tabs-in-stack
 (fn [db [_ stack]]
   (visible-tabs (get-in db [:tabs stack]))))

(re-frame/reg-sub
 ::cycled-tabs
 (fn [db]
   (into [] (mapcat (fn [stack] (visible-tabs (get-in db [:tabs stack]))) (:cycled-stacks db))))
)

(re-frame/reg-sub
 ::tab-source
 (fn [db _]
   (:tab-source db)))

(re-frame/reg-sub
 ::tab-stack-index
 (fn [db [_ [stack label]]]
   (get-tab-index (get-in db [:tabs stack]) label)))

(re-frame/reg-sub
 ::stack-size
 (fn [db [_ stack]]
   (count (get-in db [:tabs stack]))))

(re-frame/reg-sub
 ::selected-tabs
 (fn [db]
   (db :selected-tabs)))

(re-frame/reg-sub
 ::multiple-tabs-selected?
 (fn [db]
   (> (count (db :selected-tabs)) 1)))

(re-frame/reg-sub
 ::selected-stack
 (fn [db]
   (db :selected-stack)))

(re-frame/reg-sub
 ::sort-stacks-by
 (fn [db]
   (db :sort-stacks-by)))

(re-frame/reg-sub
 ::stacks
 (fn [db]
   (keys (:tabs db))))

(re-frame/reg-sub
 ::proposed-name
 (fn [db]
   (:proposed-name db)))

(re-frame/reg-sub
 ::proposed-stack
 (fn [db]
   (:proposed-stack db)))

(re-frame/reg-sub
 ::proposed-action
 (fn [db]
   (:proposed-action db)))

(re-frame/reg-sub
 ::cycle-index
 (fn [db]
   (:cycle-index db)))

(re-frame/reg-sub
 ::tab-index
 (fn [db]
   (:tab-index db)))

(re-frame/reg-sub
 ::cycled-stacks
 (fn [db]
   (:cycled-stacks db)))

(re-frame/reg-sub
 ::stack-in-cycle?
 (fn [db [_ stack]]
   (some #{stack} (db :cycled-stacks))))

(re-frame/reg-sub
 ::cycle-presets
 (fn [db]
   (:cycle-presets db)))

;; delete if you forgot about this
;; (re-frame/reg-sub
;;  ::cycle-preset
;;  (fn [db [_ preset-name]]
;;    (-> db :cycle-presets preset-name)))