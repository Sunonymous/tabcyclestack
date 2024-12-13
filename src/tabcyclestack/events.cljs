(ns tabcyclestack.events
  (:require
   [clojure.spec.alpha      :as s]
   [tabcyclestack.db        :as db]
   [re-frame.core           :as re-frame]
   [cljs.reader             :refer [read-string]]
   [tabcyclestack.util      :refer [rotate-vector move-to-end]]
   [tabcyclestack.tabs      :refer [new-tab get-tab remove-tab bump-tab review-tabs]]
   [tabcyclestack.constants :refer [constants]]
   [day8.re-frame.tracing   :refer-macros [fn-traced]]
   [clojure.set             :as set]
   ))

;; DB Validation

(defn check-and-throw
  [spec db]
  (when-not (s/valid? spec db)
    (throw (ex-info (str "DB Change Failure: " (s/explain-str spec db)) {}))))

(def check-db-change
  (re-frame/after (partial check-and-throw ::db/db)))

;; Initialization

(def local-storage-key "tabcyclestack.db")

(defn db->local-storage
  [db]
  (let [json (pr-str db)]
    (js/localStorage.setItem local-storage-key json)))

(def save-db-local
  (re-frame/after db->local-storage))

(re-frame/reg-cofx
 ::local-store
 (fn [cofx storage-key]
   (assoc cofx :local-store (js->clj (.getItem js/localStorage storage-key)))))

(re-frame/reg-event-db
 ::reset-views
 (fn [db _]
   (re-frame/dispatch [::set-cycle-index 0])
   (re-frame/dispatch [::set-tab-index 0])
   (re-frame/dispatch [::set-mode :edit])))

(re-frame/reg-event-fx
 ::initialize-db
 [(re-frame/inject-cofx ::local-store local-storage-key)]
 (fn
   [cofx _]
   (let [persisted-db (read-string (:local-store cofx))
         valid-db? (and persisted-db (s/valid? ::db/db persisted-db))
         next-db (if valid-db? persisted-db db/default-db)]
     {:db next-db})))

(re-frame/reg-event-db
 ::toggle-mode
 (fn [db _]
   ;; TODO potentially reset cycling indexes here
   (update-in db [:mode] #(if (= % :edit) :cycle :edit))))
;; sometimes it may be useful to set directly
(re-frame/reg-event-db
 ::set-mode
 (fn [db [_ next-mode]]
   (if (some (constants :modes) next-mode)
     (assoc db :mode next-mode)
     db)))

(re-frame/reg-event-db
 ::rotate-tabs
 (fn [db [_ stack]]
   (update-in db [:tabs stack] rotate-vector)))

(re-frame/reg-event-db
 ::deselect-tab
 (fn [db _]
   (-> db
       (assoc :selected-tabs nil)
       (assoc :proposed-name nil))))

(re-frame/reg-event-db
 ::deselect-stack
 (fn [db _]
   (-> db
       (assoc :selected-stack nil)
       (assoc :selected-tabs nil))))

(re-frame/reg-event-db
 ::add-tab ;; uses tab address
 [check-db-change
  save-db-local]
 (fn [db [_ [stack label]]]
   (update-in db [:tabs stack] #(if (nil? %)
                                  [(new-tab label)]
                                  (conj % (new-tab label))))))

(re-frame/reg-event-db
 ::remove-tab
 [check-db-change
  save-db-local]
 (fn [db [_ tab-address]]
   (let [[stack label] tab-address]
     (re-frame/dispatch [::deselect-tab])
     (re-frame/dispatch [::unpin-tab tab-address])
     (update-in db [:tabs stack] #(remove-tab % label)))))

(re-frame/reg-event-db
 ::remove-stack
 [check-db-change
  save-db-local]
 (fn [db [_ stack]]
   (re-frame/dispatch [::deselect-stack])
   (re-frame/dispatch [::remove-stack-from-cycle stack])
   (re-frame/dispatch [::remove-stack-from-presets stack])
   (update-in db [:tabs] #(dissoc % stack))))

(re-frame/reg-event-db
 ::rename-tab
 [check-db-change
  save-db-local]
 (fn [db [_ tab-address new-name]]
   (let [[stack label] tab-address]
     (when (and new-name (> (count (.trim new-name)) 0))
       (update-in db [:tabs stack]
                  (fn [tabs] (into []
                                    (map #(if (= label (:label %))
                                            (assoc % :label new-name)
                                            %) tabs))))))))

(re-frame/reg-event-db
 ::move-tab ;; uses tab address
 [check-db-change
  save-db-local]
 (fn [db [_ [stack label] destination-stack]]
   (let [tab-to-move (get-tab (get-in db [:tabs stack]) label)]
     (when tab-to-move
       (-> db
           (update-in [:tabs destination-stack] conj tab-to-move)
           (update-in [:tabs stack] #(remove-tab % label)))))))

(re-frame/reg-event-db
 ::bump-tab ;; uses tab address
 (fn [db [_ tab-address direction]]
   (let [[stack label] tab-address]
     (update-in db [:tabs stack] #(bump-tab direction % label)))))

(re-frame/reg-event-db
 ::move-tab-to-top
 (fn [db [_ tab-address]]
   (let [[stack label] tab-address]
     (update-in db [:tabs stack]
                (fn [tabs] (move-to-end :front #(= label (:label %)) tabs))))))

(re-frame/reg-event-db
 ::move-tab-to-bottom
 (fn [db [_ tab-address]]
   (let [[stack label] tab-address]
     (update-in db [:tabs stack]
                (fn [tabs] (move-to-end :back #(= label (:label %)) tabs))))))

(re-frame/reg-event-db
 ::select-tabs
 (fn [db [_ tab-addresses]]
   (assoc db :selected-tabs tab-addresses)))

(re-frame/reg-event-db
 ::select-stack
 (fn [db [_ stack-to-select]]
   (-> db
       (assoc  :selected-stack stack-to-select)
       (assoc  :proposed-stack stack-to-select))))

(re-frame/reg-event-db
 ::propose-name
 (fn [db [_ next-name]]
     (assoc db :proposed-name next-name)))

(re-frame/reg-event-db
 ::propose-stack
 (fn [db [_ next-stack]]
     (assoc db :proposed-stack next-stack)))

;; Cycles

(re-frame/reg-event-db
 ::set-cycle-index
 (fn [db [_ next-index]]
     (assoc db :cycle-index next-index)))

(re-frame/reg-event-db
 ::inc-cycle-index
 (fn [db [_ stack-count]]
     (update db :cycle-index #(mod (inc %) stack-count))))

(re-frame/reg-event-db
 ::dec-cycle-index
 (fn [db [_ stack-count]]
     (update db :cycle-index #(mod (dec %) stack-count))))
;; TODO refactor into util function to update with
(re-frame/reg-event-db
 ::inc-tab-index
 (fn [db [_ tab-count]]
     (update db :tab-index #(mod (inc %) tab-count))))

(re-frame/reg-event-db
 ::dec-tab-index
 (fn [db [_ tab-count]]
     (update db :tab-index #(mod (dec %) tab-count))))

(re-frame/reg-event-db
 ::set-tab-index
 (fn [db [_ next-index]]
     (assoc db :tab-index next-index)))

(re-frame/reg-event-db
 ::add-stack-to-cycle
 (fn [db [_ stack-to-cycle]]
     ;; would be a set, though order serves purpose here
     (if (some #{stack-to-cycle} (db :cycled-stacks))
       db
       (update db :cycled-stacks #(into [] (conj % stack-to-cycle))))))

(re-frame/reg-event-db
 ::clear-cycled-stacks
 (fn [db _]
     (assoc db :cycled-stacks [])))

(re-frame/reg-event-db
 ::remove-stack-from-cycle
 (fn [db [_ stack-to-remove]]
     ;; would be a set, though order serves purpose here
   (update db :cycled-stacks #(into [] (filter (fn [stack] (not= stack-to-remove stack)) %)))))

(re-frame/reg-event-db
 ::add-cycle-preset
 [check-db-change
  save-db-local]
 (fn [db [_ name stacks]]
     (assoc-in db [:cycle-presets name] stacks)))

(re-frame/reg-event-db
 ::remove-cycle-preset
 [check-db-change]
 (fn [db [_ name]]
     (update db :cycle-presets dissoc name)))

(re-frame/reg-event-db
 ::load-cycle-preset
 (fn [db [_ name]]
     (assoc db :cycled-stacks (get-in db [:cycle-presets name]))))

(re-frame/reg-event-db
 ::remove-stack-from-presets
 [check-db-change]
 (fn [db [_ stack-to-remove]]
     (update db :cycle-presets (fn [presets] (into {} (filter (fn [[_name stacks]] (not (some #{stack-to-remove} stacks))) presets))))))
;; TODO what happens when we have deleted a stack and then load it in a preset??

(re-frame/reg-event-db
 ::hide-tab-until ;; TODO resolve ERROR!
 [check-db-change
  save-db-local]
 (fn [db [_ tab-address instant-in-ms]]
   (let [[stack label] tab-address]
     (update-in db [:tabs stack] (fn [tabs] (map #(if (= label (:label %)) (assoc % :hide-until instant-in-ms) %) tabs)) ))))

(re-frame/reg-event-db
 ::reappear-tab
 [check-db-change
  save-db-local]
 (fn [db [_ tab-address]]
   (let [[stack label] tab-address]
     (update-in db [:tabs stack] (fn [tabs] (map #(if (= label (:label %)) (dissoc % :hide-until) %) tabs)) ))))

(re-frame/reg-event-db
 ::review-stack ;; function checks all the hidden tabs and unhides them if appropriate
 [check-db-change]
 (fn [db [_ stack]]
   (update-in db [:tabs stack] review-tabs)))

(re-frame/reg-event-db
 ::review-all-tabs
 [check-db-change
  save-db-local]
 (fn [db _]
   (doseq [stack (keys (db :tabs))]
     (re-frame/dispatch [::review-stack stack]))
   db))

(re-frame/reg-event-db
 ::pin-tab
 [check-db-change
  save-db-local]
 (fn [db [_ tab-address]]
   (update db :pinned-tabs #(into #{} (conj % tab-address)))))

(re-frame/reg-event-db
 ::unpin-tab
 [check-db-change
  save-db-local]
 (fn [db [_ tab-address]]
   (update db :pinned-tabs disj tab-address)))