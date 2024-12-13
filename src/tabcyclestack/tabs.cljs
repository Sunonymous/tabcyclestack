(ns tabcyclestack.tabs)

(defn new-tab [label]
  {:label label})

;; Utility functions for dealing with an ordered collection of tabs

(defn get-tab
  [tabs label]
  (first (filter #(= label (:label %)) tabs)))

(defn get-tab-index
  [tabs label]
  (some (fn [[idx tab]]
          (when (= label (:label tab))
            idx))
        (map-indexed vector tabs)))

(defn visible-tabs
  [tabs]
  (into [] (filter #(not (:hide-until %)) tabs)))

(defn remove-tab
  "This function is reused throughout events that need to remove a tab."
  [tabs labeled-as]
  (into []
        (filter #(not=  labeled-as (:label %)) tabs)))

(defn bump-tab
  [direction tabs label]
  (let [tab-index (get-tab-index tabs label)
        adjusted-index (case direction
                         :up            (dec tab-index)
                         :down          (inc tab-index)
                         :anything-else (inc tab-index)) ;; TODO am I remembering case wrong?
        highest-index (dec (count tabs))]
    ;; check to make sure it's a good idea
    (if (and tab-index
             (<= tab-index highest-index) ;; less than highest
             (>= adjusted-index 0)         ;; with a logical result
             (<= adjusted-index highest-index))
      (let [target   (nth tabs tab-index)
            neighbor (nth tabs adjusted-index)]
        (-> tabs
            (assoc adjusted-index target)
            (assoc tab-index neighbor)))
      tabs))) ;; otherwise give it back

(defn review-tabs
  [tabs]
  (into []
        (map (fn [t] (if (and (:hide-until t)
                              (< (:hide-until t) (.getTime (js/Date.))))
                       (dissoc t :hide-until)
                       t))
             tabs)))