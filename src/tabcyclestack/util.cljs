(ns tabcyclestack.util)

(defn rotate-vector [v]
  (vec (flatten [(rest v) (first v)])))

(defn move-to-end
  "Given a collection, a predicate to isolate elements, and a keyword either :front or :back,
   returns a vector of elements that match the predicate, grouped and placed at the designation
   side of the rest of the collection."
  [pos pred coll]
  (let [extracted-elements (filter pred coll)
        remaining-elements (remove pred coll)
        front (if (= pos :front)
                extracted-elements
                remaining-elements)
        back  (if (= pos :back)
                extracted-elements
                remaining-elements)]
    (into [] (flatten (cons front back)))))

(comment
  ;; works generally
  (move-to-end :front even? [1 2 3 4 5 6])
  (move-to-end :back even? [1 2 3 4 5 6])
  ;; works without predicate returning true
  (move-to-end :front even? [1 3 5 7 11])
  (move-to-end :back even? [1 3 5 7 11])
  ;; works when predicate is always true
  (move-to-end :front odd? [1 3 5 7 11])
  (move-to-end :back odd? [1 3 5 7 11])
  ;; works when predicate is true once
  (move-to-end :front even? [1 3 6 7 11])
  (move-to-end :back even? [1 3 6 7 11])
  (tomorrow)
  (let [start (js/Date.)
        delay (* 24 60 60 1000)
        finish (.getTime (js/Date. (+ (.getTime start) delay)))
        finish-2 (time-after-ms start delay)]
    (= finish finish-2))
  )

(defn tomorrow []
  (let [now (js/Date.)
        tomorrow (js/Date. (+ (.getTime now) (* 24 60 60 1000)))]
    (.getTime now)
    (.toString tomorrow)
    ))

(defn time-after-ms
  ([ms]
   (let [now (js/Date.)
         then (js/Date. (+ (.getTime now) ms))]
     (.getTime then)))
  ([start ms] ;; given a date object, starts from that point
   (let [finish (js/Date. (+ (.getTime start) ms))]
     (.getTime finish))))