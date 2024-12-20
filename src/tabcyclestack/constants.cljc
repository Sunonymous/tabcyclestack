(ns tabcyclestack.constants)

(def constants
  {:default-stack "Try Me!"
   :modes         #{:edit :cycle}
   :ms-in-sec      1000
   :ms-in-min      (* 60 (:ms-in-sec constants))
   :local-db-only '(:proposed-action)})

(def ms-values
  {"sec"  1000
   "min"  (* 60 1000)
   "hour" (* 60 60 1000)
   "day"  (* 24 60 60 1000)
   "week" (* 7 24 60 60 1000)})