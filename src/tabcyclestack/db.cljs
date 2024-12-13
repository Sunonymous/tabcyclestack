(ns tabcyclestack.db
  (:require [clojure.spec.alpha      :as s]
            [tabcyclestack.constants :refer [constants]]
            [tabcyclestack.tabs      :refer [new-tab]]))

;; Specs

; Tab Components
(s/def ::label string?)
;; Tab Composite
(s/def ::tab (s/keys :req-un [::label]))
;; DB Components
(s/def ::stack string?)
(s/def ::tabs (s/map-of ::stack (s/coll-of ::tab)))

;; TODO include this as a set of either :edit or :cycle ... this way seems to cause issues
;; (s/def ::mode (s/or :edit :cycle))
(s/def ::db (s/keys :req-un [;; ::mode
                             ::tabs ::selected-tabs
                             ::selected-stack
                             ::proposed-name ::proposed-stack
                             ::cycled-stacks
                             ::cycle-presets
                             ::cycle-index ::tab-index
                            ]))

(def sample-db
  {:mode           :edit
   :selected-tabs  nil
   :selected-stack nil
   :proposed-name  nil
   :proposed-stack ""
   :cycled-stacks  []
   :cycle-index    0
   :tab-index      0
   :cycle-presets  {}
   :tabs {"Try Me!" [(new-tab "Add a new tab")
                     (new-tab "Delete a tab")
                     (new-tab "Jump a tab between Stacks")
                     (new-tab "Copy a tab")
                     (new-tab "Edit a tab")
                    ]
          "Work" [(new-tab "Follow up about job")
                  (new-tab "Discuss Schedule")
                  (new-tab "Plan Route")
                  (new-tab "Enjoy New Challenges")
                 ]
          "Refresh" [(new-tab "Wash face")
                     (new-tab "Qi qong")
                     (new-tab "Deep breathing")
                     (new-tab "ASMR")
                     (new-tab "Shake")
                    ]
          "Daily Reset" [(new-tab "Bed made?")
                         (new-tab "Dishes done?")
                         (new-tab "Teeth brushed?")
                         (new-tab "Vent closed?")
                        ]
          "Home Duties" [
                  (new-tab "Clean Boots")
                  (new-tab "Open/Close Vent")
                  (new-tab "Check Mail")
                 ]
          "Cleaning" [
                      (new-tab "Vacuum")
                      (new-tab "Mop")
                      (new-tab "Sweep")
                     ]
          "Quick Tasks" [
                         (new-tab "Clean Ceiling Vent")
                         (new-tab "Cut Fingernails")
                        ]
          "Time Sensitive" [
                            (new-tab "Contact Progressive")
                            (new-tab "Send Christmas Cards")
                           ]
          "Length Test Tab" [
                            (new-tab "Tab 1")
                            (new-tab "Tab 2")
                            (new-tab "Tab 3")
                            (new-tab "Tab 4")
                            (new-tab "Tab 5")
                            (new-tab "Tab 6")
                            (new-tab "Tab 7")
                            (new-tab "Tab 8")
                            (new-tab "Tab 9")
                            (new-tab "Tab 10")
                            (new-tab "Tab 11")
                            (new-tab "Tab 12")
                            (new-tab "Tab 13")
                            (new-tab "Tab 14")
                            (new-tab "Tab 15")
                            (new-tab "Tab 16")
                            (new-tab "Tab 17")
                            (new-tab "Tab 18")
                            (new-tab "Tab 19")
                            (new-tab "Tab 20")
                            ]
          }})

(def default-db
  {:mode           :edit ;; :edit | :cycle
   :selected-tabs  nil ;; nil | [tab-address]
   :selected-stack nil ;; nil | stack
   :proposed-name  nil ;; used when selecting new tab to update renaming box
   :proposed-stack ""  ;; used when selecting new stack to move tabs
   :pinned-tabs    #{} ;; set of tab addresses
   :cycled-stacks  []  ;; list of tab addresses ; used for cycle mode
   :cycle-index    0   ;; index into cycled-stacks
   :tab-index      0   ;; index into stack at cycle-index
   :cycle-presets  {}  ;; map of preset collections of stacks for cycle mode
   :tabs {"Try Me!" [(new-tab "Add a new tab")
                     (new-tab "Delete a tab")
                     (new-tab "Swap a tab between Stacks")
                     (new-tab "Edit a tab")
                     (new-tab "Hide a tab")
                    ]
          }})
