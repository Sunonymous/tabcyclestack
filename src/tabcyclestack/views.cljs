(ns tabcyclestack.views
  (:require
   [reagent.core         :as r]
   [tabcyclestack.subs   :as subs]
   [tabcyclestack.events :as events]
   [re-frame.core        :as re-frame]
   [tabcyclestack.constants :refer [ms-values]]
))

;; So-called Roadmap:
;; TODO re-pressed was causing the warning! get rid of it
;; TODO code needs clean up after third iteration
;; OTHER  ;;
;; TODO visual indication of having seen all tabs in cycled stack
;; STACKS ;;
;; TODO favorite stacks
;; TODO add sorting method for stacks
;; sort by size, alpha
;; sort reverse button
;; TODO rename stack
;; CYCLES / PILES ;;
;; TODO visually indicate that a stack is empty
;; TODO rename to piles
;; TODO favorite piles
;; TABS   ;;
;; TODO pinned/star tabs
;; TODO rename tabs
;; TODO tab colors
;; TODO add tab sort buttons
;; TODO tab make visible button
;; TODO tab templates
;; LAYOUT
;; TODO determine and simplify shape and document structure
;; META
;; consider simple statistics system

(defn main-mode-display []
  (let [invert? (r/atom false)]
    (fn []
      (let [edit-mode? (= :edit @(re-frame/subscribe [::subs/mode]))
            style-active   {:font-weight     :bold
                            :text-decoration :underline}
            style-inactive {:font-weight :lighter
                            :text-decoration :none}]
        [:div#mode-toggle
         {:style {:font-family :sans-serif
                  :font-size :1rem :width :max-content
                  :cursor :pointer :user-select :none
                  :border-radius :8px
                  :padding :0.5em
                  :position :fixed :top :8em :right :9.5em}
          :on-click #(re-frame/dispatch [::events/toggle-mode])
          :on-mouse-enter #(reset! invert? true)
          :on-mouse-leave #(reset! invert? false)}
         [:span.mode-text.edit
          {:style (if edit-mode?
                    (if @invert? style-inactive style-active)
                    (if @invert? style-active style-inactive))}
          "Edit"]
         [:span "/"]
         [:span.mode-text.cycle
          {:style (if edit-mode?
                    (if @invert? style-active style-inactive)
                    (if @invert? style-inactive style-active))}
          "Cycle"]]))))

(defn shelf-toggle-button
  [name show-selector*]
  [:button.shelf-toggle-button
   {:on-click #(swap! show-selector* not)}
   (if @show-selector*
     (str "^")
     (str "v " name))])

(defn shelf-header [name]
  [:h3 {:style {:position :absolute :top :0 :left :50%
                :transform "translateX(-50%)"
                :text-transform :uppercase
                :align-self :center
                :font-family :sans-serif
                :color "rgba(0, 0, 0, 8.5)"}}
       name])

(defn stack-selector
  []
  (let [stacks @(re-frame/subscribe [::subs/stacks])
        selected-stack @(re-frame/subscribe [::subs/selected-stack])]
    [:div.selector
     {:style {:margin-block :0.25em
              :min-width :max-content
              :padding-block :0.75em
              :display :flex :flex-direction :column
              :justify-content :center
              :gap :0.2em
              ;; TODO dotted border if stack is empty?
              :border  (if selected-stack "3px solid black" "3px solid transparent")
              :font-family :sans-serif
              :font-size :1.5rem}}
     (doall
      (for [stack (sort stacks)]
        (let [stack-is-empty (= 0 @(re-frame/subscribe [::subs/stack-size stack]))]
          ^{:key stack}
          [:div {:style {:width :100%
                         :border (if (= stack selected-stack)
                                   "none" ;; this weird border was to assist the slight width-change
                                   "0px solid transparent") ;; with bolder fonts. there's probably a better way
                         :display :flex :flex-direction :column}}
           [:button.empty
            {:style {:padding-inline :0.5em}
             :disabled (or (= stack selected-stack)
                           (= :star @(re-frame/subscribe [::subs/tab-source])))
             :on-click (fn [_] (re-frame/dispatch [::events/select-stack stack])
                         (re-frame/dispatch [::events/deselect-tab]))}
            stack]
           (when (and selected-stack
                      stack-is-empty
                      (= stack selected-stack))
             [:button.delete-button
              {:style {:font-size :1rem :color :red}
               :on-click #(re-frame/dispatch [::events/remove-stack selected-stack])}
              "Delete"])])))]))

(defn stack-organize-buttons []
  (let [tab-addresses        @(re-frame/subscribe [::subs/selected-tabs])
        tab-is-selected       (boolean (seq tab-addresses))
        first-selected-tab    (first tab-addresses)
        selected-index       @(re-frame/subscribe [::subs/tab-stack-index first-selected-tab])
        stack-size           @(re-frame/subscribe [::subs/stack-size (first first-selected-tab)])
        selected-top          (= 0 selected-index)
        selected-bottom       (= (dec stack-size) selected-index)
        selected-everything   (= stack-size (count tab-addresses))]
    [:div ;; TODO maybe more logical button behavior: if multiple are selected, move down option is only available if the highest index can still increment
     {:style {:min-height "2em"
              :margin-right "0.4em"
              :padding "0.4em"
              :padding-top 0
              :display :flex, :flex-direction :column
              :justify-content :center
              :gap :0.6em}}
     [:button
      {:style {:visibility (if (and tab-is-selected (not selected-top) (not selected-everything))
                             :visible :hidden)}
       :on-click (fn [_] (doseq [address tab-addresses]
                           (re-frame/dispatch [::events/move-tab-to-top address])))}
      "Top"]
     [:button
      {:style {:visibility (if (and tab-is-selected (not selected-everything))
                             :visible :hidden)}
       :on-click (fn [_] (doseq [address tab-addresses]
                           (re-frame/dispatch [::events/bump-tab address :up])))}
      "Up"]
     [:button
      {:style {:visibility (if (and tab-is-selected (not selected-everything))
                             :visible :hidden)}
       :on-click (fn [_] (doseq [address (reverse tab-addresses)] ;; doing this with natural order is ineffective for neighboring tasks
                           (re-frame/dispatch [::events/bump-tab address :down])))}
      "Down"]
     [:button
      {:style {:visibility (if (and tab-is-selected (not selected-bottom) (not selected-everything))
                             :visible :hidden)}
       :on-click (fn [_] (doseq [address tab-addresses]
                           (re-frame/dispatch [::events/move-tab-to-bottom address])))}
      "Bottom"]]))

;; TODO causes mysterious subscription warning in console, and potential memory leak
(defn tab-creator []
  (let [label-entry (r/atom "")
        stack-entry (r/atom "")]
    (fn []
      (let [valid-label?   @(re-frame/subscribe [::subs/valid-tab-label? @label-entry])
            selected-stack @(re-frame/subscribe [::subs/selected-stack])]
        [:div#tab-creator
         {:style {:height :fit-content
                  :align-self  :center  :display :flex
                  :margin-inline :1em :margin-top :0.5em}}
         [:span {:style {:margin-right "0.4em" :font-weight :bold :align-self :center}}
          "Add "]
         [:input#label-input
          {:value @label-entry
           :placeholder "Tab Label"
           :on-change #(reset! label-entry (-> % .-target .-value))}]
         [:p {:style {:margin-inline "4px"
                      :align-self :center}}
          " in stack "]
         [:input#stack-input ;; TODO add submit on enter
          ;; TODO add autocomplete based on stack names
          {:value @stack-entry
           :max-length 18 ;; implicit stack name length limit
           :placeholder selected-stack
           :on-key-down (fn [e] (js/console.log "key: " (.-key e)) ;; TODO why don't these run?
                         (when (= "Enter" (.-key e))
                                 (.click (js/document.getElementById "add-tab-button"))))
           :on-change #(reset! stack-entry (-> % .-target .-value))}]

         [:button#add-tab-button
          {:disabled (and (nil? selected-stack) (= "" @stack-entry))
           :on-click (fn [_]
                       (when valid-label?
                         (re-frame/dispatch [::events/add-tab [(if (= "" @stack-entry)
                                                                 selected-stack
                                                                 @stack-entry)
                                                               (.trim @label-entry)]])
                         (when (not= "" @stack-entry)
                           (re-frame/dispatch [::events/select-stack @stack-entry]))
                         (reset! label-entry "")
                         (reset! stack-entry "")
                         (.focus (.getElementById js/document "label-input"))))}
          "Add Tab"]]))))

(defn action-description-text
  "This component reads the local state, determines the user's imminent action,
   and displays it in a friendly text form to ensure the user understands their choice."
  []
  (let [selected-tabs  @(re-frame/subscribe [::subs/selected-tabs])
        proposed-name  @(re-frame/subscribe [::subs/proposed-name])
        proposed-stack @(re-frame/subscribe [::subs/proposed-stack])
        [src-stack src-label] (first selected-tabs)
        stack-differs (not= src-stack proposed-stack)
        label-differs (not= src-label proposed-name)
        multiple-selected (< 1 (count selected-tabs))]

    [:span
     {:style {:font-style :italic :font-size :1.5rem}}
     (cond ;; TODO improve ability to style text by section
       (and multiple-selected stack-differs)
       (str "Moving " (count selected-tabs) " tabs from " src-stack " to " proposed-stack ".")

       (and (not multiple-selected) stack-differs label-differs)
       (str "Renaming '" src-label "' to '" proposed-name "' and moving it to " proposed-stack ".")

       (and (not label-differs) (not multiple-selected) stack-differs)
       (str "Moving '" src-label "' from " src-stack " to " proposed-stack ".")

       (and (not multiple-selected) (not stack-differs) label-differs)
       (str "Renaming '" src-label "' to '" proposed-name "'.")

       :else "")])) ;; no action is happening

(defn floating-tab-header ; Used to indicate which modal is open.
  [text css] ;; attach style map
  [:span.floating-tab-header
   {:style css}
   text])

(defn tab-editor []
  (let [stacks     @(re-frame/subscribe [::subs/stacks])]
    (fn []
      (let [reset-form!          #(re-frame/dispatch [::events/propose-name ""])
            proposed-name        @(re-frame/subscribe [::subs/proposed-name])
            proposed-stack       @(re-frame/subscribe [::subs/proposed-stack])
            selected-tabs        @(re-frame/subscribe [::subs/selected-tabs])
            [src-stack src-label] (first selected-tabs)
            multiple-selected?    (> (count selected-tabs) 1)
            changed-label?        (not= src-label proposed-name)
            changed-stack?        (not= src-stack proposed-stack)
            valid-label           (> (count (.trim (or proposed-name ""))) 0)]

        [:section#tab-editor
         [:div
          {:style {:align-self  :center  :display :flex
                   :flex-grow   1        :margin-top "0.5em"
                   :margin-left :0.5em}}
          [:span {:style {:margin-inline "0.4em" :font-weight :bold :align-self :center}}
           "Edit "]
          [:input#relabel-input
           {:value       proposed-name
            :disabled    multiple-selected?
            :placeholder "Tab Label"
            :on-change   #(re-frame/dispatch [::events/propose-name (-> % .-target .-value)])}]
          [:p {:style {:margin-inline "4px"
                       :align-self :center}}
           " in stack "]
          [:select ;; TODO doesn't make sense if only one option available
           {:value proposed-stack
            :on-change #(re-frame/dispatch [::events/propose-stack (-> % .-target .-value)])}
           (for [stack stacks]
             ^{:key stack}
             [:option {:value stack} stack])]]
         [:div
          {:style {:margin-top "0.5em"
                   :display :flex :justify-content :center
                   :align-items :center :gap :0.4em}}
          [action-description-text]
          [:button
           {:style {:visibility (if (or (and (seq proposed-name) changed-label? (not multiple-selected?)) changed-stack?)
                                  :visible
                                  :hidden)}
            :disabled (not valid-label)
            :on-click (fn [_]
                      ;; when stack is different, change its position with the new name
                        (when changed-stack?
                          (doseq [address selected-tabs]
                            (re-frame/dispatch [::events/move-tab address proposed-stack])))
                      ;; when only one is selected and label is different, change it
                        (when (and (not multiple-selected?) changed-label?)
                          (if changed-stack?
                            (re-frame/dispatch [::events/rename-tab [proposed-stack (-> selected-tabs first second)] proposed-name])
                            (re-frame/dispatch [::events/rename-tab (first selected-tabs) proposed-name])))
                        (reset-form!)
                        (re-frame/dispatch [::events/deselect-tab]))}
           "Make it So"]]]))))

(defn tab-selector
  []
  (let
   [;; because the select is uncontrolled, we reset it manually.
    deselect-all! (fn [] (doseq [element (js/document.querySelectorAll ".tab-choice")]
                           (set! (.-selected element) false)))
    selected-stack @(re-frame/subscribe [::subs/selected-stack])
    tabs-in-stack  @(re-frame/subscribe (if (= "Pinned Tabs" selected-stack)
                                          [::subs/pinned-tab-objs]
                                          [::subs/tabs-in-stack selected-stack]))
    selected-tabs  @(re-frame/subscribe [::subs/selected-tabs])
    pinned-tabs    @(re-frame/subscribe [::subs/pinned-tabs])
    first-is-pinned (some #{(first selected-tabs)} pinned-tabs)]
      [:div
       {:style {:flex-grow       1
                :margin-block    :auto
                :margin-left     0
                :padding-right   :2em
                :display         :flex
                :flex-direction  :column
                :justify-content :center}}

    (when (seq tabs-in-stack)
       [:select#tab-selector.custom.hide-scrollbar
        {:style     {:margin-left 0
                     :padding-left 0
                     :padding-right 0
                     :border-left "5px solid black"
                     :border-top (if selected-tabs "5px solid black" :none)
                     :border-bottom (if selected-tabs "5px solid black" :none)
                     }
         :size      10
         :multiple  true
         :on-change (fn [e]
                      (let [selected-elements      (-> e .-target .-selectedOptions)
                            addresses              (map (fn [em] (let [label (-> em .-value)
                                                                       stack (if first-is-pinned
                                                                               #{some (fn [[stack label-2]] (when (= label label-2) stack)) pinned-tabs}
                                                                               selected-stack)]
                                                                   [stack label])) selected-elements)
                            multiple-selected?     (> (count addresses) 1)
                            multiple-selected-text "(Multiple Selections)"]
                        (re-frame/dispatch [::events/select-tabs (when (seq addresses) addresses)])
                        (re-frame/dispatch [::events/propose-name (if multiple-selected?
                                                                    multiple-selected-text
                                                                    (-> addresses first second))])))}
        (doall
         (for [tab tabs-in-stack]
           (let [pinned? (some #{[selected-stack (:label tab)]} pinned-tabs)
                 border-color (if pinned? "white" "black")]
             [:option.tab-choice
              {:key (:label tab)
               :style {:margin-right 0
                       :margin-left 0
                       :border-top   (str "2px solid " border-color)
                       :border-right (str "2px solid " border-color)
                       :opacity (if (:hide-until tab) 0.5 1)
                       :color            (if pinned? "white" "black")
                       :background-color (cond
                                           (:hide-until tab) "rgba(0, 0, 0, 0.1)"
                                           pinned?           "black"

                                           :else :inherit)
                       }}
              (:label tab)])))])
       [:div
        {:style {:margin-bottom :1em :align-self :center
                 :visibility (if (seq selected-tabs) :visible :hidden)}}
        [:button
         {:on-click (fn [_]
                      (deselect-all!)
                      (re-frame/dispatch [::events/deselect-tab]))}
         "Deselect"]
        [:button
         {:on-click (fn [_]
                      (doseq [address selected-tabs]
                        (re-frame/dispatch [::events/remove-tab address]))
                      (deselect-all!))}
         "Delete"]
        [:button ;; TODO make this button visible only when tab is hidden
         {:on-click (fn [_]
                      (doseq [address selected-tabs]
                        (js/console.log "reappearing" address)
                        (re-frame/dispatch [::events/reappear-tab address]))
                      (deselect-all!))}
         "Reappear"]
          [:button
           {:on-click (fn [_]
                        (let [next-event (if first-is-pinned ::events/unpin-tab ::events/pin-tab)]
                          (doseq [address selected-tabs]
                            (re-frame/dispatch [next-event address]))))}
           (if first-is-pinned "Unpin" "Pin")]]
       (if selected-tabs
         [tab-editor]
         [tab-creator])]))

(defn cycle-stack-transfer-button [should-show]
  (let [cycled-stacks  @(re-frame/subscribe [::subs/cycled-stacks])
        selected-stack @(re-frame/subscribe [::subs/selected-stack])
        already-in-cycle (some #{selected-stack} cycled-stacks)]
    [:button
     {:style {:visibility (if (and selected-stack should-show (not already-in-cycle))
                            :visible
                            :hidden)}
      :on-click (fn [_]
                  (if already-in-cycle
                    (re-frame/dispatch [::events/remove-stack-from-cycle selected-stack])
                    (re-frame/dispatch [::events/add-stack-to-cycle      selected-stack]))
                  (re-frame/dispatch [::events/deselect-stack]))}
     "Add to Cycle >>"]))

(defn cycle-stacks-list
  [cycled-stacks]
  (let [cycle-index @(re-frame/subscribe [::subs/cycle-index])]
    (if (seq cycled-stacks)
      [:ul.cycle-stack-box
       (doall
        (for [[index stack] (map-indexed vector cycled-stacks)]
          ^{:key stack}
          [:li {:class (if (= index cycle-index)
                         "active-cycle-stack" "")
                :on-click (fn [_]
                            (re-frame/dispatch [::events/deselect-stack])
                            (re-frame/dispatch [::events/set-cycle-index index])
                            (re-frame/dispatch [::events/set-tab-index 0]))
                :style {:user-select :none}}
           stack]))]
      [:h3 {:style {}}
       "Add a Stack"])))

(defn clear-cycled-stacks-button
  []
  [:button
    {:on-click (fn [_]
                 (re-frame/dispatch [::events/clear-cycled-stacks])
                 (re-frame/dispatch [::events/set-cycle-index 0])
                 (re-frame/dispatch [::events/set-tab-index 0]))}
    "Clear"])

(defn cycle-stack-buttons
  [cycled-tabs stack-size]
  (let [cycle-index @(re-frame/subscribe [::subs/cycle-index])
        tab-index   @(re-frame/subscribe [::subs/tab-index])]
    [:div {:style {:display :flex :flex-direction :column :justify-content :space-evenly :gap :0.5em}}

     [:div {:style {:display :flex :justify-content :space-evenly :gap :0.5em}}
      [:button
       {:on-click (fn [_]
                    (re-frame/dispatch [::events/remove-stack-from-cycle (nth cycled-tabs cycle-index)])
                    (re-frame/dispatch [::events/dec-cycle-index (count cycled-tabs)])
                    (re-frame/dispatch [::events/set-tab-index 0]))}
       "Remove"]
      #_[:button ;; TODO make button functional
         {:on-click (fn [_])}
         "Up"]
      #_[:button ;; TODO make button functional
         {:on-click (fn [_])}
         "Down"]]]))

(defn cycle-tab-buttons [stack-address is-last-tab tab-count]
  (let [hide-menu-visible? (r/atom false)
        hide-quantity      (r/atom "")
        hide-step          (r/atom "min")]
    (fn [stack-address is-last-tab tab-count]
      (let [parsed-quantity (js/parseInt @hide-quantity)
            valid-quantity  (and (> parsed-quantity 0) (<= parsed-quantity 99))
            reappears-at    (when valid-quantity
                               (let [now    (.getTime (js/Date.))
                                     then   (+ now (* parsed-quantity (ms-values @hide-step)))]
                                 (js/Date. then)))]
        [:div
         {:style {:border (if (or @hide-menu-visible?) "1px solid black" :none)
                  :padding "0.25em 0.5em"}}
         [:div ;; buttons
          {:style {}}
          [:button
           {:on-click #(swap! hide-menu-visible? not)}
           (if @hide-menu-visible? "Cancel" "Hide")]]
         [:div ;; button menus
          [:form#hide-tab-menu
           {:style {:padding "0.25em 0.5em"
                    :visibility (if @hide-menu-visible? :visible :hidden)}}
           [:input {:type :number
                    :value @hide-quantity
                    :on-change #(reset! hide-quantity (-> % .-target .-value))
                    :min 1 :max 99
                    :style {:width :min-content}}]
           [:select
            {:value @hide-step
             :on-change #(reset! hide-step (-> % .-target .-value))}
            [:option {:value "min"}  "minutes"]
            [:option {:value "hour"} "hours"]
            [:option {:value "day"}  "days"]
            [:option {:value "week"} "weeks"]
            ]
           [:button
            {:type :button
             :on-click (fn [_] (re-frame/dispatch [::events/hide-tab-until stack-address (.getTime reappears-at)])
                         (reset! hide-quantity "")
                         (reset! hide-menu-visible? false)
                         (when is-last-tab (re-frame/dispatch [::events/dec-tab-index tab-count])))
             :disabled (or (not @hide-menu-visible?)
                           (not valid-quantity)
                           (not reappears-at))}
            "Hide Tab"] ;; TODO cycle component gets confused when last tab in sequence becomes hidden
           [:br]
           #_[:button
              {:type :button
               :on-click #(let [now (.getTime (js/Date.))
                                ten-seconds-later (+ now (* 1000 10))]
                            (re-frame/dispatch [::events/hide-tab-until stack-address ten-seconds-later]))}
              "Ten seconds"]
         ;; TODO create another display string for what the check will do,
         ;; eg. "Tab will reappear on Monday, December 10, 2024"
           (when reappears-at
             [:span "Tab will reappear on " (.toDateString reappears-at) " at " (.toLocaleTimeString reappears-at)])
           ]]
         ]))))

;; This component needs an overhaul. Manually cycling through multiple stacks is unnecessary.
;; Better to have a subscription which flatmaps all the stacks to the visible tabs they contain,
;; and then cycle through the primary list.
(defn cycle-view
  []
  (let [cycled-stacks @(re-frame/subscribe [::subs/cycled-stacks])
        cycle-index   @(re-frame/subscribe [::subs/cycle-index])
        tab-index     @(re-frame/subscribe [::subs/tab-index])
        in-stack       (get cycled-stacks cycle-index)
        tabs-in-stack @(re-frame/subscribe [::subs/visible-tabs-in-stack in-stack])
        ;; full-tabs     @(re-frame/subscribe [::subs/cycled-tabs]) ;; this is what could be used for scrolling through the full
        on-tab         (nth tabs-in-stack tab-index nil)            ;; series of tabs. the challenge is to display the stack dynamically
        on-address     (when on-tab [in-stack (:label on-tab)])
        stack-count    (count cycled-stacks)
        tab-count      (count tabs-in-stack)]
    [:section
     [:div
      {:style {:display :flex :justify-content :space-evenly
               :align-items :center :flex-grow 1
               :width :100% :height :100%
               :padding "0.5em"}}
      [:div {:style {:flex 1 :display :flex}}
       (when (> stack-count 1)
         [:button
          {:on-click (fn [_]
                       (re-frame/dispatch [::events/dec-cycle-index stack-count])
                       (re-frame/dispatch [::events/set-tab-index 0]))}
          "<<"])
       (when (> tab-count 1)
         [:button
          {:on-click #(re-frame/dispatch [::events/dec-tab-index tab-count])}
          "Prev Tab"])]
      [:form {:style {:flex 2 :user-select :none}}
       [:fieldset {:style {:position :relative
                           :border-radius :8px
                           :visibility (if on-tab :visible :hidden)}}
        [:legend {:style {:display :flex :justify-content :space-between
                          :align-items :center
                          :text-transform "uppercase"}}
         [:span {:style {:text-align :left
                         :letter-spacing :0.1em}}
          (nth cycled-stacks cycle-index nil)]]
        [:span {:style {:position :absolute
                        :right :0.5lh :top :-1lh
                        :padding-inline :0.25em
                        :border "1px solid black"
                        :background-color :white}}
         (str (inc tab-index) " / " tab-count)]
        [:p {:style {:padding :0.5em
                     :align-self :center :font-size :1.5rem
                     :font-family :sans-serif :font-weight :bold}}
         (:label on-tab)]]
       (when-not on-tab
         [:p {:style {:width :max-content}} "No visible tabs..."])
       ]
      [:div {:style {:flex 1 :display :flex :justify-content :flex-end}}
       (when (> tab-count 1)
         [:button
          {:on-click #(re-frame/dispatch [::events/inc-tab-index tab-count])}
          "Next Tab"])
       (when (> stack-count 1)
         [:button
          {:on-click (fn [_]
                       (re-frame/dispatch [::events/inc-cycle-index stack-count])
                       (re-frame/dispatch [::events/set-tab-index 0]))}
          ">>"])]]
          (when on-tab
            [cycle-tab-buttons on-address (= (dec tab-count) tab-index) tab-count])]))



(defn cycle-preset-loader
  []
  (let [preset-selection (r/atom "")]
    (fn []
      (let [presets @(re-frame/subscribe [::subs/cycle-presets])]
        (when (seq presets)
          [:div
         [:select
          {:on-change #(reset! preset-selection (-> % .-target .-value))
           :disabled (empty? presets)
           :style {}}
          [:option
           {:value ""}
           "Load Preset"]
          (doall
           (for [[preset _stacks] presets]
             ^{:key preset}
             [:option
              {:value preset}
              preset]))]
         (when (seq @preset-selection)
           [:button
            {:disabled (= "" preset-selection)
             :on-click #(re-frame/dispatch [::events/load-cycle-preset @preset-selection])}
            "Load Preset"])])))))

(defn cycle-preset-saver
  []
  (let [new-preset-name (r/atom "")]
    (fn []
      (let [cycled-stacks @(re-frame/subscribe [::subs/cycled-stacks])]
        [:div
         [:div
          [:input
           {:style {}
            :value @new-preset-name
            :on-change #(reset! new-preset-name (-> % .-target .-value))
            :max-length 20
            :placeholder "Preset Name"}]
          [:button
           {:disabled (or (empty? @new-preset-name)
                          (empty? cycled-stacks))
            :on-click (fn [_]
                        (re-frame/dispatch [::events/add-cycle-preset @new-preset-name cycled-stacks])
                        (reset! new-preset-name ""))}
           "Save Preset"]]]))))

(defn cycle-menu
  []
  (let [cycled-stacks @(re-frame/subscribe [::subs/cycled-stacks])
        cycle-index   @(re-frame/subscribe [::subs/cycle-index])
        tab-index      (re-frame/subscribe [::subs/tab-index])
        in-stack       (get cycled-stacks cycle-index)
        tabs-in-stack @(re-frame/subscribe [::subs/tabs-in-stack in-stack])
        stack-size     (count tabs-in-stack)]
    [:section
     {:style {:height :100%
              :display :flex :flex-direction :column
              :justify-content :space-evenly
              :padding-inline :0.5em
              :border-radius "0 8px 0 0"
              :border-right "3px solid black"}}
     [cycle-preset-loader]
     (when (seq cycled-stacks)
       [clear-cycled-stacks-button cycle-index tab-index])
     [cycle-stacks-list cycled-stacks]
     (when (seq cycled-stacks)
       [:div
        [cycle-stack-buttons cycled-stacks stack-size]
        [cycle-preset-saver]])
     ]))

(defn shelf
  ([name child]
   (let [show-shelf (r/atom true)]
     (fn [name child]
       [:<>
        [shelf-toggle-button name show-shelf]
        (when @show-shelf
          [:div
           {:style {:position :relative
                    :background-color :transparent}}
           [shelf-header name]
           child])])))
  ([name child _always-open] ;; anything truthy on that always-open one
   [:<>
    [shelf-header name]
    child]))

(defn tab-manager-mode-display [edit-mode*]
  [:div.mode-display
   {:style {:font-family :sans-serif
            :font-size :1rem :width :max-content
            :cursor :pointer :user-select :none
            :border "1px solid black" :border-radius :8px
            :padding :0.5em}
    :on-click #(swap! edit-mode* not)}
   [:span.mode-text.add
    {:style {:font-weight (if @edit-mode* :lighter :bold)
             :text-decoration (if @edit-mode* :none :underline)}}
    "Add"]
   [:span "/"]
   [:span.mode-text.edit
    {:style {:font-weight (if @edit-mode* :bold :lighter)
             :text-decoration (if @edit-mode* :underline :none)}}
    "Edit"]])

(defn tab-manager-adder
  []
  (let [add-tab-label (r/atom "")
        stack-entry   (r/atom "")
        ]
    (fn []
      (let [valid-label?   @(re-frame/subscribe [::subs/valid-tab-label? @add-tab-label])
            selected-stack @(re-frame/subscribe [::subs/selected-stack])]
        [:div.tab-creator
            [:input
             {:placeholder "Tab Label"
              :value @add-tab-label
              :on-change #(reset! add-tab-label (-> % .-target .-value))}]
           ]))))

(defn tab-manager-editor
  []
    (fn []
      (let [stacks @(re-frame/subscribe [::subs/stacks])
            proposed-name        @(re-frame/subscribe [::subs/proposed-name])
            proposed-stack       @(re-frame/subscribe [::subs/proposed-stack])
            selected-tabs        @(re-frame/subscribe [::subs/selected-tabs])
            [src-stack first-label] (first selected-tabs)
            multiple-selected?    (> (count selected-tabs) 1)
            changed-label?        (not= first-label proposed-name)
            changed-stack?        (not= src-stack proposed-stack)
            valid-label          @(re-frame/subscribe [::subs/valid-tab-label? proposed-name])
            ]
        [:div#tab-editor
         {:style {}}
         [:input#relabel-input
          {:value       proposed-name
           :disabled    multiple-selected?
           :placeholder "Tab Label"
           :on-change   #(re-frame/dispatch [::events/propose-name (-> % .-target .-value)])}]
         [:p {:style {:margin-inline "4px"
                      :align-self :center}}
          " in stack "]
         [:select
          {:value proposed-stack
           :on-change #(re-frame/dispatch [::events/propose-stack (-> % .-target .-value)])}
          (doall (for [stack stacks]
                   ^{:key stack}
                   [:option {:value stack} stack]))]])))

(defn tab-manager
  []
  (let [edit-mode     (r/atom false)]
    (fn []
      (let []
        [:section
         {:style {:border "1px dotted black"}}
         [tab-manager-mode-display edit-mode]
         (if @edit-mode
           [tab-manager-editor]
           [tab-manager-adder]
           )]))))

(defn app-2 []
  (let [is-cycle-mode (= :cycle @(re-frame/subscribe [::subs/mode]))]
    [:main#app
     {:style {:display :flex :flex-direction :row
              :max-width :1111px :height :100vh
              :margin-inline :auto
              :background-color :white
              :border-left "50px solid black"
              :border-right "50px solid black"}}
     (when is-cycle-mode
       [shelf "Cycles"
        [cycle-menu]])[shelf "Stacks"
      [:div {:style {:position :relative
                     :height :100%
                     :display :flex :flex-direction :column
                     :justify-content :center
                     :padding-block :3em}}
       [stack-selector]
       (when @(re-frame/subscribe [::subs/selected-stack])
        [:button {:on-click #(re-frame/dispatch [::events/deselect-stack])
                  :visibility (if @(re-frame/subscribe [::subs/selected-stack]) :visible :hidden)}
              "X"])
       [cycle-stack-transfer-button is-cycle-mode]]] ;; pass this the should-show condition

     ;; Content
     [:div
      {:style {:min-width :99px :width :100% :height :100%
               :display :flex :flex-direction :column
               :justify-content :space-around :align-items :center}}
      (if is-cycle-mode
        [:div {:style {:width :100% :height :100%
                       :position :relative
                       :display :flex :flex-direction :column
                       :justify-content :space-around :align-items :center}}
         [shelf-header "Tabs"]
         [cycle-view]
        ]
        [tab-selector])]
     [main-mode-display]
     ]))

(defn drawer ;; TODO add a size parameter, which it would divide from one to set max width
  [title child closed-child]
  (let [open? (r/atom true)] ;; TODO reset to false in prod
    (fn [title child closed-child]
      [:section.drawer {:class (if @open? "open" "closed")}
       [:button.drawer-button
        {:on-click #(swap! open? not)}
        title]
       [:div.drawer-content
        (if @open?
          child
          closed-child)]])))

(defn stack-adder
  []
  (let [stack-entry (r/atom "")]
    (fn []
      (let [stacks       @(re-frame/subscribe [::subs/stacks])
            valid-stack? (and (seq (.trim @stack-entry))
                              (not (some #{@stack-entry} stacks)))]
        [:div
         {:style {:text-align :center}}
         [:form {:on-submit #(.preventDefault %)} ;; don't refresh the page!
          [:input#new-stack-input
           {:value @stack-entry
            :placeholder "New Stack"
            :on-change #(reset! stack-entry (-> % .-target .-value))}]
          [:button#add-stack-button
           {:type :submit
            :style {:margin :0.25em} :disabled (not valid-stack?)
            :on-click (fn [_]
                        (when valid-stack?
                          (re-frame/dispatch [::events/add-empty-stack @stack-entry])
                          (reset! stack-entry "")
                          (.focus (.getElementById js/document "new-stack-input"))))}
           "+"]]]))))

(defn stack-sort-selector
  []
    [:select
     {:style {:margin-bottom :auto}
      :value @(re-frame/subscribe [::subs/sort-stacks-by])
      :on-change #(re-frame/dispatch [::events/sort-stacks-by (-> % .-target .-value)])}
     [:option {:value :alpha} "Name"]
     [:option {:value :count} "Tab Count"]
    ]
  )

(defn star-source-button
  []
  (let [star-mode (= :star @(re-frame/subscribe [::subs/tab-source]))]
    [:button#star-source-button
     {:class (when star-mode "active")
      :style {:opacity (if (or star-mode
                               (seq @(re-frame/subscribe [::subs/starred-tab-addresses])))
                         1 0)}
      :on-click (fn [_]
                  (re-frame/dispatch [::events/toggle-star-source])
                  (re-frame/dispatch [::events/deselect-stack])
                  (re-frame/dispatch [::events/deselect-all-tabs]))}
     (if star-mode "★" "☆")]))

(defn stack-drawer
  []
  [drawer "Stacks"
   [:div {:style {:position :relative
                  :max-width :max-content :height :100%
                  :display :flex :flex-direction :column
                  :justify-content :center
                  :padding-block :3em}}
    ;; [stack-sort-selector] ;; TODO find way of sorting by count
    [star-source-button]
    [stack-adder]
    [stack-selector]
    [:div#selected-stack-controls
     {:style {:margin-inline :auto
              :opacity (if @(re-frame/subscribe [::subs/selected-stack])
                         1 0)}}
     [:button {:on-click #(re-frame/dispatch [::events/deselect-stack])}
      "Deselect"]
     [:button
      {:on-click #(re-frame/dispatch [::events/propose-action :label
                                      (fn [label]
                                        (re-frame/dispatch [::events/rename-stack @(re-frame/subscribe [::subs/selected-stack]) label])
                                        (re-frame/dispatch [::events/deselect-all-tabs]))])}
      "Rename"]]]
   nil])

(defn tab-drawer-list
  [selected-stack selected-tabs]
  (let [starred-tab-addresses @(re-frame/subscribe [::subs/starred-tab-addresses])
        tab-source            @(re-frame/subscribe [::subs/tab-source])
        source-is-star        (= :star tab-source)]
    [:ul.tab-list
     [:div.tab-wrapper
      (for [tab @(re-frame/subscribe (case tab-source
                                       :stack [::subs/tabs-in-stack selected-stack]
                                       :star  [::subs/starred-tab-objs]))]
        (let [is-selected (some (fn [[_ label]] (= label (:label tab))) selected-tabs)
              is-starred  (or source-is-star
                           (some #{[selected-stack (:label tab)]} starred-tab-addresses))
              address [(if source-is-star
              ;; well this is ugly. just searches the tab addresses for a matching label
                         (first (some #(when (= (second %) (:label tab)) %) starred-tab-addresses))
                         selected-stack)
                       (:label tab)]]
          [:li.single-tab
           {:key (:label tab)
            :class (str (when is-selected "selected") " " (when is-starred "starred"))
            :on-click #(re-frame/dispatch [(if is-selected
                                             ::events/deselect-tab
                                             ::events/select-additional-tab)
                                           address])}
           (:label tab)]))]]))

(defn tab-adder
  [selected-stack selected-tabs]
  (let [label-entry (r/atom "")]
    (fn [selected-stack selected-tabs]
      (let [valid-label?   @(re-frame/subscribe [::subs/valid-tab-label? @label-entry])]
        [:div
         {:style {:text-align :center}}
         [:form {:on-submit #(.preventDefault %)} ;; don't refresh the page!
          [:input#new-tab-input
           {:max-length 42 ;; TODO what's a good limit?
            :value @label-entry
            :placeholder "New Tab"
            :on-change #(reset! label-entry (-> % .-target .-value))}]
          [:button#add-tab-button
           {:type :submit
            :style {:margin :0.25em} :disabled (not valid-label?)
            :on-click (fn [_]
                         (when valid-label?
                           (re-frame/dispatch [::events/add-tab [selected-stack
                                                                 (.trim @label-entry)]])
                           (reset! label-entry "")
                           (.focus (.getElementById js/document "new-tab-input"))))}
           "+"]]]))))

(defn tab-drawer
  []
  [drawer "Tabs"
   (let [selected-stack @(re-frame/subscribe [::subs/selected-stack])
         selected-tabs  @(re-frame/subscribe [::subs/selected-tabs])
         starred-tabs   @(re-frame/subscribe [::subs/starred-tab-addresses])
         star-source     (= :star @(re-frame/subscribe [::subs/tab-source]))]
     ;; this might be discouraged behavior! I'm really not sure.
     ;; basically checks on every render if all any tabs are starred,
     ;; and if not, gets you back to stack source.
    ;;  (when (and star-source (not (seq starred-tabs)))
    ;;    (re-frame/dispatch [::events/toggle-star-source]))
    ;; TODO this can't be correct. it creates visible latency
     (if (or star-source
             selected-stack)
       [:div {:style {:position :relative
                      :height   :100%
                      :display  :flex
                      :justify-content :space-evenly
                      :align-items :center}}
        [:div
         {:style {:display :flex :flex-direction :column
                  :justify-content :center :align-items :center}}
         (when-not star-source
           [tab-adder selected-stack selected-tabs])
         [tab-drawer-list selected-stack selected-tabs]]
        [:div
        ;;  {:style {:position :absolute :top :50% :right :1em :transform "translateY(-50%)"}}
         {:style {:position :relative :top :1.5lh ;; bump upwards to offset the tab adder
                  :display :flex :align-items :center}}
         [:span#tab-selection-indicator {:class (when (seq selected-tabs) "active")}
          "}"]
         [:ul.tab-selection-controls
          {:style {:opacity (if (seq selected-tabs) 1 0)}}
          [:button {:on-click #(re-frame/dispatch [::events/deselect-all-tabs])}
           "Deselect"]
          [:button {:on-click (fn [_]
                                (doseq [address selected-tabs]
                                  (re-frame/dispatch [::events/remove-tab address]))
                                (re-frame/dispatch [::events/deselect-all-tabs]))}
           "Delete"]
          (when (and (< 1 (count @(re-frame/subscribe [::subs/stacks])))
                     (not star-source))
            [:button {:on-click #(re-frame/dispatch [::events/propose-action :stack
                                                     (fn [stack]
                                                       (doseq [address selected-tabs]
                                                         (re-frame/dispatch [::events/move-tab address stack]))
                                                       (re-frame/dispatch [::events/deselect-all-tabs]))])}
             "Move"])
          (when (and (not star-source)
                     (= 1 (count selected-tabs)))
            [:button {:on-click #(re-frame/dispatch [::events/propose-action :label
                                                     (fn [label]
                                                       (re-frame/dispatch [::events/rename-tab (first selected-tabs) label])
                                                       (re-frame/dispatch [::events/deselect-all-tabs]))])}
             "Edit"])
          (when-not star-source
            [:button {:on-click #(doseq [address selected-tabs]
                                   (re-frame/dispatch [::events/star-tab address]))}
             "Star"])
         ;; only appears when at least one tab is starred
          (when (or star-source
                    (some starred-tabs selected-tabs))
            [:button {:on-click (fn [_]
                                  (doseq [address selected-tabs]
                                    (re-frame/dispatch-sync [::events/unstar-tab address]))
                                  (re-frame/dispatch [::events/deselect-all-tabs]))}
             "Unstar"])]]
          ;; TODO needs button to scroll to top of container when necessary
        ]
       [:span#no-stack-message
        "Select a stack to see its tabs."]))
   nil])

(defn proposed-action-description
  [request-type user-input]
  (let [selected-tabs @(re-frame/subscribe [::subs/selected-tabs])
                tab-count (count selected-tabs)]
            ;; TODO this overflows with very long tab/stack names, yet couldn't get it to work as desired
            [:p {:style {:min-width :max-content :font-size :1rem
                         :user-select :none}}
             (case request-type
               nil nil
               :stack
               (if (empty? user-input)
                 "Select a new stack from the dropdown."
                 (str "Moving "
                      (if (= tab-count 1) ;; TODO style names a bit better
                        (str "'" (-> selected-tabs first second) "' ")
                        (str tab-count " tabs "))
                      "to '" user-input "'."))
               :label
               (if (empty? user-input)
                 "Enter a new label."
                 (str "Renaming "
                      (-> selected-tabs first second)
                      " to '" user-input "'.")))]))

(defn input-bubble
  "Given a partially applied function waiting for an argument,
   raises a bubble with appropriate inputs and calls the function
   when an appropriate argument is given, or discards it otherwise."
  []
  (let [user-input (r/atom "")     ;; short delay to prevent flash
        reset-input! (fn [] (js/setTimeout #(reset! user-input "") 1000))
        submit-and-reset! (fn [_]
                            (re-frame/dispatch [::events/agree-to-action @user-input])
                            (reset-input!))]
    (fn []
      (let [give-me-a @(re-frame/subscribe [::subs/requested-value])]
        [:div
         (let [modal-open @(re-frame/subscribe [::subs/requested-value])]
           [:div#input-bubble-overlay
            {:style {:opacity (if modal-open
                                1
                                0)
                     :pointer-events (if modal-open
                                       :auto
                                       :none)}
             :on-click (fn [_]
                         (re-frame/dispatch [::events/discard-action])
                         (reset-input!))}
            (when modal-open [floating-tab-header "Action" {:top :10% :right :0
                                                            :border-right :none
                                                            :border-radius "8px 0 0 8px"}])])
         [:div#input-bubble
          {:class (when give-me-a "active")}
          [:div {:style {:display :flex :gap :0.5em
                         :padding-bottom :0.5em
                         :border-bottom "1px solid black"}}
           (case give-me-a
             nil nil
             :stack
             [:select {:style {:flex-grow 1
                               :text-align :center
                               :font-size :1.3rem}
                       :value @user-input
                       :on-change #(reset! user-input (-> % .-target .-value))}
              [:option {:value ""} ""]
              (doall
               (for [stack (filter #(not= @(re-frame/subscribe [::subs/selected-stack]) %)
                                   @(re-frame/subscribe [::subs/stacks]))]
                 ^{:key stack}
                 [:option {:value stack} stack]))]
             :label
             [:input {:auto-focus true
                      :type :text
                      :style {:flex-grow 1 :font-size :1.3rem :max-width :75%}
                      :value @user-input
                      :on-change #(reset! user-input (-> % .-target .-value))}]
             :moment
             [:input {:type  :radio
                      :name  :hide-until
                      :value :min}])
           [:button
            {:style {:font-size :1.2rem :min-width :max-content}
             :disabled (or (not @(re-frame/subscribe [::subs/proposed-action]))
                           (empty? @user-input))
             :on-click submit-and-reset!}
            "Make it So"]]
          [proposed-action-description give-me-a @user-input]
          [:button
           {:style {;; :position :absolute
                    :font-size :1rem
                    ;; :bottom :40% :left :50%
                    ;; :transform "translate(-50%, 0%)"
                    }
            :on-click (fn [_]
                        (re-frame/dispatch [::events/discard-action nil])
                        (reset-input!))}
           "Nevermind."]]]))))

;; let's really think about the layout of the full page
;; edit mode is just a series of drawers
;; cycle mode is just a feed of tabs, which needs only a source and controls

(defn app
  []
    [:main#app
     {:style {:display :flex
              :max-width :1111px :height :100vh
              :margin-inline :auto
              :background-color :white
              ;; :border-left "5px solid blue"
              ;; :border-right "5px solid blue"
              }}
     ;; everything in here should be a drawer
     ;; TODO make certain drawers take up less max space
     #_[drawer "Piles"
        [:p   "Open"]
        nil]
     [stack-drawer]
     [tab-drawer]
     [input-bubble]

     #_[:button {:style {:position :absolute
                         :right :0
                         :top :0
                         :padding-inline :0.25em}}
        "Edit?"]]
  )

