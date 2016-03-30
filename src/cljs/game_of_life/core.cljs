(ns game-of-life.core
  (:require [reagent.core :as r :refer [atom]]
            [game-of-life.rules :as rules]
            [clojure.core.matrix :as matrix]))

(enable-console-print!)


(defonce empty-grid (rules/create-empty-grid 40 40))

(def rows  40)
(def columns 40 )

(def app-state (atom {:grid empty-grid
                      :preview nil
                      :timer nil
                      :settings {:cell-size 10
                                 :board-width 400
                                 :board-height 400
                                 :live-cell-color "red"
                                 :dead-cell-color "white"
                                 :preview-color "#b3b3b3"
                                 :interval 500}}))

;; Patterns

(defonce glider (rules/expand-to [[0 1 0]
                                  [0 0 1]
                                  [1 1 1]] rows columns))

(defonce lwss (rules/expand-to [[0 1 0 0 1]
                                [1 0 0 0 0]
                                [1 0 0 0 1]
                                [1 1 1 1 0]] rows columns))

(defonce mwss (rules/expand-to [[0 0 0 1 0 0]
                                [0 1 0 0 0 1]
                                [1 0 0 0 0 0]
                                [1 0 0 0 0 1]
                                [1 1 1 1 1 0]] rows columns))

(defonce hwss (rules/expand-to [[0 0 0 1 1 0 0]
                                [0 1 0 0 0 0 1]
                                [1 0 0 0 0 0 0]
                                [1 0 0 0 0 0 1]
                                [1 1 1 1 1 1 0]] rows columns))

(defn set-preview [preview-matrix]
  (swap! app-state update-in [:preview] (constantly preview-matrix)))

(defn set-pattern [pattern-matrix]
  (swap! app-state update-in [:grid] (constantly pattern-matrix))
  (set-preview nil))

(defn game-of-life-board
  "Creates a game of life board"
  []
  (let [comp-state (atom {:canvas nil
                          :ctx nil
                          :mousedown false})
        get-canvas-coords (fn [[window-x window-y] canvas]
                               (let [rect (.getBoundingClientRect canvas)
                                     canvas-x (- window-x (.-left rect))
                                     canvas-y (- window-y (.-top rect))]
                                 [canvas-x canvas-y]))
        get-matrix-coords (fn [mouse-event canvas]
                            (let [window-x (.-x mouse-event)
                                  window-y (.-y mouse-event)
                                  [canvas-x canvas-y] (get-canvas-coords [window-x window-y] canvas)
                                  matrix-x (.floor js/Math (/ canvas-x 10))
                                  matrix-y (.floor js/Math (/ canvas-y 10))]
                              (println "window coords: " [[window-x window-y]]
                                       " canvas coords: " [[canvas-x canvas-y]]
                                       " matrix coors: " [matrix-x matrix-y])
                              [matrix-x matrix-y]))

        render-cell! (fn [ctx {:keys [cell-size live-cell-color dead-cell-color]}]
                       (println "live-cell-color: " live-cell-color)
                       (fn [[y x] item]
                         (let [alive? (= item 1)
                               redraw-dead? dead-cell-color]
                           (if alive?
                             (set! (.-fillStyle ctx) live-cell-color)
                             (set! (.-fillStyle ctx) dead-cell-color))
                           (let [slot-x (* cell-size x)
                                 slot-y (* cell-size y)]
                             (if (or alive? redraw-dead?)
                               (.fillRect ctx slot-x slot-y cell-size cell-size))))))

        render-board! (fn []
                        (let [ctx  (get-in @comp-state [:ctx])
                              grid (get-in @app-state  [:grid])
                              preview (get-in @app-state  [:preview])
                              settings (get-in @app-state [:settings])
                              preview-color (:preview-color settings)
                              preview-settings (-> settings
                                                   (assoc :live-cell-color preview-color)
                                                   (assoc :dead-cell-color nil))]
                          (when ctx
                            (matrix/emap-indexed (render-cell! ctx settings) grid)
                            (when preview
                              (matrix/emap-indexed (render-cell! ctx preview-settings) preview)))))]
    (r/create-class
     {:display-name "game-of-life-board"

      :component-did-mount
      (fn [comp]
        (println ">>component-did-mount")
        (let [canvas (r/dom-node comp)
              ctx (.getContext canvas "2d")
              set-mouse-down! (fn [val]
                                (swap! comp-state update-in [:mousedown] (constantly val)))
              mouse-down (fn [e]
                           (let [[x y] (get-matrix-coords e canvas)]
                             (swap! app-state update-in [:grid] rules/toggle-cell [x y]))
                           (set-mouse-down! true))

              mouse-up (fn [e]
                         (println "mouseup x:" (.-x e) " y:" (.-y e))
                         (set-mouse-down! false))

              mouse-move (fn [e]
                           (if (:mousedown @comp-state)
                             (let [[x y] (get-matrix-coords e canvas)]
                               (swap! app-state update-in [:grid] rules/set-living-cell [x y]))))]

          (set! (.-onmousedown canvas) mouse-down)
          (set! (.-onmouseup canvas) mouse-up)
          (set! (.-onmousemove canvas) mouse-move)

          (println "mouse click added. mousedown added")
          (reset! comp-state {:canvas canvas
                              :ctx ctx})))

      :reagent-render
      (fn []
        (render-board!)
        [:canvas {:width (get-in @app-state [:settings :board-width])
                  :height (get-in @app-state [:settings :board-height])
                  :style {:border "1px solid black" :cursor "default"}}])})))

(defn start-game! []
    (let [next-gen! (fn []
                    (let [next-gen (rules/next-generation (:grid @app-state))]
                      (swap! app-state update-in [:grid] (constantly next-gen))))
        create-timer (js/setInterval next-gen! (get-in @app-state [:settings :interval]))]
    (swap! app-state update-in [:timer] (constantly create-timer))))

(defn stop-game! []
  (js/clearInterval (:timer @app-state))
  (swap! app-state update-in [:timer] (constantly nil)))

(defn reset-board! []
  (stop-game!)
  (swap! app-state update-in [:grid] (constantly empty-grid)))

;; -------------------------
;; View

(defn game-of-life []
  [:div {:style {:margin "0px 0px 0px 0px"}}
   [:div [:h3 "Game of life"]
    [game-of-life-board]]

   (let [running? (not (nil? (:timer @app-state)))]
     [:div
      [:div {:class "half-size"}
       [:button {:on-click #(start-game!)
                      :disabled running?
                      :class "full-btn green"} "PLAY"]]
      [:div {:class "half-size"}
       [:button {:on-click #(stop-game!)
                 :disabled (not running?)
                 :class "full-btn red"} "PAUSE"]]])
   [:div
    [:button {:on-click #(reset-board!)
              :class "full-btn dark-gray"} "CLEAR"]]

   [:div
    [:h5 "space ships"]

    [:div
     [:button {:on-click #(set-pattern glider)
               :on-mouse-over #(set-preview glider)
               :on-mouse-out #(set-preview nil)
               :class "small-btn blue"} "Glider"]
     [:button {:on-click #(set-pattern lwss)
               :on-mouse-over #(set-preview lwss)
               :on-mouse-out #(set-preview nil)
               :class "small-btn blue"} "LW space ship"]
     [:button {:on-click #(set-pattern mwss)
               :on-mouse-over #(set-preview mwss)
               :on-mouse-out #(set-preview nil)
               :class "small-btn blue"} "MW space ship"]
     [:button {:on-click #(set-pattern hwss)
               :on-mouse-over #(set-preview hwss)
               :on-mouse-out #(set-preview nil)
               :class "small-btn blue"} "HW space ship"]
     ]
    ]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [game-of-life] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
