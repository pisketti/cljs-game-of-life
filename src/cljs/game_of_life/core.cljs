(ns game-of-life.core
  (:require [reagent.core :as r :refer [atom]]
            [game-of-life.rules :as rules]
            [game-of-life.utils :as utils]
            [clojure.core.matrix :as matrix]))

(enable-console-print!)

(def rows      40)
(def columns   40)
(def cell-size 10)

(defonce empty-grid (utils/create-empty-grid rows columns))

(defonce empty-grid (utils/create-empty-grid rows columns))

(def app-state (atom {:grid empty-grid
                      :preview nil
                      :timer nil
                      :settings {:cell-size cell-size
                                 :board-width (* cell-size columns)
                                 :board-height (* cell-size rows)
                                 :live-cell-color "red"
                                 :dead-cell-color "white"
                                 :preview-color "#b3b3b3"
                                 :interval 100}}))

(println (:settings @app-state))
(println "grid width: " (count (get-in @app-state [:grid 0] )))

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
                                  cell-size (get-in @app-state [:settings :cell-size])
                                  [canvas-x canvas-y] (get-canvas-coords [window-x window-y] canvas)
                                  matrix-x (.floor js/Math (/ canvas-x cell-size))
                                  matrix-y (.floor js/Math (/ canvas-y cell-size))]

                              ;; (println "window coords: " [[window-x window-y]]
                              ;;          " canvas coords: " [[canvas-x canvas-y]]
                              ;;          " matrix coors: " [matrix-x matrix-y])

                              [matrix-x matrix-y]))

        render-cell! (fn [ctx {:keys [cell-size live-cell-color dead-cell-color]}]
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
                          ;; (doseq [row grid]
                          ;;   (println row))
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
                             (swap! app-state update-in [:grid] utils/toggle-cell [x y]))
                           (set-mouse-down! true))

              mouse-up (fn [e]
                         ;;(println "mouseup x:" (.-x e) " y:" (.-y e))
                         (set-mouse-down! false))

              mouse-move (fn [e]
                           (if (:mousedown @comp-state)
                             (let [[x y] (get-matrix-coords e canvas)]
                               (swap! app-state update-in [:grid] utils/set-living-cell [x y]))))]

          (set! (.-onmousedown canvas) mouse-down)
          (set! (.-onmouseup canvas) mouse-up)
          (set! (.-onmousemove canvas) mouse-move)

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

;; -------------- Patterns ----------------

;; Space ships

(defonce glider (utils/expand-to [[0 1 0]
                                  [0 0 1]
                                  [1 1 1]] rows columns))

(defonce lwss (utils/expand-to [[0 1 0 0 1]
                                [1 0 0 0 0]
                                [1 0 0 0 1]
                                [1 1 1 1 0]] rows columns))

(defonce mwss (utils/expand-to [[0 0 0 1 0 0]
                                [0 1 0 0 0 1]
                                [1 0 0 0 0 0]
                                [1 0 0 0 0 1]
                                [1 1 1 1 1 0]] rows columns))

(defonce hwss (utils/expand-to [[0 0 0 1 1 0 0]
                                [0 1 0 0 0 0 1]
                                [1 0 0 0 0 0 0]
                                [1 0 0 0 0 0 1]
                                [1 1 1 1 1 1 0]] rows columns))

;; Oscillators

(defonce pulsar (utils/expand-to [[0 0 1 1 1 0 0 0 1 1 1 0 0]
                                  [0 0 0 0 0 0 0 0 0 0 0 0 0]
                                  [1 0 0 0 0 1 0 1 0 0 0 0 1]
                                  [1 0 0 0 0 1 0 1 0 0 0 0 1]
                                  [1 0 0 0 0 1 0 1 0 0 0 0 1]
                                  [0 0 1 1 1 0 0 0 1 1 1 0 0]
                                  [0 0 0 0 0 0 0 0 0 0 0 0 0]
                                  [0 0 1 1 1 0 0 0 1 1 1 0 0]
                                  [1 0 0 0 0 1 0 1 0 0 0 0 1]
                                  [1 0 0 0 0 1 0 1 0 0 0 0 1]
                                  [1 0 0 0 0 1 0 1 0 0 0 0 1]
                                  [0 0 0 0 0 0 0 0 0 0 0 0 0]
                                  [0 0 1 1 1 0 0 0 1 1 1 0 0]] rows columns))

(defonce figure-8 (utils/expand-to [[1 1 1 0 0 0]
                                    [1 1 1 0 0 0]
                                    [1 1 1 0 0 0]
                                    [0 0 0 1 1 1]
                                    [0 0 0 1 1 1]
                                    [0 0 0 1 1 1]] rows columns))

(defonce queenbee (utils/expand-to [
                                    [0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0]
                                    [0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0]
                                    [0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 1 1]
                                    [1 1 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 1]
                                    [1 1 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0]
                                    [0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0]
                                    [0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0]] rows columns))

(defonce twinbee (utils/expand-to [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0]
                                   [1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 1 1]
                                   [1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1]
                                   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0]
                                   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                                   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                                   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                                   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0]
                                   [1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0]
                                   [1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0]
                                   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0]]
                                  rows columns))





;; -------------------------
;; View

(defn pattern-button [pattern-matrix name]
  [:button {:on-click #(set-pattern pattern-matrix)
            :on-mouse-over #(set-preview pattern-matrix)
            :on-mouse-out #(set-preview nil)
            :class "small-btn blue"} name])

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
     (pattern-button glider "Glider")
     (pattern-button lwss "LW space ship")
     (pattern-button mwss "MW space ship")
     (pattern-button hwss "HW space ship")]]

   [:div
    [:h5 "Oscillators"]
    (pattern-button pulsar "Pulsar")
    (pattern-button figure-8 "Figure 8")
    (pattern-button queenbee "Queen bee shuttle")
    (pattern-button twinbee "Twin bees shuttle")]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [game-of-life] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
