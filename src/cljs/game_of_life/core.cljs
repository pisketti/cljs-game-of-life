(ns game-of-life.core
  (:require [reagent.core :as r :refer [atom]]
            [game-of-life.rules :as rules]
            [clojure.core.matrix :as matrix]))

(enable-console-print!)

;;Interesting pattern that evolves for
;;quite some time before stabilizing
;; (def g1 [[1 0 0 0 0 1 0 0 0 1]
;;          [0 1 0 0 1 0 0 0 0 0]
;;          [0 0 1 1 0 0 0 0 0 0]
;;          [0 0 1 1 0 0 0 0 0 0]
;;          [0 1 0 0 0 0 0 0 0 0]
;;          [1 0 0 0 0 1 0 0 0 0]
;;          [0 0 0 0 0 1 0 0 0 0]
;;          [0 0 0 0 0 1 0 0 0 0]
;;          [0 0 0 0 0 1 0 0 0 0]
;;          [1 0 0 0 0 0 0 0 0 1]])

;; (def g2 [[1 0 0 0 0 0 0 0 0 1]
;;          [0 0 0 0 0 0 0 0 0 0]
;;          [0 0 0 0 0 0 0 0 0 0]
;;          [0 0 0 0 0 0 0 0 0 0]
;;          [0 0 0 0 0 0 0 0 0 0]
;;          [0 0 0 0 0 0 0 0 0 0]
;;          [0 0 0 0 1 1 1 0 0 0]
;;          [0 0 0 0 1 0 0 0 0 0]
;;          [0 0 0 0 0 0 0 0 0 0]
;;          [1 0 0 0 0 0 0 0 0 1]])

(defonce empty-grid (rules/create-empty-grid 40 40))

(def app-state (atom {:grid empty-grid
                      :timer nil
                      :settings {:cell-size 10
                                 :board-width 400
                                 :board-height 400
                                 :live-cell-color "red"
                                 :dead-cell-color "white"
                                 :interval 500}}))

(defn game-of-life-board
  "Creates a game of life board"
  []
  (let [
        comp-state (atom {:canvas nil
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
                              (println "window coords: " [[window-x window-y]] " canvas coords: " [[canvas-x canvas-y]] " matrix coors: " [matrix-x matrix-y])
                              [matrix-x matrix-y]))

        render-cell! (fn [ctx {:keys [cell-size live-cell-color dead-cell-color]}]
                       (fn [[y x] item]
                         (if (= item 1)
                           (set! (.-fillStyle ctx) live-cell-color)
                           (set! (.-fillStyle ctx) dead-cell-color))
                         (let [slot-x (* cell-size x)
                               slot-y (* cell-size y)]
                           (.fillRect ctx slot-x slot-y cell-size cell-size))))
        render-board! (fn []
                        (let [ctx  (get-in @comp-state [:ctx])
                              grid (get-in @app-state  [:grid])
                              settings (get-in @app-state [:settings])]
                          (if ctx
                            (matrix/emap-indexed (render-cell! ctx settings) grid))))]
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
                  :style {:border "1px solid black" }}])})))

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
  [:div {:style {:margin "0px 0px 0px 50px"}}
   [:div [:h3 "Game of life"]
    [game-of-life-board]]

   (let [running? (not (nil? (:timer @app-state)))]
     [:div
      [:button {:on-click #(start-game!)
                :disabled running?
                :style {:width "200px" :margin "0px 1px 0px 0px"}} "PLAY"]
      [:button {:on-click #(stop-game!)
                :disabled (not running?)
                :style {:width "200px" :margin "0px 0px 0px 1px"}} "PAUSE"]])
   [:div
    [:button {:on-click #(reset-board!)
              :style {:width "400px" :margin "0px 1px 0px 0px"}} "reset"]]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [game-of-life] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
