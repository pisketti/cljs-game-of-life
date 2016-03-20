(ns game-of-life.core
    (:require [reagent.core :as reagent :refer [atom]]
              [game-of-life.rules :as rules]
              [clojure.core.matrix :as matrix]))

(enable-console-print!)

;;Interesting pattern that evolves for
;;quite some time before stabilizing
(def g1 [[1 0 0 0 0 1 0 0 0 1]
         [0 1 0 0 1 0 0 0 0 0]
         [0 0 1 1 0 0 0 0 0 0]
         [0 0 1 1 0 0 0 0 0 0]
         [0 1 0 0 0 0 0 0 0 0]
         [1 0 0 0 0 1 0 0 0 0]
         [0 0 0 0 0 1 0 0 0 0]
         [0 0 0 0 0 1 0 0 0 0]
         [0 0 0 0 0 1 0 0 0 0]
         [1 0 0 0 0 0 0 0 0 1]])

(def g2 [[1 0 0 0 0 0 0 0 0 1]
         [0 0 0 0 0 0 0 0 0 0]
         [0 0 0 0 0 0 0 0 0 0]
         [0 0 0 0 0 0 0 0 0 0]
         [0 0 0 0 0 0 0 0 0 0]
         [0 0 0 0 0 0 0 0 0 0]
         [0 0 0 0 1 1 1 0 0 0]
         [0 0 0 0 1 0 0 0 0 0]
         [0 0 0 0 0 0 0 0 0 0]
         [1 0 0 0 0 0 0 0 0 1]])

(def empty-grid [[0 0 0]
                 [0 0 0]
                 [0 0 0]])

(def app-state (atom {:grid g1
                      ;;:running? false
                      :timer nil
                      :settings {:cell-size 10
                                 :live-cell-color "red"
                                 :dead-cell-color "white"
                                 :interval 500}}))

(println "empty grid: " empty-grid)
(println "current grid: " (get-in @app-state [:grid]))

(defn game-of-life-canvas
  "Creates a game of life board"
  []
  (let [comp-state (atom {:canvas nil
                          :ctx nil})]
    (reagent/create-class
     {:display-name "game-of-life-canvas"

      :component-did-mount
      (fn [comp]
        (reset! comp-state {:ctx (.getContext (reagent/dom-node comp) "2d")}))

      :reagent-render
      (fn []
        (let [ctx  (get-in @comp-state [:ctx])
              grid (get-in @app-state  [:grid])
              {:keys [cell-size
                      live-cell-color
                      dead-cell-color]} (get-in @app-state [:settings])]
          (if ctx
            (let [render-cell! (fn [[y x] item]
                                 (if (= item 1)
                                   (set! (.-fillStyle ctx) live-cell-color)
                                   (set! (.-fillStyle ctx) dead-cell-color))
                                 (let [slot-x (* cell-size x)
                                       slot-y (* cell-size y)]
                                   (.fillRect ctx slot-x slot-y cell-size cell-size)))]
              (matrix/emap-indexed render-cell! grid))))
        (println "comp-state: " @comp-state)
        (println "app-state: " @app-state)
        [:canvas {:width 200 :height 200 :style {:border "1px solid black" }}])})))

(defn start-game! []
    (let [next-gen! (fn []
                    (let [next-gen (rules/next-generation (:grid @app-state))]
                      (println "next gen: " next-gen)
                      (swap! app-state update-in [:grid] (constantly next-gen))))
        create-timer (js/setInterval next-gen! (get-in @app-state [:settings :interval]))]
    (swap! app-state update-in [:timer] (constantly create-timer))))

(defn stop-game! []
  (js/clearInterval (:timer @app-state))
  (swap! app-state update-in [:timer] (constantly nil)))

(defn reset-board! []
  (stop-game!)
  (swap! app-state update-in [:grid] (constantly g1)))

;; -------------------------
;; View

(defn game-of-life []
  [:div {:style {:margin "0px 0px 0px 50px"}}
   [:div [:h3 "Game of life"]
    [game-of-life-canvas]]

   (let [running? (not (nil? (:timer @app-state)))]
     [:div
      [:button {:on-click #(start-game!)
                :disabled running?
                :style {:width "100px" :margin "0px 1px 0px 0px"}} "PLAY"]
      [:button {:on-click #(stop-game!)
                :disabled (not running?)
                :style {:width "100px" :margin "0px 0px 0px 1px"}} "PAUSE"]])
   [:div
    [:button {:on-click #(reset-board!)
              :style {:width "200px" :margin "0px 1px 0px 0px"}} "reset"]]])

;; -------------------------
;; Initialize app

(defn init! []
  (reagent/render [game-of-life] (.getElementById js/document "app")))
