(ns game-of-life.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]
              [game-of-life.rules :as rules]
              [clojure.core.matrix :as matrix]))

(enable-console-print!)

(def g [[1 0 0 0 0 1 0 0 0 1]
        [0 1 0 0 1 0 0 0 0 0]
        [0 0 1 1 0 0 0 0 0 0]
        [0 0 1 1 0 0 0 0 0 0]
        [0 1 0 0 1 0 0 0 0 0]
        [1 0 0 0 0 1 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0]
        [1 0 0 0 0 0 0 0 0 1]
        ])

(def empty-grid [[0 0 0]
                 [0 0 0]
                 [0 0 0]])

(def test-grid [0 1 0 1])

(def app-state (atom {:grid g
                      :test-grid test-grid
                      ;;:running? false
                      :timer nil
                      :settings {:cell-size 10
                                 :live-cell-color "red"
                                 :dead-cell-color "white"}}))

(println "empty grid: " empty-grid)
(println "current grid: " (get-in @app-state [:grid]))
;;(println "next gen: " (rules/next-generation g))

(defn test-canvas
  "Creates animated clock based on seconds only."
  []
  (let [comp-state (atom {})]
    (reagent/create-class
     {
      :component-did-mount
      (fn [comp]
        (reset! comp-state {:ctx (.getContext (reagent/dom-node comp) "2d")}))
      :display-name "game-of-life-grid"
      :reagent-render (fn [comp]
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
                              (matrix/emap-indexed render-cell! grid)))

                          )
                        (println "comp-state: " @comp-state)
                        (println "app-state: " @app-state)
                        [:canvas {:width 200 :height 200 :style {:border "1px solid black" }}])})))

(defn start-game! []
  (swap! app-state update-in [:running] (constantly true))
  ;; (loop []
  ;;   (if (get-in @app-state [:running])
  ;;     (do
  ;;       (js/setTimeout #(println "running") 1000)
  ;;       (recur))))
  (let [next-gen! (fn []
                    (let [next-gen (rules/next-generation (:grid @app-state))]
                      (println "next gen: " next-gen)
                      (swap! app-state update-in [:grid] (constantly next-gen))))
        create-timer (js/setInterval next-gen! 2000)]
    (swap! app-state update-in [:timer] (constantly create-timer))))

(defn stop-game! []
  ;; (swap! app-state update-in [:running] (constantly true))
  ;; (loop []
  ;;   (if (get-in @app-state [:running])
  ;;     ))
  (println "Stopping timer")
  (js/clearInterval (:timer @app-state))
  (swap! app-state update-in [:timer] (constantly nil))
  ;;(js/alert "stop")
  )

;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Welcome to game-of-life 2"]
   [:div [:a {:href "/about"} "go to about page"]
    ]])

(defn about-page []
  [:div [:h2 "About game-of-life"]
   [:div [:a {:href "/"} "go to the home page"]]])

(defn current-page []
  [:div ;;[(session/get :current-page)]
   [:div [:h3 "Game of life"]
    [test-canvas]]
   [:div
    [:button {:on-click #(start-game!) :disabled (not (nil? (:timer @app-state)))} "START"]
    [:button {:on-click #(stop-game!)  :disabled (nil? (:timer @app-state))} "STOP"]]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
