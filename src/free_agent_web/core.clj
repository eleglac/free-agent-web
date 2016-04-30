(ns free-agent-web.core
  (:require [free-agent-web.collision :as fa-c]
            [quil.core :refer :all]
            [quil.applet :refer [defapplet]]))

(def wide  800)
(def high  600)
(def popl  100)
(def rate  24)
(def foods 0)

(defn make-agent
  "Creates an agent, which is a map containing the included params and a few others."
  
  [x y rad shell core food]
  
  {:id    (gensym)
   :x     x
   :y     y
   :rad   rad
   :shell shell
   :core  core
   :food  food
   :d-x   0 
   :d-y   0
   :dead?       false
   :colliding?  false
   :stationary? false
   :behavior?   (rand-nth [true true true false])})


;(defn make-food
  ;"Creates a 'food,' which is a map containing location and quantity."
  
  ;[x y food]
  
  ;{:x    x
   ;:y    y
   ;:diam 3
   ;:food food})

(defn draw-agent
  "Draws an agent.  The agent is visualized as an circle, with a 'shell' and a 'core.'"
  
  [agent]
  
  (let [[rgb f]   (:shell agent)
        [r g b l] (:core  agent)
        rad       (:rad   agent)
        x         (:x     agent)
        y         (:y     agent)]
    
    (stroke rgb f)
    (stroke-weight 2)
    (fill r g b l)
    (ellipse-mode :radius)
    (ellipse x y rad rad)))

;(defn draw-food
  ;"Draws a food.  Food is essentially a small, non-moving agent."
  
  ;[food]
  
  ;(stroke 0 200 0)
  ;(stroke-weight 3)
  ;(fill 40 240 40)
  ;(ellipse-mode :center)
  ;(ellipse (:x food) (:y food) (:diam food) (:diam food)))

(defn populate
  "Creates a list of entities of either type :agent or :food.  If neither of those is
   the type given as *type, will create a list of nil, I think."
  
  [pop-count *type]
  
  (repeatedly pop-count
    (fn []
      (let [x     (rand-int wide)
            y     (rand-int high)
            rad   5 
            shell [0 255]
            core  [(rand-int 255) (rand-int 255) (rand-int 255) 255]
            food  0] 

        (make-agent x y rad shell core food)))))

(defn keep-within-bounds
  "Checks to see that an agent isn't about to move off of the screen. If it is,
   returns a new agent that is within bounds, otherwise lets it go on."
  
  [agent]
    
  (let 
    [ x  (if (pos? (:d-x agent)) 
           (min (+ (:x agent) (:d-x agent)) (width))
           (max (+ (:x agent) (:d-x agent)) 0)) 
      y  (if (pos? (:d-y agent)) 
           (min (+ (:y agent) (:d-y agent)) (height))
           (max (+ (:y agent) (:d-y agent)) 0))]

    (assoc agent :x x :y y)))

(defn flow-back 
  "If coord is negative, adjust it to flow to other side of screen.  Otherwise pass through."

  [limit coord]

  (if (pos? coord)
    coord
    (- limit coord)))

(defn wrap-around
  "Attempts to allow agents to travel across the edges of the screen.  Top wraps to bottom,
   left wraps to right, and vice-versa for each."

  [agent]

  (let 
    [x  (if (pos? (:d-x agent))
          (rem (+ (:d-x agent) (:x agent)) (width))
          (flow-back (width) (+ (:d-x agent) (:x agent))))
     y  (if (pos? (:d-y agent))
          (rem (+ (:d-y agent) (:y agent)) (height))
          (flow-back (height) (+ (:d-y agent) (:y agent))))]

    (assoc agent :x x :y y)))

(defn change-movement [agent]
  (assoc agent :d-x (- 2 (rand-int 5))
               :d-y (- 2 (rand-int 5))
               :stationary? false
               :colliding? false
               :rad (min (inc (:rad agent)) 20)))

(defn stationary? 
  "Attempts to allow for changing of agent direction when it is clear that the agent
   has become stationary, either because it never moved or because it is stuck in a 
   corner."
  
  [agent]

  (let [d-agent (wrap-around agent)]
    (if (= agent d-agent) 
      (assoc d-agent :stationary? true)
      (assoc d-agent :stationary? false
                     :rad (max (dec (:rad agent)) 5)))))


(defn update-agents
  "Take a list of agents and return a new list, very similar to the old one except that
   what needs changing has been changed.  I'm on a horse."
  
  [agents]
  
  (let [d-agents (map stationary? (fa-c/collision-detect agents))]
    (map
      (fn [agent]
        (if (or (:stationary? agent) (:colliding? agent))
          (change-movement agent)
          agent))
      d-agents)))

(defn setup []
  (smooth)
  (frame-rate rate)
  (def agent-list (atom (populate popl :agent))))
  ;(def food-list  (atom (populate foods :food))))

(defn draw []
  (background 200)
  ;(doall (map draw-food @food-list))
  (doall (map draw-agent (swap! agent-list update-agents))))

(defapplet free-agent
  :title "A game inspired by Darwin Pond."
  :setup setup
  :draw draw
  :size [wide high])

(defn -main []
  (print "running?"))
