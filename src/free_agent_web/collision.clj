(ns free-agent-web.collision
  (:require [clojure.math.combinatorics :as mc]
            [clojure.set :as s]))

(defn euclid-dist
  "Gives the euclidean distance between two points."
  
  [x-dim y-dim]
  
  (Math/sqrt (+ (* x-dim x-dim) (* y-dim y-dim))))

(defn colliding? 
  "Returns true if [agent-one agent-two] are colliding, false otherwise."
  
  [agent-pair]
  
  (if (seq agent-pair)
    (let [[agent-one agent-two] (flatten agent-pair) 
          dist  (+ (:rad agent-one) (:rad agent-two))
          x-dim (- (:x agent-two) (:x agent-one))
          y-dim (- (:y agent-two) (:y agent-one))
          coll  (<= (euclid-dist x-dim y-dim) dist)] 
      
      coll)
    false))

(defn agent-and-cells 
  "Annotates an agent with the cells it occupies. 
   [agent] -> [agent [cells]]"

  [agent]

  (let [min-x (- (:x agent) (:rad agent))
        max-x (+ (:x agent) (:rad agent))
        min-y (- (:y agent) (:rad agent))
        max-y (+ (:y agent) (:rad agent))
        
        pts   [[min-x min-y]
               [max-x min-y]
               [min-x max-y]
               [max-x max-y]]] 

    [agent 
     (distinct 
       (map 
         (fn [[x y]] 
           [(int (/ x 40)) 
            (int (/ y 40))]) 
         pts))]))

(defn pair-to-cell-agent 
  "Creates the cell keywords for each group of agents in a cell.
   [agent [cells]] -> [[:cell agent]...]"

  [[agent cells]]
  
  (interleave 
    (map 
       (fn [[x y]] (keyword (str x "-" y)))
       cells) 
    (repeat (count cells) agent)))

(defn nearby-agent-maps
  "Takes the [agent cell] pairs and returns a list of maps of agents to check for collisions."

  [agent-cell-maps]
  
  (vals
    (apply merge-with
      (comp vec flatten vector)
      (map 
        (fn [[cell agent]]
          (hash-map cell (vector agent)))
        agent-cell-maps))))

(defn all-occupied-cells
  "Take all the agents, get a list of each cell occupied by at least one agent.
   [agents] -> [[:cell [agents]]...]"

  [agents]

  (->> (map agent-and-cells agents)
       (map pair-to-cell-agent)
       (flatten)
       (partition 2)
       (nearby-agent-maps)))

(defn check-cell
  "Check a single cell for all collisions. Sets :colliding? to true when relevant."

  [agents]

  (let [pairs       (mc/combinations agents 2)
        klidepairs  (filter colliding? pairs)
        kliders     (distinct (flatten klidepairs))
        freebirds   (s/difference (set agents) kliders)]
 
    (concat (map #(assoc % :colliding? true) kliders) freebirds)))

(defn check-all 
  "Given a map of cells and agents within those cells, check for collisions.
   Returns a list of all agents...?"
 
  [collision-map]
  
  (let [cells  (map check-cell collision-map) 
        agents (distinct (flatten cells))
        ids    (map :id agents)
        lawlz  (partition 2 (interleave ids agents))]

    (vals
      (apply merge-with
        (fn [id agent] (assoc agent :colliding? true))
        (map (fn [[key val]] (hash-map key val)) lawlz)))))

(defn collision-detect 
  "In theory, will set the :colliding? flags on all agents that are running into each other."

  [agents]

  (check-all (all-occupied-cells agents)))

