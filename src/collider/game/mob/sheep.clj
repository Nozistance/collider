(ns collider.game.mob.sheep
  "What makes a sheep a sheep: grazing and wool colour."
  (:require [collider.data :as data]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.out :as out]
            [collider.world.block :as block]
            [collider.world.blocks.grass :as grass]))

(set! *warn-on-reflection* true)

(def ^:private ^:const eat-ticks 40)
(def ^:private ^:const eat-chance 1000)
(def ^:private ^:const baby-eat-chance 50)
(def ^:private ^:const bite-growth 1200)

(def ^:private ^:table edible
  (delay (set (data/tag-values "block" "edible_for_sheep"))))

(def ^:private colors
  [:white :orange :magenta :light-blue :yellow :lime :pink :gray
   :light-gray :cyan :purple :blue :brown :green :red :black])

(def ^:private mixes
  {#{:blue :red}    :purple #{:blue :green} :cyan #{:black :white} :gray
   #{:gray :white}  :light-gray #{:green :white} :lime #{:red :white} :pink
   #{:purple :pink} :magenta #{:red :yellow} :orange #{:blue :white} :light-blue})

(defn- edible? [world cell] (contains? @edible (block/block-of (sense/block-at world cell))))

(defn- start-eat [world eid e t _]
  (when (and (animal/one-in? t eid :eat (quot (if (mobs/baby? e) baby-eat-chance eat-chance) 2))
             (let [cell (animal/feet e)] (or (edible? world cell) (animal/grass-block? world cell))))
    [(assoc e :task {:kind :eat :until (+ (long t) eat-ticks)})
     [(out/all (out/status eid :eat))]]))

(defn- bite [world e t]
  (let [cell (animal/feet e)
        [x y z] cell
        below [x (dec (long y)) z]
        griefing? (get-in world [:rules :mob-griefing] true)
        ate (cond-> (assoc e :sheared? false)
                    (mobs/baby? e) (assoc :baby-until (max (long t) (- (long (:baby-until e)) bite-growth))))]
    (cond
      (edible? world cell)
      [ate (when griefing?
             [[:set-blocks [[cell 0]]] (out/all (out/break-effect cell (sense/block-at world cell)))])]
      (animal/grass-block? world cell)
      [ate (when griefing?
             [[:set-blocks [[below (grass/dirt-state)]]]
              (out/all (out/break-effect below (grass/grass-state)))])]
      :else [e nil])))

(defn- eat-tick [world _ e t]
  (if (and (= :eat (get-in e [:task :kind]))
           (= (- (long (get-in e [:task :until])) (long t)) 4))
    (bite world e t)
    [e nil]))

(defn- eating? [_ e t]
  (and (= :eat (get-in e [:task :kind]))
       (> (long (get-in e [:task :until])) (long t))))

(defn- lamb-color [t eid a b]
  (let [m (mixes (hash-set (colors (:color a)) (colors (:color b))))]
    (cond m (.indexOf ^java.util.List colors m)
          (< (animal/rnd t eid :mix) 0.5) (:color a)
          :else (:color b))))

(def ^:private spec
  (animal/spec {:goals       (let [[before after] (split-with #(not= :wander (first %)) animal/goals)]
                               (vec (concat before [[:eat #{:move :look} start-eat]] after)))
                :child-color lamb-color
                :continue?   eating?
                :tick        eat-tick}))

(defn brain [world eid e t tempters] (animal/brain spec world eid e t tempters))
