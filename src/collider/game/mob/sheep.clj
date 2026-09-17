(ns collider.game.mob.sheep
  "Sheep grazing and lamb colours."
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
(def ^:private ^:const bite-at 4)
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

(defn- on-grass? [world [x y z]] (= (grass/grass-state) (sense/block-at world [x (dec (long y)) z])))

(defn- start-eat [world eid e t _]
  (let [cell (sense/feet-cell (:pos e))]
    (when (and (animal/one-in? t eid :eat (quot (if (mobs/baby? e) baby-eat-chance eat-chance) 2))
               (or (edible? world cell) (on-grass? world cell)))
      [(assoc e :task {:kind :eat :until (+ (long t) eat-ticks)})
       [(out/all (out/status eid :eat))]])))

(defn- eating? [_ e t _] (> (long (get-in e [:task :until])) (long t)))

(defn- ate [e t]
  (cond-> (assoc e :sheared? false)
          (mobs/baby? e) (assoc :baby-until (max (long t) (- (long (:baby-until e)) bite-growth)))))

(defn- bitten [world [x y z :as cell]]
  (let [below [x (dec (long y)) z]]
    (cond
      (edible? world cell)
      [[:set-blocks [[cell 0]]] (out/all (out/break-effect cell (sense/block-at world cell)))]
      (on-grass? world cell)
      [[:set-blocks [[below (grass/dirt-state)]]] (out/all (out/break-effect below (grass/grass-state)))])))

(defn- eat-tick [_ world _ e t _]
  (if-let [ds (when (= bite-at (- (long (get-in e [:task :until])) (long t)))
                (bitten world (sense/feet-cell (:pos e))))]
    [(ate e t) (when (get-in world [:rules :mob-griefing] true) ds)]
    [e nil]))

(defn- lamb-color [t eid a b]
  (let [m (mixes (hash-set (colors (:color a)) (colors (:color b))))]
    (cond m (.indexOf ^java.util.List colors m)
          (< (animal/rnd t eid :mix) 0.5) (:color a)
          :else (:color b))))

(def ^:private eat
  {:kind :eat :flags #{:move :look} :start start-eat :continue? eating? :tick eat-tick})

(def ^:private spec
  (let [[before after] (split-with #(not= :wander (:kind %)) animal/goals)]
    (animal/spec (concat before [eat] after) lamb-color)))

(defn brain [world eid e t tempters] (animal/brain spec world eid e t tempters))
