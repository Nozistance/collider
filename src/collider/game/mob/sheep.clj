(ns collider.game.mob.sheep
  "Sheep grazing, shearing, dyeing and lamb colours."
  (:require [collider.data :as data]
            [collider.game.craft :as craft]
            [collider.game.entity :as entity]
            [collider.game.loot :as loot]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.grass :as grass]))

(set! *warn-on-reflection* true)

(def ^:private ^:const eat-ticks 40)

(def ^:private ^:const eat-chance 1000)

(def ^:private ^:const baby-eat-chance 50)

(def ^:private ^:const bite-at 4)

(def ^:private ^:const bite-growth 1200)

(def ^:private ^:const shear-lift 1.0)

(def ^:private ^:const shear-spread 0.1)

(def ^:private ^:const shear-jump 0.05)

(def ^:private ^:table edible
  (delay (set (data/tag-values "block" "edible_for_sheep"))))

(def ^:private ^:table tables (delay (data/entity-drops)))

(defn- edible? [world cell]
  (contains? @edible (block/block-of (sense/block-at world cell))))

(defn- grass-at? [world [x y z]]
  (= :grass-block
     (block/block-of (sense/block-at world [x (dec (long y)) z]))))

(defn- eat-odds ^long [e]
  (quot (if (mobs/baby? e) baby-eat-chance eat-chance) 2))

(defn- start-eat [world eid e t _]
  (let [cell (sense/feet-cell (:pos e))]
    (when (and (animal/one-in? t eid :eat (eat-odds e))
               (or (edible? world cell) (grass-at? world cell)))
      [(assoc e :task {:kind :eat :until (+ (long t) eat-ticks)})
       [(out/all (out/status eid :eat))]])))

(defn- eating? [_ e t _]
  (> (long (get-in e [:task :until])) (long t)))

(defn- ate [e t]
  (let [left (long (or (:baby-until e) t))
        grown (max (long t) (- left bite-growth))]
    (cond-> (assoc e :sheared? false)
            (mobs/baby? e) (assoc :baby-until grown))))

(defn- bitten [world [x y z :as cell]]
  (let [below [x (dec (long y)) z]]
    (cond
      (edible? world cell)
      [[:set-blocks [[cell 0]]]
       (out/all (out/break-effect cell (sense/block-at world cell)))]
      (grass-at? world cell)
      [[:set-blocks [[below (grass/dirt-state)]]]
       (out/all (out/break-effect below (grass/grass-state)))])))

(defn- bite-now? [e t]
  (= bite-at (- (long (get-in e [:task :until])) (long t))))

(defn- eat-tick [_ world _ e t _]
  (if-let [ds (when (bite-now? e t)
                (bitten world (sense/feet-cell (:pos e))))]
    [(ate e t) (when (get-in world [:rules :mob-griefing] true) ds)]
    [e nil]))

(defn- dye-item [color] (keyword (str (name color) "-dye")))

(defn- mixed
  "Returns the colour the two dyes craft into, else nil."
  [a b]
  (let [stacks [{:item (dye-item a) :count 1}
                {:item (dye-item b) :count 1}]
        in (craft/trim {:w 2 :h 1 :stacks stacks})]
    (when-let [r (craft/find (craft/index) in nil)]
      (data/dye-color (get-in r [:result :item])))))

(defn- lamb-color [t eid a b]
  (let [ca (mobs/dye-colors (long (:color a)))
        cb (mobs/dye-colors (long (:color b)))]
    (or (mobs/color-id (mixed ca cb))
        (if (< (animal/rnd t eid :mix) 0.5) (:color a) (:color b)))))

(defn- wool-stacks [t eid e]
  (loot/drops @tables :sheep-shear
              {:looting 0 :entity (mobs/loot-entity e)}
              #(random/of-key t eid [:shear %])))

(defn- shear-vel [t eid ^long i]
  (let [r (fn [k] (random/of-key t eid [:shear-vel i k]))
        [vx vy vz] (entity/pop-velocity [t eid :shear i])]
    [(+ (double vx) (* shear-spread (- (r :x1) (r :x2))))
     (+ (double vy) (* shear-jump (r :y)))
     (+ (double vz) (* shear-spread (- (r :z1) (r :z2))))]))

(defn- singles [stacks]
  (mapcat #(repeat (long (:count % 1)) (:item %)) stacks))

(defn- shorn-items [t eid e]
  (let [[x y z] (:pos e)
        at [x (+ (double y) shear-lift) z]
        one (fn [i item]
              (let [s {:item item :count 1}]
                [:spawn-entity
                 (entity/item at (shear-vel t eid i) s)]))]
    (map-indexed one (singles (wool-stacks t eid e)))))

(defn- sheared [t eid e]
  (list* [:merge-entity eid {:sheared? true}]
         (out/all (out/sound :sheep/shear (:pos e) 1.0 1.0))
         (shorn-items t eid e)))

(defn- dye-in-hand [hands]
  (some data/dye-color hands))

(defn- dyed [peid eid e color]
  (let [id (mobs/color-id color)]
    (when (not= (long id) (long (or (:color e) 0)))
      [[:merge-entity eid {:color id}]
       (out/except peid (out/sound :dye/use (:pos e) 1.0 1.0))])))

(defn- shearable? [e]
  (and (not (mobs/baby? e)) (not (:sheared? e))))

(defn- used [t peid eid e hands]
  (cond
    (hands :shears) (when (shearable? e) (sheared t eid e))
    (:sheared? e) nil
    :else (when-let [c (dye-in-hand hands)] (dyed peid eid e c))))

(defn interact-deltas
  "Returns the deltas for players who shear or dye a sheep.
  Shears take the wool of a grown unshorn sheep, and a dye of
  another colour recolours one that still wears its wool."
  [world events t]
  (let [f (fn [peid p eid e]
            (when (= :sheep (:type e))
              (used t peid eid e (sense/hands-of p))))]
    (animal/on-interact world events f)))

(def ^:private eat
  {:kind  :eat :flags #{:move :look} :start start-eat
   :continue? eating? :tick eat-tick})

(def ^:private spec
  (let [before? #(not= :wander (:kind %))
        [before after] (split-with before? animal/goals)]
    (animal/spec (concat before [eat] after) lamb-color)))

(defn brain [world eid e t tempters]
  (animal/brain spec world eid e t tempters))
