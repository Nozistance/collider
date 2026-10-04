(ns collider.game.mob.sheep
  "Sheep grazing, shearing, dyeing and lamb colours."
  (:require [collider.data :as data]
            [collider.game.changes :as changes]
            [collider.game.craft :as craft]
            [collider.game.delta :as delta]
            [collider.game.entity :as entity]
            [collider.game.inventory :as inventory]
            [collider.game.loot :as loot]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.nav :as nav]
            [collider.game.mob.sense :as sense]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.grass :as grass]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(def ^:private ^:const eat-ticks 40)

(def ^:private ^:const eat-chance 1000)

(def ^:private ^:const baby-eat-chance 50)

(def ^:private ^:const bite-at 2)

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

(defn- eat-task [t] {:kind :eat :until (+ (long t) eat-ticks)})

(defn- start-eat [world eid e t _]
  (let [cell (sense/feet-cell (:pos e))]
    (when (and (animal/one-in? t eid :eat (eat-odds e))
               (or (edible? world cell) (grass-at? world cell)))
      [(nav/stop (assoc e :task (eat-task t)))
       [(out/all (out/status eid :eat))]])))

(defn- eating? [_ e t _]
  (> (long (get-in e [:task :until])) (inc (long t))))

(defn- ate [e t]
  (cond-> (assoc e :sheared? false)
    (animal/grows? e) (animal/aged-up t bite-growth)))

(defn- bitten
  [world [x y z :as cell]]
  (let [below [x (dec (long y)) z]
        st (sense/block-at world cell)]
    (cond
      (edible? world cell)
      (changes/set-deltas
        world [[cell (block/emptied st) [[:break st]]]])
      (grass-at? world cell)
      (let [grass [:break (grass/grass-state)]]
        (changes/flagged-deltas
          world [[below (grass/dirt-state) [grass]]] 2)))))

(defn- animation-left
  "Returns what is left of the eating animation of sheep e after its
  goal ticked at tick t. It counts passes of the goal selector, which
  runs on every second tick."
  ^long [e t]
  (quot (- (long (get-in e [:task :until])) (long t) 2) 2))

(defn- bite-now? [e t]
  (= bite-at (animation-left e t)))

(defn biting?
  "Returns true when sheep e may bite what it eats at tick t, the one
  way a sheep changes a block."
  [e t]
  (and (= :eat (get-in e [:task :kind])) (bite-now? e t)))

(defn- eat-tick [_ world eid e t _]
  (if-let [ds (when (bite-now? e t)
                (-> (bitten world (sense/feet-cell (:pos e)))
                    (delta/authored (delta/entity-author eid e))))]
    [(ate e t) (when (get-in world [:rules :mob-griefing] true) ds)]
    [e nil]))

(defn- dye-item [color] (keyword (str (name color) "-dye")))

(defn- mixed [a b]
  (let [stacks [{:item (dye-item a) :count 1}
                {:item (dye-item b) :count 1}]
        in (craft/trim {:w 2 :h 1 :stacks stacks})]
    (when-let [r (craft/recipe-for (craft/index) in nil)]
      (data/dye-color (get-in r [:result :item])))))

(defn- lamb-color [_ t eid a b]
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

(defn- sheared [t peid p hand eid e]
  (concat [[:merge-entity eid {:sheared? true}]
           (out/all (out/sound :sheep/shear (:pos e) 1.0 1.0))]
          (signal/game-event :shear (:pos e) peid)
          (inventory/hurt-item-deltas peid p hand 1)
          (shorn-items t eid e)))

(defn- shearable? [e]
  (and (not (mobs/baby? e)) (not (:sheared? e))))

(defn shear-result
  "Returns what shears do to a sheep.
  A grown sheep with wool loses it. Any other sheep swallows
  the click."
  [{:keys [t peid p hand eid e item]}]
  (when (= :shears item)
    (if (shearable? e)
      {:result :success-server :deltas (sheared t peid p hand eid e)}
      {:result :consume})))

(defn- dyed [peid p hand eid e id]
  (concat [[:merge-entity eid {:color id}]
           (out/except peid (out/sound :dye/use (:pos e) 1.0 1.0))]
          (inventory/consume-deltas peid p hand 1)))

(defn dye-result
  "Returns what a dye does to a sheep that still wears its wool.
  The coat takes the colour."
  [{:keys [peid p hand eid e item]}]
  (when (and (= :sheep (:type e)) (not (:sheared? e)))
    (when-let [id (some-> (data/dye-color item) mobs/color-id)]
      (when (not= (long id) (long (or (:color e) 0)))
        {:result :success :deltas (dyed peid p hand eid e id)}))))

(def ^:private eat
  {:kind  :eat :flags #{:move :look :jump} :start start-eat
   :continue? eating? :tick eat-tick})

(def spec
  "The goals of a sheep, with eating grass before strolling.
  A lamb wears the colours of its parents mixed."
  (let [before? #(not= :wander (:kind %))
        [before after] (split-with before? animal/goals)]
    (animal/spec (concat before [eat] after) lamb-color)))

(defn brain
  [world eid e t tempters]
  (animal/brain spec world eid e t tempters))
