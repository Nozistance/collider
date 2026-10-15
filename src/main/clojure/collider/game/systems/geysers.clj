(ns collider.game.systems.geysers
  "Geysers of potent sulfur under water and what they lift."
  (:require [collider.data :as data]
            [collider.game.changes :as changes]
            [collider.game.effect :as effect]
            [collider.game.effect.account :as account]
            [collider.game.entity :as entity]
            [collider.game.mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.game.level :as level]
            [collider.game.projectile :as projectile]
            [collider.game.reach :as reach]
            [collider.num :as num]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.geyser :as geyser]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def ^:private ^:const countdown-period 20)

(def ^:private lift (num/f32 0.2))

(def ^:private base-speed (num/f32 0.3))

(def ^:private ^:table unmoved
  (delay (set (data/tag-values "entity_type"
                               "not_affected_by_geysers"))))

(def ^:private bodies
  #{:player :item :tnt :falling-block :experience-orb})

(defn- caught? [e]
  (let [t (:type e)]
    (and (or (bodies t) (contains? entity/thrown-types t)
             (mobs/mob-type? t))
         (entity/alive? e) (not (game-mode/spectator? e)))))

(defn- launched? [e]
  (not (or (and (= :player (:type e)) (:flying e))
           (contains? @unmoved (:type e)))))

(defn- caught-size [e] (when (caught? e) (entity/box e)))

(defn- inside? [[x lo z] ^double hi e [half h]]
  (let [[ex ey ez] (:pos e)
        ex (double ex) ey (double ey) ez (double ez)
        half (double half) x (long x) z (long z)]
    (and (< (- ex half) (inc x)) (> (+ ex half) x)
         (< ey hi) (> (+ ey (double h)) (double lo))
         (< (- ez half) (inc z)) (> (+ ez half) z))))

(defn- slow? [e ^long depth]
  (< (v/y (or (:vel e) [0.0 0.0 0.0])) (+ base-speed (* depth 0.1))))

(defn- lifted [eid e]
  (cond-> [[:push eid [0.0 lift 0.0]]]
    (#{:item :player :experience-orb} (:type e))
    (conj [:merge-entity eid {:needs-sync? true}])))

(defn- eased [eid e]
  (when (and (> (v/y (or (:vel e) [0.0 0.0 0.0])) -0.5)
             (> (double (or (:fall e) 0.0)) 1.0))
    [[:merge-entity eid {:fall 1.0}]]))

(defn- caught-deltas [eid e depth]
  (concat (eased eid e)
          (when (and (launched? e) (slow? e depth)) (lifted eid e))))

(defn- launch-deltas
  [world [x y z] depth]
  (let [n (geyser/reach (:chunks world) [x y z] depth)
        lo [x (+ (long y) 1 (min 0 (dec n))) z]
        hi (double (+ (long y) 2 (max 0 (dec n))))]
    (for [[eid e] (sort-by key (:entities world))
          :let [s (caught-size e)]
          :when (and s (inside? lo hi e s))
          d (caught-deltas eid e depth)]
      d)))

(defn- turn-deltas [world pos st]
  (changes/set-deltas world [[pos (geyser/turned st)]]))

(defn- countdown-deltas [world pos st e depth]
  (when (zero? (rem (long (:tick world)) countdown-period))
    (let [ph (geyser/phase st)
          c (geyser/counted pos ph depth (:countdown e))]
      (concat (when (not= c (:countdown e))
                [[:set-block-entity pos (assoc e :countdown c)]])
              (when (zero? c) (turn-deltas world pos st))))))

(def ^:private ^:const gas-period 10)

(def ^:private sickness (effect/instance 80 0 true true))

(defn- gas-box [[x y z]]
  (let [x (long x) y (long y) z (long z)]
    [(- x 2.5) y (- z 2.5) (+ x 3.5) (inc y) (+ z 3.5)]))

(defn- reached? [world q eye]
  (let [chunks (:chunks world)
        below [(v/x eye) (dec (v/y eye)) (v/z eye)]
        from (v/centre (dir/down q))]
    (and (geyser/passable? (chunk/at chunks (v/cell eye)))
         (<= (v/dist-sq eye (v/centre q)) 9.0)
         (block/holds-water-source? (chunk/at chunks (v/cell below)))
         (nil? (projectile/block-clip world from
                                      (mapv - below from))))))

(defn- sickened? [world q b e]
  (and (account/living? e) (entity/alive? e)
       (not (game-mode/spectator? e))
       (v/boxes-meet? b (projectile/body-box e (:pos e)))
       (reached? world q (reach/eye-pos e))))

(defn- gas-deltas [world q]
  (when (zero? (rem (long (:tick world)) gas-period))
    (let [b (gas-box q)]
      (for [[eid e] (sort-by key (:entities world))
            :when (sickened? world q b e)
            :let [acc (account/account eid e)]
            d (account/deltas (account/land acc :nausea sickness) e)]
        d))))

(defn tick-deltas
  [world [pos e]]
  (let [st (chunk/at (:chunks world) pos)
        ph (geyser/phase st)
        q (when (#{:wet :dormant :erupting :continuous} ph)
            (geyser/source (:chunks world) pos))
        depth (when q (geyser/water-depth pos q))]
    (when depth
      (concat
        (when (#{:erupting :continuous} ph)
          (launch-deltas world pos depth))
        (when (#{:dormant :erupting} ph)
          (countdown-deltas world pos st e depth))
        (when (#{:wet :dormant} ph)
          (gas-deltas world q))))))

(defn unsynced
  "Returns the deltas that end the lift of every player lifted
  before, ahead of the geysers of this tick."
  [world]
  (for [[eid e] (level/of-types world [:player])
        :when (:needs-sync? e)]
    [:merge-entity eid {:needs-sync? false}]))
