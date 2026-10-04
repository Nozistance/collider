(ns collider.game.mob.gift
  "What a mob drops of itself, by a loot table of its own."
  (:require [collider.data :as data]
            [collider.game.entity :as entity]
            [collider.game.loot :as loot]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(def ^:private ^:table tables (delay (data/entity-drops)))

(defn wait
  "Returns the ticks mob eid waits at tick t for its next gift, from
  n up to below twice n. The key k salts the draw."
  ^long [t eid k ^long n]
  (+ n (long (* n (random/of-key t eid k)))))

(defn dropped
  "Returns the deltas that spawn what loot table drops at mob eid,
  e, at tick t. The key k salts the draws."
  [t eid e table k]
  (let [pos (:pos e)
        ctx {:entity (mobs/loot-entity e)}
        roll #(random/of-key t eid [k %])
        spawn (fn [i s]
                (let [vel (entity/pop-velocity [t eid k i])]
                  [:spawn-entity (entity/item pos vel s)]))]
    (map-indexed spawn (loot/drops @tables table ctx roll))))

(defn- pitch ^double [t eid k]
  (let [k (keyword (str (name k) "-pitch"))
        r #(random/of-key t eid [k %])]
    (+ 1.0 (* 0.2 (- (double (r 1)) (double (r 2)))))))

(defn gift-deltas
  "Returns the deltas of mob eid dropping its gift table at tick t.
  When anything falls the mob sounds snd and places it, else nothing
  happens. The key k salts the draws."
  [t eid e table snd k]
  (when-let [ds (seq (dropped t eid e table k))]
    (let [s (out/sound snd (:pos e) 1.0 (pitch t eid k))]
      (concat ds [(out/all s)]
              (signal/game-event :entity-place (:pos e) eid)))))
