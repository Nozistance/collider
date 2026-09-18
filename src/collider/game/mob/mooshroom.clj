(ns collider.game.mob.mooshroom
  "Mooshroom stew, shearing and flower feeding."
  (:require [collider.data :as data]
            [collider.game.entity :as entity]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.out :as out]
            [collider.game.systems.items :as items]))

(set! *warn-on-reflection* true)

(def ^:private ^:const mutate-chance 1024)

(def ^:private ^:const brown 1)

(def ^:private ^:const shorn-mushrooms 5)

(defn- variant ^long [e] (long (or (:color e) 0)))

(defn- calf-variant [t eid a b]
  (cond (and (= (variant a) (variant b)) (animal/one-in? t eid :mutate mutate-chance)) (- 1 (variant a))
        (< (animal/rnd t eid :variant) 0.5) (variant a)
        :else (variant b)))

(def ^:private spec (animal/spec animal/goals calf-variant))

(defn brain [world eid e t tempters] (animal/brain spec world eid e t tempters))

(def ^:private ^:table stews
  (delay (into {} (for [r (:crafting (data/recipes))
                        :when (= :suspicious-stew (get-in r [:result :item]))
                        :let [flower (first (remove #{:bowl :brown-mushroom :red-mushroom}
                                                    (mapcat identity (:ingredients r))))]
                        :when flower]
                    [flower (get-in r [:result :components :suspicious-stew-effects])]))))

(defn- given [world peid p stack]
  (let [[changes left] (items/add-stack (or (:inventory p) {}) stack)]
    (concat (for [[slot s] changes] [:set-slot peid slot s])
            (when left [[:spawn-entity (items/dropped world peid left)]]))))

(defn- stew [effects]
  (cond-> {:item (if effects :suspicious-stew :mushroom-stew) :count 1}
          effects (assoc :components {:suspicious-stew-effects effects})))

(defn- bowled [world peid p eid e]
  (let [effects (:stew e)]
    (concat (given world peid p (stew effects))
            [(out/all (out/sound (if effects :mooshroom/suspicious :mooshroom/milk) (:pos e) 1.0 1.0))]
            (when effects [[:merge-entity eid {:stew nil}]]))))

(defn- sheared [eid e t]
  (let [[x y z] (:pos e)
        mushroom (if (= brown (variant e)) :brown-mushroom :red-mushroom)]
    (concat [[:remove-entity eid]
             [:spawn-entity (assoc (mobs/new-mob :cow (:pos e) nil t) :yaw (:yaw e) :head-yaw (:head-yaw e))]
             (out/all (out/sound :mooshroom/shear (:pos e) 1.0 1.0))
             (out/all (out/particles :explosion nil [x (+ (double y) 0.7) z] 1 0.0))]
            (for [i (range shorn-mushrooms)]
              [:spawn-entity (entity/item [x (+ (double y) 1.4) z] (entity/pop-velocity [t eid :shear i])
                                          {:item mushroom :count 1})]))))

(defn- fed-flower [eid e hands]
  (when-let [flower (and (= brown (variant e)) (not (:stew e)) (some #(when (@stews %) %) hands))]
    [[:merge-entity eid {:stew (@stews flower)}]
     (out/all (out/sound :mooshroom/eat (:pos e) 2.0 1.0))]))

(defn interact-deltas
  "Returns the deltas for players who use an item on a mooshroom.
  The item is a bowl, shears or a flower and the mooshroom is
  grown. A bowl goes before shears and shears before a flower."
  [world events t]
  (animal/on-interact world events
                      (fn [peid p eid e]
                        (when (and (= :mooshroom (:type e)) (not (mobs/baby? e)))
                          (let [hands (sense/hands-of p)]
                            (cond (hands :bowl) (bowled world peid p eid e)
                                  (hands :shears) (sheared eid e t)
                                  :else (fed-flower eid e hands)))))))
