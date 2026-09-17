(ns collider.game.mob.mooshroom
  "What makes a mooshroom a mooshroom: stew from a bowl, mushrooms under the
   shears, and a brown one that eats flowers."
  (:require [collider.data :as data]
            [collider.game.entity :as entity]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.systems.items :as items]))

(set! *warn-on-reflection* true)

(def ^:private ^:const mutate-chance 1024)
(def ^:private ^:const brown 1)

(defn- variant ^long [e] (long (or (:color e) 0)))

(defn- calf-variant [t eid a b]
  (cond (and (= (variant a) (variant b)) (animal/one-in? t eid :mutate mutate-chance)) (- 1 (variant a))
        (< (animal/rnd t eid :variant) 0.5) (variant a)
        :else (variant b)))

(def ^:private spec (animal/spec {:child-color calf-variant}))

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
    (concat (map (fn [[slot s]] [:set-slot peid slot s]) changes)
            (when left [[:spawn-entity (items/dropped world peid left)]]))))

(defn- bowled [world peid p eid e]
  (let [effects (:stew e)]
    (concat (given world peid p (cond-> {:item (if effects :suspicious-stew :mushroom-stew) :count 1}
                                        effects (assoc :components {:suspicious-stew-effects effects})))
            [(out/all (out/sound (if effects :mooshroom/suspicious :mooshroom/milk) (:pos e) 1.0 1.0))]
            (when effects [[:merge-entity eid {:stew nil}]]))))

(defn- sheared [eid e t]
  (let [[x y z] (:pos e)
        mushroom (if (= brown (variant e)) :brown-mushroom :red-mushroom)]
    (concat [[:remove-entity eid]
             [:spawn-entity (assoc (mobs/new-mob :cow (:pos e) nil t) :yaw (:yaw e) :head-yaw (:head-yaw e))]
             (out/all (out/sound :mooshroom/shear (:pos e) 1.0 1.0))
             (out/all (out/particles :explosion nil [x (+ (double y) 0.7) z] 1 0.0))]
            (for [i (range 5)]
              [:spawn-entity (entity/item [x (+ (double y) 1.4) z] (entity/pop-velocity [t eid :shear i])
                                          {:item mushroom :count 1})]))))

(defn- fed-flower [eid e flower]
  (when-not (:stew e)
    [[:merge-entity eid {:stew (@stews flower)}]
     (out/all (out/sound :mooshroom/eat (:pos e) 2.0 1.0))]))

(defn interact-deltas [world events t]
  (mapcat (fn [[tag peid target]]
            (when (= :interact tag)
              (let [e (get-in world [:entities target])
                    p (get-in world [:entities peid])]
                (when (and (= :mooshroom (:type e)) (not (mobs/baby? e)))
                  (let [hands (keep #(get-in p [:inventory % :item]) [(+ 36 (long (or (:held-slot p) 0))) 45])]
                    (cond (some #{:bowl} hands) (bowled world peid p target e)
                          (some #{:shears} hands) (sheared target e t)
                          (= brown (variant e)) (when-let [flower (some #(when (contains? @stews %) %) hands)]
                                                  (fed-flower target e flower))))))))
          events))
