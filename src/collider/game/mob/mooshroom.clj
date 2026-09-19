(ns collider.game.mob.mooshroom
  "Mooshroom stew, shearing, flower feeding and lightning."
  (:require [collider.data :as data]
            [collider.game.entity :as entity]
            [collider.game.loot :as loot]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.out :as out]
            [collider.game.systems.items :as items]
            [collider.random :as random]))

(set! *warn-on-reflection* true)

(def ^:private ^:const mutate-chance 1024)

(def ^:private ^:const brown 1)

(def ^:private ^:const shear-lift 1.0)

(def ^:private ^:const explode-lift 0.5)

(def ^:private ^:const loud-volume 2.0)

(def ^:private kept-on-shear
  [:pos :vel :yaw :head-yaw :on-ground :breed-ready-at])

(defn- variant ^long [e] (long (or (:color e) 0)))

(defn- mutates? [t eid a b]
  (and (= (variant a) (variant b))
       (animal/one-in? t eid :mutate mutate-chance)))

(defn- calf-variant [t eid a b]
  (cond (mutates? t eid a b) (- 1 (variant a))
        (< (animal/rnd t eid :variant) 0.5) (variant a)
        :else (variant b)))

(def spec
  "The mooshroom's goals; a calf takes either parent's variant,
  rarely the other one."
  (animal/spec animal/goals calf-variant))

(defn brain [world eid e t tempters]
  (animal/brain spec world eid e t tempters))

(def ^:private ^:table tables (delay (data/entity-drops)))

(def ^:private bowl-items #{:bowl :brown-mushroom :red-mushroom})

(defn- flower-of [r]
  (first (remove bowl-items (mapcat identity (:ingredients r)))))

(defn- stew-recipe? [r]
  (= :suspicious-stew (get-in r [:result :item])))

(defn- effects-of [r]
  (get-in r [:result :components :suspicious-stew-effects]))

(def ^:private ^:table stews
  (delay (into {}
               (for [r (:crafting (data/recipes))
                     :when (stew-recipe? r)
                     :let [flower (flower-of r)]
                     :when flower]
                 [flower (effects-of r)]))))

(defn- stew [effects]
  (let [item (if effects :suspicious-stew :mushroom-stew)]
    (cond-> {:item item :count 1}
            effects
            (assoc :components {:suspicious-stew-effects effects}))))

(defn- bowled [world peid eid e]
  (let [effects (:stew e)
        snd (if effects :mooshroom/suspicious :mooshroom/milk)
        s (stew effects)]
    (concat (items/filled-result-deltas world peid s true)
            [(out/all (out/sound snd (:pos e) 1.0 1.0))]
            (when effects [[:merge-entity eid {:stew nil}]]))))

(defn- mushrooms [t eid e]
  (loot/drops @tables :mooshroom-shear
              {:looting 0 :entity (mobs/loot-entity e)}
              #(random/of-key t eid [:shear %])))

(defn- shorn-items [t eid e]
  (let [[x y z] (:pos e)
        at [x (+ (double y) shear-lift) z]
        one (fn [i s]
              (let [vel (entity/pop-velocity [t eid :shear i])]
                [:spawn-entity (entity/item at vel s 0)]))]
    (map-indexed one (mushrooms t eid e))))

(defn- converted
  "Returns the cow a shorn mooshroom becomes.
  It keeps where it stood and how soon it breeds again; vanilla
  gives the new body full health, a temperate coat and a classic
  voice."
  [e t]
  (merge (mobs/new-mob :cow (:pos e) nil t)
         (select-keys e kept-on-shear)))

(defn- sheared [eid e t]
  (let [[x y z] (:pos e)
        height (double (second (mobs/box-of e)))
        at [x (+ (double y) (* explode-lift height)) z]
        puff (out/particles :explosion nil at 1 0.0)]
    (concat [[:remove-entity eid]
             (out/all (out/sound :mooshroom/shear (:pos e) 1.0 1.0))
             [:spawn-entity (converted e t)]
             (out/all puff)]
            (shorn-items t eid e))))

(defn- flower-in-hand [e hands]
  (when (and (= brown (variant e)) (not (:stew e)))
    (some #(when (@stews %) %) hands)))

(defn- fed-flower [eid e hands]
  (when-let [flower (flower-in-hand e hands)]
    [[:merge-entity eid {:stew (@stews flower)}]
     (out/all (out/sound :mooshroom/eat (:pos e) loud-volume 1.0))]))

(defn struck
  "Returns the deltas for a mooshroom that lightning bolt bid hits.
  Its colour turns over, and the same bolt never turns it twice."
  [eid e bid]
  (when-not (= bid (:struck-by e))
    (let [snd (out/sound :mooshroom/convert (:pos e) loud-volume 1.0)]
      [[:merge-entity eid {:color (- 1 (variant e)) :struck-by bid}]
       (out/all snd)])))

(defn- used [world peid eid e hands t]
  (cond (hands :bowl) (bowled world peid eid e)
        (hands :shears) (sheared eid e t)
        :else (fed-flower eid e hands)))

(defn interact-deltas
  "Returns the deltas for players who use an item on a mooshroom.
  The item is a bowl, shears or a flower and the mooshroom is
  grown. A bowl goes before shears and shears before a flower."
  [world events t]
  (let [f (fn [peid p eid e _]
            (when (and (= :mooshroom (:type e))
                       (not (mobs/baby? e)))
              (used world peid eid e (sense/hands-of p) t)))]
    (animal/on-interact world events f)))
