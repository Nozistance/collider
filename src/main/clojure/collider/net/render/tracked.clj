(ns collider.net.render.tracked
  "Spawn and data packets of a tracked entity."
  (:require [collider.data :as data]
            [collider.game.entity :as entity]
            [collider.game.hanging :as hanging]
            [collider.proto.entitydata :as ed]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def ^:private kinds
  (into #{:player :sheep :cow :mooshroom :pig :chicken :rabbit :item
          :experience-orb :tnt :falling-block :area-effect-cloud
          :painting :item-frame :glow-item-frame}
        entity/thrown-types))

(def ^:private ^:table entity-type
  (delay (zipmap kinds
                 (map #(data/registry-id "entity_type" %) kinds))))

(def ^:private ^:table entity-effect-particle
  (delay (data/registry-id "particle_type" :entity-effect)))

(defn kind-of
  "Returns the type e shows itself as on the wire."
  [e]
  (let [t (:type e)]
    (if (contains? kinds t) t :player)))

(def ^:private shared-flags
  {:burning? 0 :sneaking? 1 :sprinting? 3 :swimming? 4 :invisible? 5
   :glowing? 6})

(defn- flags-byte ^long [meta]
  (reduce-kv (fn [^long b k ^long bit]
               (if (get meta k) (bit-or b (bit-shift-left 1 bit)) b))
             0 shared-flags))

(def ^:private flag-keys (vec (keys shared-flags)))

(defn- flags? [meta] (boolean (some #(contains? meta %) flag-keys)))

(def ^:private pose-id
  {:standing 0 :sleeping 2 :swimming 3 :crouching 5})

(def ^:private living-flags {:is-using 1 :off-hand 2})

(defn- using-item-byte ^long [meta]
  (let [using (long (:is-using living-flags))]
    (case (:using-item? meta)
      :off (bit-or using (long (:off-hand living-flags)))
      (nil false) 0
      using)))

(defn- common-fields [meta]
  (cond-> {} (flags? meta) (assoc :shared-flags (flags-byte meta))))

(defn- player-fields [meta]
  (cond-> (common-fields meta)
          (contains? meta :pose)
          (assoc :pose (pose-id (:pose meta) 0))
          (contains? meta :using-item?)
          (assoc :living-flags (using-item-byte meta))
          (contains? meta :sleeping-pos)
          (assoc :sleeping-pos (:sleeping-pos meta))
          (contains? meta :absorption)
          (assoc :absorption (double (or (:absorption meta) 0.0)))
          (contains? meta :score)
          (assoc :score (long (or (:score meta) 0)))))

(defn- color-byte ^long [meta]
  (bit-or (bit-and (long (or (:color meta) 0)) 15)
          (if (:sheared? meta) 0x10 0)))

(defn- animal-fields [meta]
  (cond-> (common-fields meta)
          (contains? meta :baby?)
          (assoc :baby (boolean (:baby? meta)))))

(def ^:private coat-keys
  {:cow [:cow-variant :cow-sound]
   :pig [:pig-variant :pig-sound]
   :chicken [:chicken-variant :chicken-sound]})

(defn- coat-fields
  "Returns the coat and the voice of a cow, a pig or a chicken as the
  ids of their registries."
  [kind meta]
  (let [[ck vk] (coat-keys kind)
        n (name kind)]
    (cond-> (animal-fields meta)
      (contains? meta ck)
      (assoc :variant
             (data/datapack-id (str n "_variant") (ck meta)))
      (contains? meta vk)
      (assoc :sound-variant
             (data/datapack-id (str n "_sound_variant") (vk meta))))))

(defn- stack-fields [meta]
  (cond-> {} (contains? meta :stack) (assoc :item (:stack meta))))

(defn- tnt-fields [meta]
  (let [f (:fuse meta)]
    (if (and f (not= f (ed/default :primed-tnt :fuse)))
      {:fuse f}
      {})))

(defn- cloud-fields [meta]
  (cond-> {}
          (contains? meta :radius) (assoc :radius (:radius meta))
          (contains? meta :waiting?) (assoc :waiting (:waiting? meta))
          (contains? meta :color)
          (assoc :particle [@entity-effect-particle (:color meta)])))

(def ^:private renamed-classes
  {:mooshroom :mushroom-cow :item :item-entity :tnt :primed-tnt
   :glow-item-frame :item-frame})

(defn- class-of [kind]
  (cond (entity/thrown-types kind) :throwable-item-projectile
        (kinds kind) (get renamed-classes kind kind)))

(defn- sheep-fields [meta]
  (cond-> (animal-fields meta)
    (contains? meta :color) (assoc :wool (color-byte meta))))

(defn- variant-fields [meta]
  (cond-> (animal-fields meta)
    (contains? meta :variant) (assoc :type (long (:variant meta)))))

(defn- falling-fields [meta]
  (cond-> {}
    (contains? meta :start) (assoc :start-pos (:start meta))))

(defn- effect-particle [[k c]]
  [(data/registry-id "particle_type" k) c])

(defn- living-fields [meta]
  (cond-> {}
    (contains? meta :effect-particles)
    (assoc :effect-particles
           (mapv effect-particle (:effect-particles meta)))
    (contains? meta :effect-ambience)
    (assoc :effect-ambience (boolean (:effect-ambience meta)))))

(defn- orb-fields [meta]
  (cond-> (common-fields meta)
    (contains? meta :value) (assoc :value (:value meta))))

(defn- hanging-fields [meta]
  (cond-> {}
    (contains? meta :facing)
    (assoc :direction (dir/index (or (:facing meta) :south)))
    (contains? meta :stack) (assoc :item (:stack meta))
    (contains? meta :rotation)
    (assoc :rotation (long (or (:rotation meta) 0)))
    (contains? meta :variant)
    (assoc :variant (or (:variant meta) (hanging/default-variant)))))

(defn- entity-fields [kind meta]
  (case kind
    :player (merge (player-fields meta) (living-fields meta))
    (:cow :pig :chicken)
    (merge (coat-fields kind meta) (living-fields meta))
    :sheep (merge (sheep-fields meta) (living-fields meta))
    (:mooshroom :rabbit)
    (merge (variant-fields meta) (living-fields meta))
    :item (merge (common-fields meta) (stack-fields meta))
    :tnt (tnt-fields meta)
    :falling-block (falling-fields meta)
    :area-effect-cloud (cloud-fields meta)
    :experience-orb (orb-fields meta)
    (:painting :item-frame :glow-item-frame) (hanging-fields meta)
    (stack-fields meta)))

(defn entity-data
  "Returns the entity data entries of meta for an entity of kind."
  [kind meta]
  (if-let [cls (class-of kind)]
    (ed/entries cls (entity-fields kind meta))
    []))

(def equipment-slots
  "The wire slots of the equipment a tracker holds, in its order."
  [0 2 3 4 5])

(defn- spawn-rotation [tr kind k now]
  (if-let [a (and tr (entity/thrown-types kind) (get tr k))]
    (/ (* (double a) 360.0) 256.0)
    now))

(defn- spawn-pos [e tr kind]
  (cond (hanging/types kind) (mapv double (:block-pos e))
        tr (mapv double (:pos tr))
        :else (:pos e)))

(defn- spawn-data [e kind]
  (cond (= :falling-block kind) (:block e)
        (entity/thrown-types kind) (long (:owner e 0))
        (hanging/types kind) (dir/index (:facing e))
        :else 0))

(defn- spawn-head-yaw [e kind]
  (if (or (entity/thrown-types kind) (hanging/types kind))
    0.0
    (or (:head-yaw e) (:yaw e 0.0))))

(defn- add-entity-packet [eid e tr kind]
  {:packet :add-entity :eid eid :uuid (entity/uuid-of eid e)
   :type   (@entity-type kind)
   :pos    (spawn-pos e tr kind)
   :vel    (or (when tr (:vel-sent tr)) (:vel e) [0.0 0.0 0.0])
   :yaw    (spawn-rotation tr kind :yaw (:yaw e 0.0))
   :pitch  (spawn-rotation tr kind :pitch (:pitch e 0.0))
   :head-yaw (spawn-head-yaw e kind)
   :data   (spawn-data e kind)})

(defn- equipment-of [tr]
  (let [equip (if tr (:equip tr) [])]
    (keep-indexed (fn [i s] (when s [(equipment-slots i) s]))
                  equip)))

(defn- spawn-packets [world eid]
  (when-let [e (get-in world [:entities eid])]
    (let [kind (kind-of e)
          tr (:track e)
          d (entity-data kind (if tr (:mdata tr) {}))
          equip (equipment-of tr)]
      (concat
        [{:packet :bundle-delimiter}
         (add-entity-packet eid e tr kind)]
        (when (seq d) [{:packet :set-entity-data :eid eid :data d}])
        (when (seq equip)
          [{:packet :set-equipment :eid eid :slots equip}])
        [{:packet :bundle-delimiter}]))))

(defn tracking-packets
  "Returns the packets that spawn the new entities of a :tracking
  delta, one bundle each."
  [world [_ _ add _]]
  (mapcat #(spawn-packets world %) add))
