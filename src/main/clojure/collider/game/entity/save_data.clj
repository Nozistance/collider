(ns collider.game.entity.save-data
  "Entities as the tags of a vanilla save."
  (:require [collider.data :as data]
            [collider.game.mob.mobs :as mobs]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defn- flag [b] (byte (if b 1 0)))

(defn- on? [x] (and (number? x) (not (zero? (long x)))))

(defn- left [e k t]
  (when-let [at (get e k)] (- (long at) (long t))))

(defn- numbers [x n]
  (when (and (vector? x) (= n (count x)) (every? number? x))
    (mapv double x)))

(defn- clamp ^double [^double lo ^double hi ^double x]
  (max lo (min hi x)))

(defn- xyz [p] [(v/x p) (v/y p) (v/z p)])

(def ^:private ^:const far-xz 3.0000512E7)

(def ^:private ^:const far-y 2.0E7)

(defn- within [[a b c]]
  [(clamp (- far-xz) far-xz a) (clamp (- far-y) far-y b)
   (clamp (- far-xz) far-xz c)])

(defn- placed [e x _]
  (if-let [p (numbers x 3)] (assoc e :pos (v/v3 (within p))) e))

(defn- calm ^double [^double c] (if (> (Math/abs c) 10.0) 0.0 c))

(defn- moved [e x _]
  (if-let [m (numbers x 3)] (assoc e :vel (v/v3 (mapv calm m))) e))

(defn- f32 ^double [x] (double (unchecked-float x)))

(defn- turned [e x _]
  (if-let [[yaw pitch] (numbers x 2)]
    (assoc e :yaw (f32 yaw) :head-yaw (f32 yaw) :pitch (f32 pitch))
    e))

(defn- turn [e]
  [(float (or (:yaw e) 0.0)) (float (or (:pitch e) 0.0))])

(defn- tagged [e x _]
  (if (and (vector? x) (every? string? x)) (assoc e :tags (set x)) e))

(def ^:private entity-fields
  {:Pos [(fn [e _] (xyz (:pos e))) placed]
   :Motion [(fn [e _] (xyz (or (:vel e) [0.0 0.0 0.0]))) moved]
   :Rotation [(fn [e _] (turn e)) turned]
   :OnGround [(fn [e _] (flag (:on-ground e)))
              (fn [e x _] (assoc e :on-ground (on? x)))]
   :Tags [(fn [e _] (when (seq (:tags e)) (vec (:tags e))))
          tagged]
   :CustomName [(fn [e _] (:custom-name e))
                (fn [e x _]
                  (if (or (string? x) (map? x)) (assoc e :custom-name x) e))]
   :CustomNameVisible [(fn [e _] (when (:custom-name-visible e) (flag true)))
                       (fn [e x _] (assoc e :custom-name-visible (on? x)))]
   :TicksFrozen
   [(fn [e _]
      (let [n (long (or (:ticks-frozen e) 0))] (when (pos? n) (int n))))
    (fn [e x _]
      (if (number? x) (assoc e :ticks-frozen (long (unchecked-int x))) e))]})

(defn- numeric [f]
  (fn [e x t] (if (number? x) (f e x t) e)))

(defn- int-field [k]
  [(fn [e _] (int (or (get e k) 0)))
   (numeric (fn [e x _] (assoc e k (long (unchecked-int x)))))])

(defn- flag-field [k]
  [(fn [e _] (flag (get e k)))
   (fn [e x _] (assoc e k (on? x)))])

(defn- health [e x _]
  (let [top (mobs/max-health (:type e))]
    (assoc e :health (clamp 0.0 top (f32 x)))))

(defn- dying [e x _]
  (assoc e :death-time (long (unchecked-short x))))

(def ^:private living-fields
  {:Health [(fn [e _] (float (or (:health e) 0.0))) (numeric health)]
   :DeathTime [(fn [e _] (short (or (:death-time e) 0)))
               (numeric dying)]})

(defn- age
  "Returns the age of mob e at tick t as a save keeps it, below zero
  for a baby and above zero while it may not breed again. A locked
  baby stays as old as it was when its age was locked."
  ^long [e t]
  (if-let [until (:baby-until e)]
    (let [now (if (:age-locked? e) (or (:age-lock-at e) t) t)]
      (min -1 (- (long now) (long until))))
    (max 0 (long (or (left e :breed-ready-at t) 0)))))

(defn- aged [e x t]
  (let [a (long (unchecked-int x)) t (long t)]
    (assoc e :baby-until (when (neg? a) (- t a))
             :breed-ready-at (when (pos? a) (+ t a)))))

(defn- ticks-left [k absent]
  [(fn [e t] (if-let [n (left e k t)] (int (max 0 (long n))) absent))
   (numeric (fn [e x t]
              (let [n (long (unchecked-int x))]
                (assoc e k (when (pos? n) (+ (long t) n))))))])

(def ^:private animal-fields
  {:Age [(fn [e t] (int (age e t))) (numeric aged)]
   :ForcedAge (int-field :forced-age)
   :AgeLocked (flag-field :age-locked?)
   :InLove (ticks-left :love-until (int 0))})

(defn- named [k default xs]
  [(fn [e _] (data/wire (or (get e k) default)))
   (fn [e x _]
     (let [n (when (string? x) (data/kebab x))]
       (if (some #{n} xs) (assoc e k n) e)))])

(defn- voice [voices]
  [(fn [e _] (data/wire (voices (or (:sound-variant e) 0))))
   (fn [e x _]
     (let [n (when (string? x) (data/kebab x))
           i (.indexOf ^java.util.List voices n)]
       (if (neg? i) e (assoc e :sound-variant i))))])

(defn- coat-fields [type]
  (let [coats (get (data/datapack) (str (name type) "_variant"))]
    {:variant (named :variant :temperate coats)
     :sound_variant (voice (get-in mobs/types [type :voices]))}))

(defn- legacy-field [k ids cast]
  [(fn [e _] (cast (or (get e k) 0)))
   (numeric (fn [e x _]
              (let [i (long (cast x))]
                (assoc e k (if (ids i) i 0)))))])

(defn- horn [k]
  [(fn [e _] (flag (not (false? (get e k)))))
   (fn [e x _] (assoc e k (on? x)))])

(def ^:private mooshroom-types ["red" "brown"])

(defn- mooshroom-type [e x _]
  (let [i (.indexOf ^java.util.List mooshroom-types x)]
    (if (neg? i) e (assoc e :variant i))))

(defn- mushrooms [e _] (mooshroom-types (or (:variant e) 0)))

(def ^:private sheep-fields
  {:Color (legacy-field :color (set (range 16)) unchecked-byte)
   :Sheared (flag-field :sheared?)})

(def ^:private rabbit-fields
  {:RabbitType
   (legacy-field :variant (set (keys mobs/rabbit-variants))
                 unchecked-int)})

(def ^:private goat-fields
  {:IsScreamingGoat (flag-field :screaming?)
   :HasLeftHorn (horn :left-horn?)
   :HasRightHorn (horn :right-horn?)})

(defn- stated [e x _]
  (let [s (when (string? x) (keyword x))]
    (if (#{:idle :rolling :scared :unrolling} s)
      (assoc e :armadillo-state s)
      e)))

(def ^:private armadillo-fields
  {:state [(fn [e _] (name (or (:armadillo-state e) :idle))) stated]
   :scute_time (ticks-left :scute-at nil)})

(defn- kind-fields [type]
  (case type
    :sheep sheep-fields
    (:cow :pig) (coat-fields type)
    :chicken (assoc (coat-fields type)
                    :EggLayTime (ticks-left :egg-at nil))
    :mooshroom {:Type [mushrooms mooshroom-type]}
    :rabbit rabbit-fields
    :armadillo armadillo-fields
    :goat goat-fields))

(defn- fields [type]
  (cond (mobs/mob-type? type)
        (merge entity-fields living-fields animal-fields
               (kind-fields type))
        (= :player type) (merge entity-fields living-fields)
        :else entity-fields))

(defn saved
  "Returns the tag a vanilla save writes for entity e at tick t. It
  holds only what the entity keeps here."
  [e t]
  (reduce-kv (fn [m k [out]]
               (let [x (out e t)] (cond-> m (some? x) (assoc k x))))
             {} (fields (:type e))))

(defn loaded
  "Returns entity e at tick t with the values of tag on it. A value
  of the wrong kind leaves its field as it was, and so does a key
  the entity does not keep."
  [e tag t]
  (let [fs (fields (:type e))]
    (reduce-kv (fn [e k x]
                 (if-let [[_ in] (fs k)] (in e x t) e))
               e tag)))
