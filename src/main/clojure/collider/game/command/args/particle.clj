(ns collider.game.command.args.particle
  "The particle argument, a particle type and its options as an SNBT
  compound decoded by the type."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.data.particles :as particles]
            [collider.game.command.args :as args]
            [collider.game.command.decode :as dfu]
            [collider.game.command.reader :as r]
            [collider.game.command.snbt :as snbt]
            [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(defn- ok? [res] (= :ok (first res)))

(defn- or-else
  "Returns result x, or what alt decodes tag to when x fails, or the
  error of both."
  [x alt tag]
  (if (ok? x)
    x
    (let [y (alt tag)] (if (ok? y) y (dfu/either x y)))))

(defn- channel ^long [x]
  (let [v (unchecked-float (* (double (unchecked-float x)) 255.0))]
    (bit-and 0xFF (unchecked-int (Math/floor v)))))

(defn- as-list [tag]
  (cond (vector? tag) tag
        (and (some? tag) (.isArray (class tag))) (vec tag)))

(defn- list-result [n good bad]
  (let [size (when-not (= n (count good))
               (str "Input is not a list of " n " elements"))
        why (concat (repeat bad "Not a number") (when size [size]))]
    (cond (empty? why) [:ok good]
          (<= n (count good)) [:partial (str/join "; " why)]
          :else [:malformed (str/join "; " why)])))

(defn- number-list
  "Decodes list tag to n numbers through f as a codec of a list cut to
  a fixed size does. A longer list keeps its first n as a partial
  result, a shorter one fails, and each element that is no number
  adds its error."
  [f n tag]
  (if-let [xs (as-list tag)]
    (list-result n (mapv f (filter number? xs))
                 (count (remove number? xs)))
    (dfu/not-a "list" tag)))

(def ^:private float-list
  (partial number-list #(double (unchecked-float %))))

(defn- packed [n v]
  (let [[a cs] (if (= 4 n) [(peek v) (pop v)] [1.0 v])
        add #(bit-or (bit-shift-left %1 8) (channel %2))]
    (long (unchecked-int (reduce add (channel a) cs)))))

(defn- color [n tag]
  (if (number? tag)
    [:ok (long (unchecked-int tag))]
    (let [[op v :as res] (float-list n tag)]
      (if (= :ok op)
        [:ok (packed n v)]
        (dfu/either [:malformed "Not a number"] res)))))

(def ^:private rgb (partial color 3))

(def ^:private argb (partial color 4))

(def ^:private scale
  (dfu/checked dfu/float-of
               #(<= (double (float 0.01)) % (double (float 4.0)))
               #(str "Value must be within range [0.01;4.0]: "
                     (float %))))

(defn- prop-str [v]
  (if (keyword? v) (data/snake (name v)) (str v)))

(defn- prop-of [props [k vs]]
  (let [tag (get props (keyword (data/snake (name k))))]
    (when (string? tag)
      (some #(when (= tag (prop-str %)) [k %]) vs))))

(defn- state-of [k props]
  (block/state k (when (map? props)
                   (into {} (keep #(prop-of props %))
                         (:props (data/info k))))))

(def ^:private block-name (dfu/by-name "block"))

(defn- no-key [k tag]
  [:malformed (str "No key " k " in MapLike[" (dfu/printed tag) "]")])

(defn- full-state [tag]
  (cond (not (map? tag)) (dfu/not-map tag)
        (nil? (:Name tag)) (no-key "Name" tag)
        :else ((dfu/mapped block-name #(state-of % (:Properties tag)))
               (:Name tag))))

(defn- named-state [tag]
  (if (string? tag)
    ((dfu/mapped block-name block/state) tag)
    [:malformed "Not a string"]))

(defn- block-state [tag]
  (or-else (full-state tag) named-state tag))

(def ^:private air-free
  (dfu/checked (dfu/by-name "item") #(not= :air %)
               (fn [_] "Item must not be minecraft:air")))

(def ^:private item-count
  (dfu/checked dfu/int-of #(<= 1 % 99)
               #(str "Value must be within range [1;99]: " %)))

(def ^:private item-map
  (dfu/mapped (dfu/record [:id :item air-free :req]
                          [:count :count item-count :opt 1])
              #(assoc % :patch nil)))

(def ^:private item-name
  (dfu/mapped air-free (fn [k] {:item k :count 1 :patch nil})))

(defn- item [tag]
  (let [x (item-map tag)]
    (if (and (map? tag) (not (ok? x)) (ok? (air-free (:id tag))))
      x
      (or-else x item-name tag))))

(def ^:private int-array-class (Class/forName "[I"))

(defn- block-pos [tag]
  (cond (not (or (instance? int-array-class tag)
                 (and (vector? tag) (every? number? tag))))
        [:malformed (str "Not an int array: " (dfu/printed tag))]
        (not= 3 (count tag))
        [:malformed "Input is not a list of 3 ints"]
        :else [:ok (mapv #(long (unchecked-int %)) (vec tag))]))

(def ^:private source-type (dfu/by-name "position_source_type"))

(def ^:private block-source
  (dfu/mapped (dfu/record [:pos :pos block-pos :req])
              #(vector :block (:pos %))))

(defn- source [tag]
  (let [[op v :as res] (when (map? tag) (source-type (:type tag)))]
    (cond (not (map? tag)) (dfu/not-map tag)
          (nil? (:type tag)) (no-key "type" tag)
          (not= :ok op) res
          (= :entity v)
          [:malformed "Entity position sources are not allowed"]
          :else (block-source tag))))

(def ^:private vec3 (partial number-list double 3))

(defn- req [k f] [k k f :req])

(defn- opt [k f d] [k k f :opt d])

(defn- options-of
  "Returns the decoder of a record over fields, giving the value of
  its only field or the values of all in order."
  [& fields]
  (let [ks (mapv second fields)]
    (dfu/mapped (apply dfu/record fields)
                (if (next ks) (apply juxt ks) (first ks)))))

(def ^:private decoders
  {:none (fn [_] [:ok nil])
   :state (options-of (req :block_state block-state))
   :color (options-of (req :color argb))
   :power (options-of (opt :power dfu/float-of 1.0))
   :spell (options-of (opt :color rgb -1)
                      (opt :power dfu/float-of 1.0))
   :dust (options-of (req :color rgb) (req :scale scale))
   :transition (options-of (req :from_color rgb) (req :to_color rgb)
                           (req :scale scale))
   :roll (options-of (req :roll dfu/float-of))
   :item (options-of (req :item item))
   :vibration (options-of (req :destination source)
                          (req :arrival_in_ticks dfu/int-of))
   :delay (options-of (req :delay dfu/int-of))
   :geyser (options-of (req :water_blocks dfu/positive-int))
   :geyser-base (options-of (req :water_blocks dfu/positive-int)
                            (req :burst_impulse_base dfu/float-of))
   :trail (options-of (req :target vec3) (req :color rgb)
                      (req :duration dfu/positive-int))})

(def ^:private ^:table type-index
  (delay (into {} (map (fn [k] [(data/wire k) k]))
               (keys (get (data/registries) "particle_type")))))

(defn- options-tag [[s n :as rd]]
  (if (and (< n (count s)) (= \{ (nth s n)))
    (snbt/read-tag rd)
    [{} rd]))

(defn- decoded [k [tag end]]
  (let [[op v] ((decoders (particles/kind k)) tag)]
    (if (= :ok op)
      [[(data/registry-id "particle_type" k) v] end]
      (r/error "particle.invalidOptions" (or v (dfu/printed tag))))))

(defn- options [k rd]
  (let [res (options-tag rd)]
    (if (r/error? res) res (decoded k res))))

(defn- read-particle [rd]
  (let [res (args/read-id rd)
        [id end] (when-not (r/error? res) res)
        k (when id (@type-index id))]
    (cond (r/error? res) res
          k (options k end)
          :else (r/error-at end "particle.notFound" id))))

(defn particle-arg []
  {:id "minecraft:particle" :parse read-particle})
