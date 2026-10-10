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

(defn- channel ^long [x]
  (let [v (unchecked-float (* (double (unchecked-float x)) 255.0))]
    (bit-and 0xFF (unchecked-int (Math/floor v)))))

(defn- number-list
  "Decodes list tag to n numbers through f as a codec of a list cut to
  a fixed size does. A longer list keeps its first n as a partial
  result, a shorter one fails, and each element that is no number
  adds its error."
  [f n tag]
  (if-let [tag (cond (vector? tag) tag
                     (and (some? tag) (.isArray (class tag))) (vec tag))]
    (let [good (mapv f (filter number? tag))
          bad (count (remove number? tag))
          size (when-not (= n (count good))
                 (str "Input is not a list of " n " elements"))
          why (seq (concat (repeat bad "Not a number") (when size [size])))]
      (cond (nil? why) [:ok good]
            (<= n (count good)) [:partial (str/join "; " why)]
            :else [:malformed (str/join "; " why)]))
    (dfu/not-a "list" tag)))

(def ^:private float-list
  (partial number-list #(double (unchecked-float %))))

(defn- color [n tag]
  (if (number? tag)
    [:ok (long (unchecked-int tag))]
    (let [[op v :as r] (float-list n tag)]
      (if (= :ok op)
        (let [[a cs] (if (= 4 n) [(peek v) (pop v)] [1.0 v])]
          [:ok (long (unchecked-int
                       (reduce #(bit-or (bit-shift-left %1 8) (channel %2))
                               (channel a) cs)))])
        (dfu/either [:malformed "Not a number"] r)))))

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
  (let [wanted (when (map? props)
                 (into {} (keep #(prop-of props %))
                       (:props (data/info k))))]
    (block/state k wanted)))

(def ^:private block-name (dfu/by-name "block"))

(defn- full-state [tag]
  (if (map? tag)
    (if-some [nm (:Name tag)]
      (let [[op v :as r] (block-name nm)]
        (if (= :ok op) [:ok (state-of v (:Properties tag))] r))
      [:malformed (str "No key Name in MapLike[" (dfu/printed tag) "]")])
    (dfu/not-map tag)))

(defn- block-state [tag]
  (let [x (full-state tag)]
    (if (= :ok (first x))
      x
      (let [[op v :as y] (if (string? tag)
                           (block-name tag)
                           [:malformed "Not a string"])]
        (if (= :ok op)
          [:ok (block/state v)]
          (dfu/either x y))))))

(def ^:private air-free
  (dfu/checked (dfu/by-name "item") #(not= :air %)
               (fn [_] "Item must not be minecraft:air")))

(def ^:private item-count
  (dfu/checked dfu/int-of #(<= 1 % 99)
               #(str "Value must be within range [1;99]: " %)))

(def ^:private item-map
  (dfu/record [:id :item air-free :req]
              [:count :count item-count :opt 1]))

(defn- item [tag]
  (let [x (item-map tag)]
    (cond (= :ok (first x)) (update x 1 assoc :patch nil)
          (and (map? tag) (= :ok (first (air-free (:id tag))))) x
          :else
          (let [[op v :as y] (air-free tag)]
            (if (= :ok op)
              [:ok {:item v :count 1 :patch nil}]
              (dfu/either x y))))))

(def ^:private int-array-class (Class/forName "[I"))

(defn- block-pos [tag]
  (if (or (instance? int-array-class tag)
          (and (vector? tag) (every? number? tag)))
    (if (= 3 (count tag))
      [:ok (mapv #(long (unchecked-int %)) (vec tag))]
      [:malformed "Input is not a list of 3 ints"])
    [:malformed (str "Not an int array: " (dfu/printed tag))]))

(def ^:private source-type (dfu/by-name "position_source_type"))

(defn- source [tag]
  (if (map? tag)
    (let [[op v :as r] (source-type (:type tag))]
      (cond (nil? (:type tag))
            [:malformed (str "No key type in MapLike["
                             (dfu/printed tag) "]")]
            (not= :ok op) r
            (= :entity v) [:malformed "Entity position sources are not allowed"]
            :else (let [[o p :as pr] ((dfu/record [:pos :pos block-pos :req])
                                      tag)]
                    (if (= :ok o) [:ok [:block (:pos p)]] pr))))
    (dfu/not-map tag)))

(def ^:private vec3 (partial number-list double 3))

(defn- shaped [rec f]
  (dfu/mapped rec f))

(def ^:private decoders
  {:none (fn [_] [:ok nil])
   :state (shaped (dfu/record [:block_state :v block-state :req]) :v)
   :color (shaped (dfu/record [:color :v argb :req]) :v)
   :power (shaped (dfu/record [:power :v dfu/float-of :opt 1.0]) :v)
   :spell (shaped (dfu/record [:color :c rgb :opt -1]
                              [:power :p dfu/float-of :opt 1.0])
                  (juxt :c :p))
   :dust (shaped (dfu/record [:color :c rgb :req] [:scale :s scale :req])
                 (juxt :c :s))
   :transition (shaped (dfu/record [:from_color :a rgb :req]
                                   [:to_color :b rgb :req]
                                   [:scale :s scale :req])
                       (juxt :a :b :s))
   :roll (shaped (dfu/record [:roll :v dfu/float-of :req]) :v)
   :item (shaped (dfu/record [:item :v item :req]) :v)
   :vibration (shaped (dfu/record [:destination :d source :req]
                                  [:arrival_in_ticks :t dfu/int-of :req])
                      (juxt :d :t))
   :delay (shaped (dfu/record [:delay :v dfu/int-of :req]) :v)
   :geyser (shaped (dfu/record [:water_blocks :v dfu/positive-int :req])
                   :v)
   :geyser-base (shaped (dfu/record [:water_blocks :w dfu/positive-int :req]
                                    [:burst_impulse_base :i dfu/float-of
                                     :req])
                        (juxt :w :i))
   :trail (shaped (dfu/record [:target :t vec3 :req]
                              [:color :c rgb :req]
                              [:duration :d dfu/positive-int :req])
                  (juxt :t :c :d))})

(def ^:private ^:table type-index
  (delay (into {} (map (fn [k] [(data/wire k) k]))
               (keys (get (data/registries) "particle_type")))))

(defn- options [k [s n :as rd]]
  (let [res (if (and (< n (count s)) (= \{ (nth s n)))
              (snbt/read-tag rd)
              [{} rd])]
    (if (r/error? res)
      res
      (let [[tag end] res
            [op v] ((decoders (particles/kind k)) tag)]
        (if (= :ok op)
          [[(data/registry-id "particle_type" k) v] end]
          (r/error "particle.invalidOptions" (or v (dfu/printed tag))))))))

(defn- read-particle [rd]
  (let [res (args/read-id rd)]
    (if (r/error? res)
      res
      (let [[id end] res]
        (if-let [k (@type-index id)]
          (options k end)
          (r/error-at end "particle.notFound" id))))))

(defn particle-arg []
  {:id "minecraft:particle" :parse read-particle})
