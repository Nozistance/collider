(ns collider.game.command.args
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.command.reader :as r]))

(set! *warn-on-reflection* true)

(defn- at? [[s n :as rd] c]
  (and (r/can-read? rd) (= c (nth s n))))

(defn- skip [[s n]] [s (inc n)])

(defn- axis [kind v] {:kind kind :value (double v)})

(def ^:private flat (axis :relative 0))

(defn- read-field [rd read]
  (if (and (r/can-read? rd) (not (at? rd \space)))
    (read rd)
    [0.0 rd]))

(defn- centered? [center? [s n] [_ e]]
  (and center? (not (str/includes? (subs s n e) "."))))

(defn- world-value [rel? v center? at end]
  (cond rel? (axis :relative v)
        (centered? center? at end) (axis :world (+ (double v) 0.5))
        :else (axis :world v)))

(defn- coord [read missing center? rd]
  (cond (at? rd \^) (r/error-at rd "argument.pos.mixed")
        (not (r/can-read? rd)) (r/error-at rd missing)
        :else
        (let [rel? (at? rd \~)
              at (if rel? (skip rd) rd)
              res (read-field at (if rel? r/read-double read))]
          (if (r/error? res)
            res
            (let [[v end] res]
              [(world-value rel? v center? at end) end])))))

(defn- int-coord [rd]
  (coord r/read-int "argument.pos.missing.int" false rd))

(defn- double-coord [center?]
  (partial coord r/read-double "argument.pos.missing.double"
           center?))

(defn- fields [start parsers incomplete]
  (loop [rd start ps parsers acc []]
    (let [res ((first ps) rd)
          [v end] (when-not (r/error? res) res)]
      (cond (r/error? res) res
            (empty? (rest ps)) [(conj acc v) end]
            (at? end \space) (recur (skip end) (rest ps) (conj acc v))
            :else (r/error-at start incomplete)))))

(defn- local-field [start]
  (fn [rd]
    (cond (not (r/can-read? rd))
          (r/error-at rd "argument.pos.missing.double")
          (not (at? rd \^)) (r/error-at start "argument.pos.mixed")
          :else
          (let [res (read-field (skip rd) r/read-double)]
            (if (r/error? res)
              res
              [(axis :local (first res)) (second res)])))))

(def ^:private incomplete-3d "argument.pos3d.incomplete")

(defn- local-coords [rd]
  (fields rd (repeat 3 (local-field rd)) incomplete-3d))

(defn block-pos-arg []
  {:id "minecraft:block_pos"
   :parse (fn [rd]
            (if (at? rd \^)
              (local-coords rd)
              (fields rd (repeat 3 int-coord) incomplete-3d)))})

(defn- vec3-fields [center?]
  [(double-coord center?) (double-coord false)
   (double-coord center?)])

(defn vec3-arg
  ([] (vec3-arg true))
  ([center?]
   {:id "minecraft:vec3" :center center?
    :parse (fn [rd]
             (if (at? rd \^)
               (local-coords rd)
               (fields rd (vec3-fields center?) incomplete-3d)))}))

(defn- pair [rd parsers incomplete build]
  (if-not (r/can-read? rd)
    (r/error-at rd incomplete)
    (let [res (fields rd parsers incomplete)]
      (if (r/error? res) res (update res 0 build)))))

(defn- column [[x z]] [x flat z])

(defn vec2-arg
  ([] (vec2-arg true))
  ([center?]
   {:id "minecraft:vec2" :center center?
    :parse #(pair % (repeat 2 (double-coord center?))
                  "argument.pos2d.incomplete" column)}))

(defn column-pos-arg []
  {:id "minecraft:column_pos"
   :parse #(pair % (repeat 2 int-coord)
                 "argument.pos2d.incomplete" column)})

(defn rotation-arg []
  {:id "minecraft:rotation"
   :parse #(pair % (repeat 2 (double-coord false))
                 "argument.rotation.incomplete" identity)})

(def ^:private ^:const sin-scale 10430.378350470453)

(def ^:private sin-table
  (let [a (float-array 65536)]
    (dotimes [i 65536]
      (aset a i (unchecked-float
                 (Math/sin (/ (double i) sin-scale)))))
    a))

(defn- sin-at ^double [^double i]
  (let [k (bit-and (unchecked-long i) 65535)]
    (aget ^floats sin-table (unchecked-int k))))

(defn- mth-sin ^double [^double a] (sin-at (* a sin-scale)))

(defn- mth-cos ^double [^double a]
  (sin-at (+ (* a sin-scale) 16384.0)))

(def ^:private deg (unchecked-float (/ Math/PI 180.0)))

(defn- f* ^double [^double a ^double b] (unchecked-float (* a b)))

(defn- f+ ^double [^double a ^double b] (unchecked-float (+ a b)))

(defn- cross [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by)) (- (* az bx) (* ax bz))
   (- (* ax by) (* ay bx))])

(defn- basis [yaw pitch]
  (let [ya (f* (f+ yaw 90.0) deg)
        xa (f* (- (double pitch)) deg)
        ua (f* (f+ (- (double pitch)) 90.0) deg)
        yc (mth-cos ya) ys (mth-sin ya)
        xc (mth-cos xa) xs (mth-sin xa)
        uc (mth-cos ua) us (mth-sin ua)
        fw [(f* yc xc) xs (f* ys xc)]
        up [(f* yc uc) us (f* ys uc)]]
    [fw up (mapv #(* (double %) -1.0) (cross fw up))]))

(defn local-offset [[yaw pitch] [left up forwards]]
  (let [[fw u l] (basis yaw pitch)
        dx (double left) dy (double up) dz (double forwards)]
    (mapv (fn [i]
            (+ (+ (* (double (fw i)) dz) (* (double (u i)) dy))
               (* (double (l i)) dx)))
          [0 1 2])))

(defn- anchor [{:keys [pos anchor eye-height]}]
  (if (and (= anchor :eyes) eye-height)
    (update pos 1 #(+ (double %) (double eye-height)))
    pos))

(defn- world-get [{:keys [kind value]} base]
  (if (= kind :relative)
    (+ (double value) (double base))
    (double value)))

(defn position [coords src]
  (if (= :local (:kind (first coords)))
    (mapv #(+ (double %1) (double %2))
          (local-offset (:rot src) (mapv :value coords))
          (anchor src))
    (mapv world-get coords (:pos src))))

(defn- floor-int [x] (unchecked-int (Math/floor (double x))))

(defn block-pos [coords src]
  (mapv floor-int (position coords src)))

(defn column-pos [coords src]
  (let [[x _ z] (block-pos coords src)] [x z]))

(defn vec2 [coords src]
  (let [[x _ z] (position coords src)]
    [(unchecked-float x) (unchecked-float z)]))

(defn rotation [coords src]
  (if (= :local (:kind (first coords)))
    [(unchecked-float 0.0) (unchecked-float 0.0)]
    (mapv #(Float/valueOf (unchecked-float (world-get %1 %2)))
          coords (:rot src))))

(defn- horizontal? [[x _ z]]
  (and (<= -30000000 x) (< x 30000000)
       (<= -30000000 z) (< z 30000000)))

(defn loaded-block-pos [[_ y :as pos] {:keys [loaded? min-y max-y]}]
  (cond (not (loaded? pos)) (r/error "argument.pos.unloaded")
        (not (and (<= min-y y max-y) (horizontal? pos)))
        (r/error "argument.pos.outofworld")
        :else pos))

(defn spawnable-pos [[_ y :as pos]]
  (if (and (<= -20000000 y) (< y 20000000) (horizontal? pos))
    pos
    (r/error "argument.pos.outofbounds")))

(defn- between? [c lo hi] (<= (int lo) (int c) (int hi)))

(defn- path-char? [c]
  (or (between? c \a \z) (between? c \0 \9)
      (= c \_) (= c \-) (= c \.) (= c \/)))

(defn- ns-char? [c] (and (not= c \/) (path-char? c)))

(defn- id-char? [c] (or (= c \:) (path-char? c)))

(defn- identifier [raw]
  (let [i (str/index-of raw ":")
        nm (if (and i (pos? i)) (subs raw 0 i) "minecraft")
        path (if i (subs raw (inc i)) raw)]
    (when (and (not= nm "..") (every? ns-char? nm)
               (every? path-char? path))
      (str nm ":" path))))

(defn- read-raw [[s n]]
  (let [len (count s)]
    (loop [i n]
      (if (and (< i len) (id-char? (nth s i)))
        (recur (inc i))
        [(subs s n i) [s i]]))))

(defn read-id [rd]
  (let [[raw end] (read-raw rd)]
    (if-let [id (identifier raw)]
      [id end]
      (r/error-at rd "argument.id.invalid"))))

(defn id-arg [] {:id "minecraft:resource_location" :parse read-id})

(defn dimension-arg [] {:id "minecraft:dimension" :parse read-id})

(defn dimension [id levels]
  (or (some #(when (= id (data/wire %)) %) levels)
      (r/error "argument.dimension.invalid" id)))

(defn- entries [registry]
  (or (keys (get (data/registries) registry))
      (get (data/datapack) registry)))

(defn- wire-index [registry]
  (into {} (map (fn [k] [(data/wire k) k])) (entries registry)))

(def ^:private not-found "argument.resource.not_found")

(defn- registry-id [registry] (str "minecraft:" registry))

(defn- not-found-at [end id registry]
  (r/error-at end not-found id (registry-id registry)))

(defn- find-resource [index registry rd]
  (let [res (read-id rd)
        k (when-not (r/error? res) (@index (first res)))]
    (cond (r/error? res) res
          k (assoc res 0 k)
          :else (not-found-at (second res) (first res) registry))))

(defn resource-arg [registry]
  (let [index (delay (wire-index registry))]
    {:id "minecraft:resource" :registry registry
     :parse (partial find-resource index registry)}))

(def ^:private units {"d" 24000 "s" 20 "t" 1 "" 1})

(def ^:private time-unit "argument.time.invalid_unit")

(def ^:private time-low "argument.time.tick_count_too_low")

(defn- ticks [v factor]
  (Integer/valueOf
   (Math/round (unchecked-float (* (double v) (double factor))))))

(defn- read-time [lo rd]
  (let [res (r/read-float rd)]
    (if (r/error? res)
      res
      (let [[unit end] (r/read-unquoted (second res))
            factor (units unit)
            t (when factor (ticks (first res) factor))]
        (cond (nil? factor) (r/error-at end time-unit)
              (< t lo) (r/error-at end time-low lo t)
              :else [t end])))))

(defn time-arg
  ([] (time-arg 0))
  ([lo] {:id "minecraft:time" :min lo :parse (partial read-time lo)}))
