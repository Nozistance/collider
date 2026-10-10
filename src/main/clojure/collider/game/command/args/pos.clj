(ns collider.game.command.args.pos
  "Position, column and rotation arguments of commands, and where they
  point for a command source."
  (:require [clojure.string :as str]
            [collider.game.command.reader :as r]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defn- axis [kind v] {:kind kind :value (double v)})

(def ^:private flat (axis :relative 0))

(defn- read-field [rd read]
  (if (and (r/can-read? rd) (not (r/at? rd \space)))
    (read rd)
    [0.0 rd]))

(defn- centered? [center? [s n] [_ e]]
  (and center? (not (str/includes? (subs s n e) "."))))

(defn- world-value [rel? v center? at end]
  (cond rel? (axis :relative v)
        (centered? center? at end) (axis :world (+ (double v) 0.5))
        :else (axis :world v)))

(defn- coord [read missing center? rd]
  (cond (r/at? rd \^) (r/error-at rd "argument.pos.mixed")
        (not (r/can-read? rd)) (r/error-at rd missing)
        :else
        (let [rel? (r/at? rd \~)
              at (if rel? (r/skip rd) rd)
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
            (r/at? end \space)
            (recur (r/skip end) (rest ps) (conj acc v))
            :else (r/error-at start incomplete)))))

(defn- local-field [start]
  (fn [rd]
    (cond (not (r/can-read? rd))
          (r/error-at rd "argument.pos.missing.double")
          (not (r/at? rd \^)) (r/error-at start "argument.pos.mixed")
          :else
          (let [res (read-field (r/skip rd) r/read-double)]
            (if (r/error? res)
              res
              [(axis :local (first res)) (second res)])))))

(def ^:private incomplete-3d "argument.pos3d.incomplete")

(defn- local-coords [rd]
  (fields rd (repeat 3 (local-field rd)) incomplete-3d))

(defn block-pos-arg []
  {:id "minecraft:block_pos"
   :parse (fn [rd]
            (if (r/at? rd \^)
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
             (if (r/at? rd \^)
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
        yc (v/cos ya) ys (v/sin ya)
        xc (v/cos xa) xs (v/sin xa)
        uc (v/cos ua) us (v/sin ua)
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
