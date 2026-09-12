(ns collider.game.block.sign
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private kinds
  {:standing-sign :sign :wall-sign :sign :ceiling-hanging-sign :hanging-sign :wall-hanging-sign :hanging-sign})

(defn kind [^long st] (get kinds (block/type-of st)))
(def empty-text {:lines ["" "" "" ""] :color :black :glowing? false})
(defn fresh [kind editor]
  {:kind kind :front empty-text :back empty-text :waxed? false :editor editor})

(defn at [world pos] (get-in world [:block-entities (chunk/block-chunk pos) pos]))
(defn type-id ^long [e] (data/registry-id "block_entity_type" (:kind e)))
(defn- text-nbt [t]
  {:messages (vec (:lines t))
   :color (data/snake (:color t))
   :has_glowing_text (boolean (:glowing? t))})

(defn nbt [e]
  {:front_text (text-nbt (:front e)) :back_text (text-nbt (:back e)) :is_waxed (boolean (:waxed? e))})

(defn wire [entries]
  (into {} (map (fn [[pos e]] [pos {:type (type-id e) :nbt (nbt e)}])) entries))

(defn- y-rot ^double [^long st]
  (case (block/type-of st)
    (:standing-sign :ceiling-hanging-sign)
    (let [d (* 22.5 (block/prop-long st :rotation))]
      (if (>= d 180.0) (- d 360.0) d))
    (double ({:south 0 :west 90 :north 180 :east 270} (block/facing-of st)))))

(defn- hit-center [^long st]
  (if (= :wall-sign (block/type-of st))
    (case (block/facing-of st) :north [0.5 0.53 0.9375] :south [0.5 0.53 0.0625] :west [0.9375 0.53 0.5] [0.0625 0.53 0.5])
    [0.5 0.5 0.5]))

(defn- degrees-difference ^double [^double a ^double b]
  (let [d (mod (- b a) 360.0)] (Math/abs (double (if (>= d 180.0) (- d 360.0) d)))))

(defn front? [^long st [x _ z] player-pos]
  (let [[cx _ cz] (hit-center st)
        dx (- (double (nth player-pos 0)) (+ (long x) (double cx)))
        dz (- (double (nth player-pos 2)) (+ (long z) (double cz)))
        player-rot (- (Math/toDegrees (Math/atan2 dz dx)) 90.0)]
    (<= (degrees-difference (y-rot st) player-rot) 90.0)))

(defn side [front?] (if front? :front :back))
(defn strip-formatting [^String s]
  (str/replace s #"(?i)\u00a7[0-9a-fk-or]" ""))

(defn written [e front? lines]
  (-> e
      (assoc-in [(side front?) :lines] (mapv #(strip-formatting (str %)) (take 4 (concat lines (repeat "")))))
      (assoc :editor nil)))

(defn applied [e front? item]
  (let [s (side front?) text (get e s)]
    (cond
      (data/dye-color item) (when (not= (data/dye-color item) (:color text)) [(assoc-in e [s :color] (data/dye-color item)) :dye/use])
      (= :glow-ink-sac item) (when-not (:glowing? text) [(assoc-in e [s :glowing?] true) :glow-ink/use])
      (= :ink-sac item) (when (:glowing? text) [(assoc-in e [s :glowing?] false) :ink-sac/use])
      (= :honeycomb item) (when-not (:waxed? e) [(assoc e :waxed? true) :wax]))))
