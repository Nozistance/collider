(ns collider.game.block.sign
  "Signs, the text on each side and its changes."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(def ^:private kinds
  {:standing-sign        :sign
   :wall-sign            :sign
   :ceiling-hanging-sign :hanging-sign
   :wall-hanging-sign    :hanging-sign})

(defn kind
  "Returns the block entity kind of sign state st, or nil."
  [^long st]
  (get kinds (block/type-of st)))

(def ^:private empty-text
  {:lines ["" "" "" ""] :color :black :glowing? false})

(defn fresh
  "Returns a blank sign of kind k that player editor writes."
  [k editor]
  {:kind k :front empty-text :back empty-text :waxed? false
   :editor editor})

(defn- text-nbt [t]
  {:messages         (vec (:lines t))
   :color            (data/snake (:color t))
   :has_glowing_text (boolean (:glowing? t))})

(defn nbt
  [e]
  {:front_text (text-nbt (:front e))
   :back_text  (text-nbt (:back e))
   :is_waxed   (boolean (:waxed? e))})

(def ^:private wall-rot {:south 0 :west 90 :north 180 :east 270})

(defn- y-rot ^double [^long st]
  (case (block/type-of st)
    (:standing-sign :ceiling-hanging-sign)
    (let [d (* 22.5 (block/prop-long st :rotation))]
      (if (>= d 180.0) (- d 360.0) d))
    (double (wall-rot (block/facing-of st)))))

(def ^:private wall-centres
  {:north [0.5 0.53 0.9375]
   :south [0.5 0.53 0.0625]
   :west  [0.9375 0.53 0.5]})

(def ^:private east-centre [0.0625 0.53 0.5])

(defn- hit-centre [^long st]
  (if (= :wall-sign (block/type-of st))
    (get wall-centres (block/facing-of st) east-centre)
    [0.5 0.5 0.5]))

(defn- degrees-difference ^double [^double a ^double b]
  (let [d (mod (- b a) 360.0)]
    (Math/abs (double (if (>= d 180.0) (- d 360.0) d)))))

(defn front?
  "Returns true when a player at player-pos faces the front of sign
  st at pos."
  [^long st [x _ z] player-pos]
  (let [[cx _ cz] (hit-centre st)
        dx (- (double (nth player-pos 0)) (+ (long x) (double cx)))
        dz (- (double (nth player-pos 2)) (+ (long z) (double cz)))
        player-rot (- (Math/toDegrees (Math/atan2 dz dx)) 90.0)]
    (<= (degrees-difference (y-rot st) player-rot) 90.0)))

(defn- side [front?] (if front? :front :back))

(defn- strip-formatting [^String s]
  (str/replace s #"(?i)\u00a7[0-9a-fk-or]" ""))

(defn- four-lines [lines]
  (mapv #(strip-formatting (str %))
        (take 4 (concat lines (repeat "")))))

(defn written
  "Returns sign e with lines on the side front? names, closed to its
  editor."
  [e front? lines]
  (-> e
      (assoc-in [(side front?) :lines] (four-lines lines))
      (assoc :editor nil)))

(defn- dyed [e s color]
  (when (not= color (get-in e [s :color]))
    [(assoc-in e [s :color] color) :dye/use]))

(defn- glowed [e s glowing? sound]
  (when (not= glowing? (boolean (get-in e [s :glowing?])))
    [(assoc-in e [s :glowing?] glowing?) sound]))

(defn applied
  "Returns sign e after item is used on the side front? names, with
  the sound it makes, or nil when nothing changes. Honeycomb returns
  :wax in place of a sound."
  [e front? item]
  (let [s (side front?)]
    (if-let [color (data/dye-color item)]
      (dyed e s color)
      (case item
        :glow-ink-sac (glowed e s true :glow-ink/use)
        :ink-sac (glowed e s false :ink-sac/use)
        :honeycomb (when-not (:waxed? e)
                     [(assoc e :waxed? true) :wax])
        nil))))
