(ns collider.game.glide
  "Gliding on elytra, or on any glider worn where it is made to go."
  (:require [collider.data :as data]
            [collider.game.stack :as stack]
            [collider.num :as num]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defn look
  "Returns the unit vector a body looks along at pitch and yaw in
  degrees, at the precision of the game."
  [^double pitch ^double yaw]
  (let [xr (num/fmul (num/f32 pitch) num/deg->rad)
        yr (num/fmul (- (num/f32 yaw)) num/deg->rad)
        xc (v/cos xr)]
    [(num/fmul (v/sin yr) xc) (- (v/sin xr))
     (num/fmul (v/cos yr) xc)]))

(defn- boosted-axis ^double [^double m ^double l]
  (+ m (+ (* l 0.1) (* (- (* l 1.5) m) 0.5))))

(defn boosted
  "Returns speed vel of a glider that a rocket pushes along look."
  [vel [lx ly lz]]
  [(boosted-axis (v/x vel) lx) (boosted-axis (v/y vel) ly)
   (boosted-axis (v/z vel) lz)])

(def ^:private worn
  [[45 :offhand] [8 :feet] [7 :legs] [6 :chest] [5 :head]])

(defn- given [s k]
  (let [d (get-in (data/items) [(:item s) :defaults k])]
    (cond (contains? (:components s) k) [true (get-in s [:components k])]
          (contains? (:removed s) k) [false nil]
          (some? d) [true d]
          :else [false nil])))

(defn- slot-of [s]
  (let [[has? v] (given s :equippable)]
    (when has?
      (if (contains? (:components s) :equippable)
        (:slot v)
        (keyword (get v "slot"))))))

(defn- breaks-next? [s]
  (and (stack/damageable? s)
       (>= (stack/damage s) (dec (stack/max-damage s)))))

(defn- glider? [s slot]
  (and s (first (given s :glider)) (= slot (slot-of s))
       (not (breaks-next? s))))

(defn glider-slots
  "Returns the inventory slots of player e that hold a glider it can
  glide on, with the part each is worn on, in vanilla slot order."
  [e]
  (let [held (+ 36 (long (:held-slot e 0)))]
    (filterv (fn [[i slot]] (glider? (get-in e [:inventory i]) slot))
             (cons [held :mainhand] worn))))

(defn can-glide?
  "Returns true when player e may glide, off the ground, under no
  levitation and with a glider worn."
  [e]
  (and (not (:on-ground e))
       (not (contains? (:effects e) :levitation))
       (boolean (seq (glider-slots e)))))

(defn started
  "Returns player e after it asks to glide. Asking while gliding, or
  when it may not, stops it and shows it so."
  [e]
  (if (and (not (:fall-flying e)) (can-glide? e) (not (:in-water? e)))
    (assoc e :fall-flying true)
    (assoc e :fall-flying false :glide-shown? true)))
