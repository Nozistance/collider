(ns collider.game.item
  "Item entities that blocks, players and containers drop."
  (:require [collider.game.entity :as entity]
            [collider.random :as random]))

(set! *warn-on-reflection* true)

(def ^:private ^:const throw-pickup-delay 40)

(def ^:private ^:const throw-power 0.3)

(def ^:private ^:const throw-spread 0.02)

(def ^:private ^:const throw-lift 0.1)

(def ^:private ^:const throw-jitter 0.1)

(def ^:private ^:const around-power 0.5)

(def ^:private ^:const around-lift 0.2)

(def ^:private ^:const hand-drop (double (float 0.3)))

(defn- throw-velocity [world eid salt]
  (let [e (get-in world [:entities eid])
        yaw (Math/toRadians (double (or (:yaw e) 0.0)))
        pitch (Math/toRadians (double (or (:pitch e) 0.0)))
        t (:tick world)
        ang (* (random/of-key t eid salt :a) Math/PI 2.0)
        mag (* throw-spread (random/of-key t eid salt :m))
        jitter (- (random/of-key t eid salt :y1)
                  (random/of-key t eid salt :y2))]
    [(+ (* (- throw-power) (Math/sin yaw) (Math/cos pitch))
        (* (Math/cos ang) mag))
     (+ (* (- throw-power) (Math/sin pitch)) throw-lift
        (* throw-jitter jitter))
     (+ (* throw-power (Math/cos yaw) (Math/cos pitch))
        (* (Math/sin ang) mag))]))

(defn- around-velocity [world eid salt]
  (let [t (:tick world)
        pow (* around-power (random/of-key t eid salt :p))
        dir (* Math/PI 2.0 (random/of-key t eid salt :d))]
    [(* -1.0 (Math/sin dir) pow) around-lift (* (Math/cos dir) pow)]))

(defn dropped
  "Returns the item entity a player throws out of hand.
  With randomly? the item flies in a random direction."
  ([world thrower stack] (dropped world thrower stack false 0))
  ([world thrower stack randomly? salt]
   (let [e (get-in world [:entities thrower])
         [px py pz] (:pos e)
         y (- (+ (double py) (entity/eye-height e)) hand-drop)
         at [(double px) y (double pz)]
         vel (if randomly?
               (around-velocity world thrower salt)
               (throw-velocity world thrower salt))]
     (entity/item at vel stack throw-pickup-delay))))

(defn thrown-deltas
  "Returns the deltas of player eid throwing the stacks.
  The stats of each throw come with them."
  [world eid stacks]
  (mapcat (fn [i s]
            [[:spawn-entity (dropped world eid s false i)]
             [:award eid (keyword "dropped" (name (:item s)))
              (long (:count s 1))]
             [:award eid :custom/drop 1]])
          (range) stacks))

(defn popped
  "Returns the item dropped by a broken block."
  [world pos stack salt]
  (let [t (:tick world)
        r (fn [k] (random/of-key t pos salt k))
        [x y z] pos]
    (entity/item [(+ (double x) 0.5 (- (* 0.5 (r :x)) 0.25))
                  (+ (double y) 0.5 (- (* 0.5 (r :y)) 0.25) -0.125)
                  (+ (double z) 0.5 (- (* 0.5 (r :z)) 0.25))]
                 (entity/pop-velocity [t pos salt])
                 stack)))

(defn split-drop
  "Returns the stack split into the piles a broken block drops."
  [world pos stack salt]
  (loop [n (long (:count stack 1)) i 0 acc []]
    (if-not (pos? n)
      acc
      (let [k [(:tick world) pos salt :split i]
            got (min n (+ 10 (long (* 21.0 (random/of-key k)))))]
        (recur (- n got) (inc i)
               (conj acc (assoc stack :count got)))))))

(def ^:private ^:const scatter-spread 0.11485000171139836)

(defn- scatter-spot [pos roll]
  (let [f #(Math/floor (double (nth pos %)))]
    [(+ (f 0) (* (double (roll :x)) 0.75) 0.125)
     (+ (f 1) (* (double (roll :y)) 0.75))
     (+ (f 2) (* (double (roll :z)) 0.75) 0.125)]))

(defn- scatter-axis ^double [roll i a ^double mode]
  (let [r #(double (roll [:vel i %]))]
    (+ mode (* scatter-spread (- (r a) (r (inc (long a))))))))

(defn- scatter-speed [roll i]
  [(scatter-axis roll i 0 0.0) (scatter-axis roll i 2 0.2)
   (scatter-axis roll i 4 0.0)])

(defn- pile ^long [roll i ^long left]
  (min left (+ 10 (long (* 21.0 (double (roll [:split i])))))))

(defn scattered
  "Returns the items that stack at pos scatters into.
  Piles of 10 to 30 leave one spot inside the cell, each thrown its
  own way, none held back from pickup. roll gives a number in [0, 1)
  for each key."
  [pos stack roll]
  (let [at (scatter-spot pos roll)]
    (loop [n (long (:count stack 1)) i 0 acc []]
      (if-not (pos? n)
        acc
        (let [got (pile roll i n)
              s (assoc stack :count got)
              it (entity/item at (scatter-speed roll i) s 0)]
          (recur (- n got) (inc i) (conj acc it)))))))
