(ns collider.world.blocks.grow.common
  "Block reads and dice shared by plant growth."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.light :as light]))

(set! *warn-on-reflection* true)

(defn air-at?
  "Returns true when the cell p is air inside the world height."
  [chunks [_ y _ :as p]]
  (and (chunk/in-range? y) (zero? (chunk/at chunks p))))

(defn lit?
  "Returns true when the light at the cell p is at least n."
  [chunks [x y z] ^long n]
  (>= (long (light/light-at chunks x y z)) n))

(defn chance?
  "Returns true with a chance of one in n for the roll of salt."
  [roll salt ^long n]
  (< (double (roll salt)) (/ 1.0 n)))

(defn age
  ^long [st]
  (block/prop-long st :age))

(defn aged
  ^long [st ^long n]
  (block/with st :age n))

(defn older
  ^long [st]
  (aged st (inc (age st))))

(defn flagged
  [flags changes]
  (mapv (fn [[p st fx]] [p st fx flags]) changes))

(defn- run-length [chunks p self cap d]
  (loop [h 0]
    (if (and (< h (long cap))
             (= self (block/block-of
                      (chunk/at chunks (dir/toward p d (inc h))))))
      (recur (inc h))
      h)))

(defn height-below
  "Returns how many blocks of self stand right below p, up to cap."
  ^long [chunks p self ^long cap]
  (run-length chunks p self cap :down))

(defn height-above
  "Returns how many blocks of self stand right above p, up to cap."
  ^long [chunks p self ^long cap]
  (run-length chunks p self cap :up))

(defn- self-at? [chunks [x y z] self dx dy dz]
  (let [q [(+ (long x) (long dx)) (+ (long y) (long dy))
           (+ (long z) (long dz))]]
    (= self (block/block-of (chunk/at chunks q)))))

(defn crowd
  "Returns how many blocks of self stand within four blocks of p
  across and one block up or down."
  ^long [chunks p self]
  (count (for [dx (range -4 5) dy [-1 0 1] dz (range -4 5)
               :when (self-at? chunks p self dx dy dz)]
           1)))

(defn- walk-step [roll salt i]
  (let [r #(random/below (roll (salt i %)) 3)
        dy (* (dec (r :y)) (r :n))]
    [(dec (r :x)) (quot dy 2) (dec (r :z))]))

(defn meal-walk
  "Returns where the bone meal walk number j from p ends, or nil when
  a step lands where pass? fails. The function salt gives the roll
  key of each part of the step i."
  [roll salt p j pass?]
  (loop [i 0 q p]
    (if (= i (quot (long j) 16))
      q
      (let [q' (mapv + q (walk-step roll salt i))]
        (when (pass? q')
          (recur (inc i) q'))))))
