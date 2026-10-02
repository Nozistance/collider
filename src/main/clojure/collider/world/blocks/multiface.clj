(ns collider.world.blocks.multiface
  "Blocks that sit on the faces of their neighbours and spread."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn has-face?
  "Returns true when st shows a face on side."
  [^long st side]
  (= :true (get (block/props-of st) side)))

(def ^:private face-axes
  {:west [0 1 2] :east [0 1 2] :down [1 0 2] :up [1 0 2]
   :north [2 0 1] :south [2 0 1]})

(defn- spans? [box ^long i]
  (and (== 0 (double (box i))) (== 16 (double (box (+ 3 i))))))

(defn- box-full? [box side]
  (let [[a u v] (face-axes side)
        a (long a) lo (double (box a)) hi (double (box (+ 3 a)))]
    (and (if (#{:east :up :south} side)
           (and (< lo 16.0) (>= hi 16.0))
           (and (<= lo 0.0) (> hi 0.0)))
         (spans? box u) (spans? box v))))

(defn- collision-full? [^long st side]
  (boolean (some #(box-full? % side) (block/collision-boxes st))))

(defn face-held?
  "Returns true when the block at q can hold a face on side."
  [chunks q side]
  (let [n (chunk/at-void chunks q)]
    (and (pos? n)
         (or (block/face-sturdy? n side) (collision-full? n side)))))

(defn attaches?
  "Returns true when the neighbour of p on dir holds a face toward p."
  [chunks p dir]
  (face-held? chunks (dir/toward p dir) (dir/opposite dir)))

(defn- face-kept [chunks pos m side]
  (if (and (= :true (get m side))
           (not (attaches? chunks pos side)))
    (assoc m side :false)
    m))

(defn- kept ^long [chunks pos ^long st sides]
  (let [step (fn [m side] (face-kept chunks pos m side))
        props' (reduce step (block/props-of st) sides)
        st' (block/state (block/block-of st) props')]
    (if (seq (block/faces-of st')) st' (block/emptied st))))

(defn updated
  "Returns st at pos without the faces that nothing holds, or what
  is left of it when no face is left."
  ^long [chunks pos ^long st]
  (kept chunks pos st block/face-props))

(defn sides-updated
  "Returns st without the faces on sides that lost the block they
  cover. A nil side means a change at pos and checks every face."
  ^long [chunks pos ^long st sides]
  (if (contains? sides nil)
    (updated chunks pos st)
    (kept chunks pos st (filter sides block/face-props))))

(defn- replaceable? [^long st self]
  (or (zero? st)
      (= self (block/block-of st))
      (block/water-source? st)))

(defn- valid-placement? [chunks old p dir self]
  (and (or (not= self (block/block-of (long old)))
           (not (has-face? old dir)))
       (attaches? chunks p dir)))

(defn- spread-into? [chunks [q dir] self]
  (let [existing (chunk/at-void chunks q)]
    (and (not (neg? existing))
         (replaceable? existing self)
         (valid-placement? chunks existing q dir self))))

(defn- spread-pos [p from-face spread-dir type]
  (let [ahead (dir/toward p spread-dir)]
    (case type
      :same-position [p spread-dir]
      :same-plane [ahead from-face]
      :wrap-around [(dir/toward ahead from-face)
                    (dir/opposite spread-dir)])))

(defn spread-toward
  "Returns the [pos face] that st at p spreads onto from from-face
  toward spread-dir, or nil when it cannot."
  [chunks p st from-face spread-dir]
  (when (and (not= (dir/axis spread-dir) (dir/axis from-face))
             (has-face? st from-face)
             (not (has-face? st spread-dir)))
    (first (for [type [:same-position :same-plane :wrap-around]
                 :let [sp (spread-pos p from-face spread-dir type)]
                 :when (spread-into? chunks sp (block/block-of st))]
             sp))))

(defn- base-state [^long old self]
  (cond
    (= self (block/block-of old)) old
    (block/water-source? old) (block/with-water (block/state self))
    :else (block/state self)))

(defn- placed-state ^long [chunks [q dir] self]
  (let [base (base-state (chunk/at-void chunks q) self)]
    (block/with base dir :true)))

(defn- shuffled-six [roll]
  (random/shuffled #(random/below (roll [:shuffle %]) %) dir/six))

(defn- from-face-random [chunks p st from-face roll]
  (let [dirs (shuffled-six #(roll [:dir from-face %]))]
    (first (keep #(spread-toward chunks p st from-face %) dirs))))

(defn- face-spread [chunks p st roll from-face]
  (when (has-face? st from-face)
    (from-face-random chunks p st from-face roll)))

(defn spread-random
  "Returns the change that spreads st at p onto one face picked by
  roll, or nil when no face can take it."
  [chunks p ^long st roll]
  (let [self (block/block-of st)
        faces (shuffled-six #(roll [:face %]))
        spread #(face-spread chunks p st roll %)]
    (when-let [sp (first (keep spread faces))]
      [[(first sp) (placed-state chunks sp self)]])))
