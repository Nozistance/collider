(ns collider.world.blocks.multiface
  "Blocks that sit on the faces of their neighbours and spread."
  (:require [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn shuffled [roll xs]
  (loop [v (vec xs) i (count v)]
    (if (< i 2)
      v
      (let [j (long (Math/floor (* (double (roll [:shuffle i])) i)))
            a (v (dec i)) b (v j)]
        (recur (assoc v (dec i) b j a) (dec i))))))

(defn- has-face? [^long st dir]
  (= :true (get (block/props-of st) dir)))

(defn- attachable? [chunks p dir]
  (let [n (chunk/at-void chunks (mapv + p (dir/offset dir)))]
    (and (pos? n) (block/face-sturdy? n (dir/opposite dir)))))

(defn- water-source? [^long st]
  (and (pos? st) (block/water-source? st)))

(defn- replaceable? [^long st self]
  (or (zero? st)
      (and (pos? st) (= self (block/block-of st)))
      (water-source? st)))

(defn- valid-placement? [chunks old p dir self]
  (let [old (max 0 (long old))]
    (and (or (not= self (block/block-of old))
             (not (has-face? old dir)))
         (attachable? chunks p dir))))

(defn- spread-into? [chunks [q dir] self]
  (let [existing (chunk/at-void chunks q)]
    (and (not (neg? existing))
         (replaceable? existing self)
         (valid-placement? chunks existing q dir self))))

(defn- spread-pos [p from-face spread-dir type]
  (let [ahead (mapv + p (dir/offset spread-dir))]
    (case type
      :same-position [p spread-dir]
      :same-plane [ahead from-face]
      :wrap-around [(mapv + ahead (dir/offset from-face))
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
    (= self (block/block-of (max 0 old))) old
    (water-source? old) (block/with-water (block/state self))
    :else (block/state self)))

(defn- placed-state ^long [chunks [q dir] self]
  (let [base (base-state (chunk/at-void chunks q) self)]
    (block/state (block/block-of base)
                 (assoc (block/props-of base) dir :true))))

(defn- from-face-random [chunks p st from-face roll]
  (let [dirs (shuffled #(roll [:dir from-face %]) dir/six)]
    (first (keep #(spread-toward chunks p st from-face %) dirs))))

(defn- face-spread [chunks p st roll from-face]
  (when (has-face? st from-face)
    (from-face-random chunks p st from-face roll)))

(defn spread-random [chunks p ^long st roll]
  (let [self (block/block-of st)
        faces (shuffled #(roll [:face %]) dir/six)
        spread #(face-spread chunks p st roll %)]
    (when-let [sp (first (keep spread faces))]
      [[(first sp) (placed-state chunks sp self)]])))

(defn can-spread? [chunks p ^long st]
  (boolean (some (fn [[from to]] (spread-toward chunks p st from to))
                 (for [from dir/six to dir/six] [from to]))))
