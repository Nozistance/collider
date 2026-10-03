(ns collider.world.blocks.scaffold
  "Scaffolding, its distance from a hold and its fall."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn- scaffold? [^long st]
  (= :scaffolding (block/type-of st)))

(defn- nearer [chunks pos d side]
  (let [n (chunk/at chunks (dir/toward pos side))]
    (if (scaffold? n)
      (min (long d) (inc (block/prop-long n :distance)))
      d)))

(defn distance
  "Returns how many steps a scaffolding at pos is from a block that
  holds it from below, 7 when none holds it."
  ^long [chunks pos]
  (let [below (chunk/at chunks (dir/down pos))
        start (if (scaffold? below)
                (block/prop-long below :distance)
                7)]
    (if (and (block/face-sturdy? below :up) (not (scaffold? below)))
      0
      (reduce #(nearer chunks pos %1 %2) start dir/horizontal))))

(defn shaped
  "Returns scaffolding st with the distance and bottom it has at pos."
  ^long [chunks pos ^long st]
  (let [d (distance chunks pos)
        on-scaffold? (scaffold? (chunk/at chunks (dir/down pos)))
        bottom? (and (pos? d) (not on-scaffold?))]
    (block/with st :distance d :bottom (block/flag bottom?))))

(defn- too-far? [^long st] (= 7 (block/prop-long st :distance)))

(defn- due [chunks p _ctx]
  (let [st (chunk/at chunks p)
        st' (shaped chunks p st)]
    (cond
      (not (too-far? st')) (when (not= st' st) [[p st']])
      (too-far? st) [[p (block/emptied st) [[:fall st']]]]
      :else [(block/destroyed p st)])))

(def rule
  "The block rule of scaffolding."
  {:name   :scaffold
   :match? (fn [_chunks st _p] (scaffold? st))
   :wake   (fn [_chunks _dim tick _p _old _side] (inc (long tick)))
   :due    due})
