(ns collider.world.blocks.grow.bamboo
  "Growth of bamboo."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.blocks.grow.common
             :refer [age air-at? chance? height-above height-below
                     lit?]]))

(set! *warn-on-reflection* true)

(def ^:private ^:table shoot
  (delay (block/state :bamboo {:leaves :small})))

(defn- bamboo? [st] (= :bamboo (block/block-of st)))

(defn- leaves-for [below]
  (if (or (not (bamboo? below))
          (= :none (:leaves (block/props-of below))))
    :small
    :large))

(defn- thinned [p below two leaves]
  (when (and (= leaves :large) (bamboo? two))
    [[(dir/down p) (block/with below :leaves :small)]
     [(dir/toward p :down 2) (block/with two :leaves :none)]]))

(defn- stage-for ^long [^long height roll]
  (if (or (and (>= height 11) (< (double (roll :stage)) 0.25))
          (= height 15))
    1
    0))

(defn- grown [chunks p st roll height]
  (let [below (chunk/at chunks (dir/down p))
        two (chunk/at chunks (dir/toward p :down 2))
        leaves (leaves-for below)
        new-age (if (and (not= 1 (age st)) (not (bamboo? two))) 0 1)
        stage (stage-for height roll)
        top (block/with (block/state :bamboo)
                        :age new-age :leaves leaves :stage stage)]
    (conj (vec (thinned p below two leaves)) [(dir/up p) top])))

(defn- room-above? [chunks p roll]
  (and (chance? roll :gate 3)
       (air-at? chunks (dir/up p))
       (lit? chunks (dir/up p) 9)))

(defn tick
  [chunks p st roll _time _world]
  (when (and (= 0 (block/prop-long st :stage))
             (room-above? chunks p roll))
    (let [height (inc (height-below chunks p :bamboo 16))]
      (when (< height 16)
        (grown chunks p st roll height)))))

(defn sapling-tick
  "Returns the shoot a random tick of a bamboo sapling at p grows."
  [chunks p _st roll _time _world]
  (when (room-above? chunks p roll)
    [[(dir/up p) @shoot]]))

(defn sapling-meal
  "Returns the shoot bone meal grows on a bamboo sapling at p."
  [chunks p _st _roll]
  (when (air-at? chunks (dir/up p))
    {:changes [[(dir/up p) @shoot]]}))

(defn meal
  "Returns the bone meal result for the bamboo at p, or nil when it
  cannot grow. The new segment goes on top of the whole stalk."
  [chunks p _st roll]
  (let [above (height-above chunks p :bamboo 16)
        below (height-below chunks p :bamboo 16)
        top (dir/toward p :up above)
        top-st (chunk/at chunks top)
        target (dir/up top)]
    (when (and (< (+ above below 1) 16)
               (not= 1 (block/prop-long top-st :stage))
               (chunk/in-range? (long (target 1)))
               (air-at? chunks target))
      {:changes (grown chunks top top-st roll (+ above below 1))})))
