(ns collider.world.orientation

  (:require [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def face-offsets
  {0 [0 -1 0], 1 [0 1 0], 2 [0 0 -1], 3 [0 0 1], 4 [-1 0 0], 5 [1 0 0]})

(def ^:private logs        #{17 162})
(def ^:private stairs      #{53 67 108 109 114 128 134 135 136 156 163 164 180})
(def ^:private slabs       #{44 126 182})
(def ^:private torches     #{50 75 76})
(def ^:private pumpkins    #{86 91})
(def ^:private face-player #{23 54 61 62 130 146 158})
(def no-collision #{50 65 75 76})

(def slab->double {44 43, 126 125, 182 181})
(defn player-facing

  ^long [yaw]
  (bit-and (long (Math/floor (+ (/ (* (double yaw) 4.0) 360.0) 0.5))) 3))

(defn placement-meta [item-id damage face yaw cursor-y]
  (let [id   (long item-id)
        face (long face)
        f    (player-facing yaw)
        top? (or (= face 0) (and (not= face 1) (> (long cursor-y) 8)))]
    (cond
      (logs id)        (bit-or (bit-and (long damage) 3)
                               (case face (0 1) 0, (4 5) 4, 8))
      (stairs id)      (bit-or (long (nth [2 1 3 0] f)) (if top? 4 0))
      (slabs id)       (bit-or (bit-and (long damage) 7) (if top? 8 0))
      (torches id)     (long (case face 2 4, 3 3, 4 2, 5 1, 5))
      (= id 65)        (long (if (<= 2 face 5) face 2))
      (face-player id) (long (nth [2 5 3 4] f))
      (pumpkins id)    (long (nth [2 3 0 1] f))
      :else            (bit-and (long damage) 15))))

(defn- same-slab? [state item-id damage]
  (and (= (bit-shift-right (long state) 4) (long item-id))
       (= (bit-and (long state) 7) (bit-and (long damage) 7))))

(defn slab-merge [chunks pos pos' face item-id damage]
  (when-let [did (slab->double (long item-id))]
    (let [at      (fn [p] (chunk/chunks-get-block chunks gen/flat-chunk p))
          clicked (at pos)
          state   (bit-or (bit-shift-left (long did) 4) (bit-and (long damage) 7))
          lower?  (and (= 1 (long face)) (zero? (bit-and (long clicked) 8)))
          upper?  (and (= 0 (long face)) (pos? (bit-and (long clicked) 8)))]
      (cond
        (and (same-slab? clicked item-id damage) (or lower? upper?))
        [pos state]
        (and pos' (same-slab? (at pos') item-id damage))
        [pos' state]))))
