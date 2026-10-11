(ns collider.world.blocks.motion
  "What the blocks an entity touches do to the speed it keeps."
  (:require [collider.data :as data]
            [collider.num :as num]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]
            [collider.world.space.crossed :as crossed]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(def ^:private ^:const deflate 1.0E-5)

(def ^:private web-speed [0.25 (double (float 0.05)) 0.25])

(def ^:private ^:const step-offset 0.2)

(def ^:private ^:const slow-fall 0.1)

(def ^:private ^:const step-base 0.4)

(def ^:private ^:const step-slope 0.2)

(defn- lo ^long [^double v] (num/floor (+ v deflate)))

(defn- hi ^long [^double v] (num/floor (- v deflate)))

(defn- state-at ^long [chunks ^long x ^long y ^long z]
  (if (chunk/in-range? y) (chunk/block-state chunks x y z) 0))

(def ^:private snow-speed
  [(double (float 0.9)) 1.5 (double (float 0.9))])

(def ^:private berry-speed
  [(double (float 0.8)) 0.75 (double (float 0.8))])

(def ^:private unberried #{:fox :bee})

(defn- held-by ^long [^long st snow? berry?]
  (case (block/type-of st)
    :web 1
    :powder-snow (if snow? 2 0)
    :sweet-berry-bush (if berry? 3 0)
    0))

(def ^:private shrink (double (float 1.0E-5)))

(def ^:private ^:const max-steps 16)

(defn- box-at [[x y z] half height]
  (let [x (double x) y (double y) z (double z)
        half (double half) height (double height)]
    [(+ (- x half) shrink) (+ y shrink) (+ (- z half) shrink)
     (- (+ x half) shrink) (- (+ y height) shrink) (- (+ z half) shrink)]))

(defn- walked
  "Returns hold acc after the box of a body passes from point from to
  point to, meeting no more than steps cells, and the steps it took.
  The fn held tells what a block state does to it."
  [chunks acc from to half height held steps]
  (let [cs (take-while #(< (long (nth % 3)) (long steps))
                       (crossed/cells from to (box-at to half height)))
        acc (reduce (fn [[_ seen :as acc] [x y z]]
                      (let [h (long (held (state-at chunks x y z)))
                            k [x y z]]
                        (if (or (zero? h) (contains? seen k))
                          acc
                          [h (conj seen k)])))
                    acc cs)]
    [acc (inc (long (if-let [c (last cs)] (nth c 3) 0)))]))

(defn- walked-move
  "Returns hold acc after one move [from to d] of a body, cut along
  the axes of d, the move it asked for, when it has one."
  [chunks acc [from to d] half height held]
  (let [delta (mapv - to from)
        axes (if (< (Math/abs (double (nth d 0 0.0)))
                    (Math/abs (double (nth d 2 0.0))))
               [1 2 0]
               [1 0 2])
        [acc left]
        (if (and d (pos? (double (reduce + (map * delta delta)))))
          (reduce (fn [[acc left p] i]
                    (let [a (double (delta i))]
                      (if (zero? a)
                        [acc left p]
                        (let [q (update p i + a)
                              [acc n] (walked chunks acc p q half height
                                              held left)]
                          [acc (- (long left) (long n)) q]))))
                  [acc max-steps (vec from)] axes)
          (let [[acc n] (walked chunks acc from to half height held
                                max-steps)]
            [acc (- max-steps (long n))]))]
    (if (<= (long left) 0)
      (first (walked chunks acc to to half height held 1))
      acc)))

(def ^:private ^:table snowy
  (delay (let [n (data/block-state-count) a (boolean-array n)]
           (dotimes [st n]
             (when (= :powder-snow (block/type-of st))
               (aset a st true)))
           a)))

(defn in-powder-snow?
  "Returns true when a body at pos of half width half and height
  touches powder snow."
  [chunks [x y z] half height]
  (let [x (double x) y (double y) z (double z)
        half (double half) height (double height)]
    (phys/some-cell? chunks @snowy
                     (lo (- x half)) (lo y) (lo (- z half))
                     (inc (hi (+ x half))) (inc (hi (+ y height)))
                     (inc (hi (+ z half))))))

(defn- snow-at-feet? [chunks [x y z]]
  (= :powder-snow
     (block/type-of (state-at chunks (num/floor (double x))
                              (num/floor (double y))
                              (num/floor (double z))))))

(defn stuck-speed
  "Returns the factor the blocks a body met on its moves this tick
  apply to its next move, or nil when none of them holds it. Each
  move is [from to d], d the move it asked for or nil. Of a cobweb,
  powder snow and a sweet berry bush, the one met last holds it.
  Powder snow holds a living body only by its feet, a berry bush
  only a living body, and of those not a fox or a bee. Type is that
  of a living body, nil for any other."
  ([chunks moves half height] (stuck-speed chunks moves half height nil))
  ([chunks moves half height type]
   (let [end (nth (peek (vec moves)) 1)
         snow? (or (nil? type) (snow-at-feet? chunks end))
         berry? (and (some? type) (not (contains? unberried type)))
         held #(held-by % snow? berry?)
         [found] (reduce #(walked-move chunks %1 %2 half height held)
                         [0 #{}] moves)]
     (case (long found)
       1 web-speed
       2 snow-speed
       3 berry-speed
       nil))))

(defn- below-of ^long [chunks [x y z]]
  (state-at chunks (num/floor (double x))
            (num/floor (- (double y) step-offset))
            (num/floor (double z))))

(defn- on-slime? [chunks pos]
  (= :slime (block/type-of (below-of chunks pos))))

(defn stepped-speed
  "Returns the motion vel of a body at pos after it steps on the
  block under it. Slime slows a body that barely falls."
  [chunks pos vel]
  (let [ay (Math/abs (double (nth vel 1)))]
    (if (and (on-slime? chunks pos) (< ay slow-fall))
      (let [s (+ step-base (* ay step-slope))]
        [(* (double (nth vel 0)) s) (nth vel 1)
         (* (double (nth vel 2)) s)])
      vel)))

(defn- motion-table ^doubles [k ^double default]
  (let [a (double-array (data/block-state-count) default)]
    (doseq [[_ b] (data/blocks)
            :let [v (k b)] :when v
            i (range (reduce * 1 (map count (vals (:props b)))))]
      (aset a (+ (long (:first b)) (long i))
            (double (float (double v)))))
    a))

(def ^:private ^:table frictions
  (delay (motion-table :friction (double (float 0.6)))))

(def ^:private ^:table speed-factors
  (delay (motion-table :speed-factor 1.0)))

(def ^:private ^:table jump-factors
  (delay (motion-table :jump-factor 1.0)))

(def ^:private ^:table bounces
  (delay (motion-table :bounce 0.0)))

(defn- factor-of ^double [^doubles a ^long st]
  (if (and (< -1 st) (< st (alength a))) (aget a st) (aget a 0)))

(def ^:private ^:const lifeless-bounce (double (float 0.8)))

(defn bounce
  "Returns the speed up of a body that lands on st after it asked to
  move down by vy and moved by dy, or nil when st does not bounce it.
  g is its gravity, drag its air drag, and a body that is not alive
  takes four fifths of the bounce of st."
  [st vy dy g drag living?]
  (let [vy (double vy) dy (double dy) g (double g) drag (double drag)
        b (factor-of @bounces (long st))
        b (if living? b (num/fmul b lifeless-bounce))]
    (when (and (pos? b) (not (< (- vy) g))
               (not (block/tagged? st "suppresses_bounce")))
      (let [p (/ dy vy)
            d (+ 1.0 (* p (- drag 1.0)))]
        (* (* (- (* p g) vy) d) b)))))

(defn friction
  "Returns how much of its speed a body that slides on st keeps."
  ^double [st]
  (factor-of @frictions (long st)))

(defn speed-factor
  "Returns what st multiplies the speed of a body on it by."
  ^double [st]
  (factor-of @speed-factors (long st)))

(defn jump-factor
  "Returns what st multiplies the jump of a body on it by."
  ^double [st]
  (factor-of @jump-factors (long st)))

(def ^:private ^:const below-offset (double (float 0.500001)))

(def ^:private ^:table supports
  (delay (block/state-table
           :boolean #(#{:wall :fence-gate} (block/type-of %)))))

(def ^:private ^:table watery
  (delay (block/state-table
           :boolean #(#{:water :bubble-column} (block/block-of %)))))

(defn- flagged? [^booleans a ^long st]
  (and (< -1 st) (< st (alength a)) (aget a st)))

(defn- holds-support? [st]
  (flagged? @supports (long st)))

(defn below-state
  "Returns the state of the block under a body at pos that sets its
  friction and speed. sup is the block the body rests on, or nil.
  offset is how far below its feet the body looks."
  (^long [chunks pos sup] (below-state chunks pos sup below-offset))
  (^long [chunks pos sup ^double offset]
   (let [y (num/floor (- (v/y pos) offset))
         st (when sup (chunk/at chunks sup))
         xz (or sup pos)
         x (num/floor (v/x xz))
         z (num/floor (v/z xz))]
     (if (and sup (holds-support? st))
       st
       (state-at chunks x y z)))))

(defn- feet-state ^long [chunks pos]
  (state-at chunks (num/floor (v/x pos)) (num/floor (v/y pos))
            (num/floor (v/z pos))))

(defn block-speed-factor
  "Returns what the blocks at and under a body at pos multiply its
  horizontal speed by after a move. sup is the block the body rests
  on, or nil."
  ^double [chunks pos sup]
  (let [st (feet-state chunks pos)
        here (speed-factor st)]
    (if (or (not (== here 1.0))
            (flagged? @watery st))
      here
      (speed-factor (below-state chunks pos sup)))))
