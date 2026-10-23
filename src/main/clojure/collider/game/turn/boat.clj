(ns collider.game.turn.boat
  "A boat falls, floats and rests. A fall leaves it whole."
  (:require [collider.game.areas :as areas]
            [collider.game.boat :as boat]
            [collider.game.entity :as entity]
            [collider.game.entity.size :as size]
            [collider.game.out :as out]
            [collider.math :as math]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.motion :as motion]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ^:private ^:const gravity 0.04)

(def ^:private ^:const deep-lift (double (float 0.01)))

(defn- cell ^long [chunks x y z]
  (long (chunk/at chunks [x y z])))

(defn- surface [chunks x y z st]
  (+ (double y) (double (liquid/height chunks [x y z] st))))

(defn- underwater
  [chunks min-x max-x body-top min-z max-z top]
  (let [y0 (long (Math/floor body-top))
        y1 (long (Math/ceil top))
        x0 (long (Math/floor min-x))
        x1 (long (Math/ceil max-x))
        z0 (long (Math/floor min-z))
        z1 (long (Math/ceil max-z))]
    (loop [x x0 y y0 z z0 under false]
      (cond
        (>= y y1) (when under :under-water)
        (>= x x1) (recur x0 (inc y) z0 under)
        (>= z z1) (recur (inc x) y z0 under)
        :else
        (let [st (cell chunks x y z)]
          (if (and (block/water? st) (< top (surface chunks x y z st)))
            (if (pos? (block/liquid-level st))
              :under-flowing
              (recur x y (inc z) true))
            (recur x y (inc z) under)))))))

(defn- water-line
  [chunks min-x max-x foot min-z max-z]
  (let [y0 (long (Math/floor foot))
        y1 (long (Math/ceil (+ foot 0.001)))
        x0 (long (Math/floor min-x))
        x1 (long (Math/ceil max-x))
        z0 (long (Math/floor min-z))
        z1 (long (Math/ceil max-z))]
    (loop [x x0 y y0 z z0 line (- Double/MAX_VALUE) wet false]
      (cond
        (>= y y1) (when wet line)
        (>= x x1) (recur x0 (inc y) z0 line wet)
        (>= z z1) (recur (inc x) y z0 line wet)
        :else
        (let [st (cell chunks x y z)]
          (if (block/water? st)
            (let [h (surface chunks x y z st)]
              (recur x y (inc z) (max line h) (or wet (< foot h))))
            (recur x y (inc z) line wet)))))))

(defn- meets? [x y z a b c d e f]
  (and (< (double x) d) (> (inc (double x)) a)
       (< (double y) e) (> (inc (double y)) b)
       (< (double z) f) (> (inc (double z)) c)))

(defn- rub [chunks x z y0 y1 a b c d e f sum n edges]
  (loop [y y0 sum sum n n]
    (cond
      (>= y y1) [sum n]
      (and (pos? edges) (or (= y y0) (= y (dec y1))))
      (recur (inc y) sum n)
      :else
      (let [st (cell chunks x y z)
            hit (and (block/blocks-motion? st)
                     (block/full-cube? st)
                     (meets? x y z a b c d e f))]
        (recur (inc y)
               (if hit
                 (unchecked-float (+ sum (float (motion/friction st))))
                 sum)
               (if hit (inc n) n))))))

(defn- slide-of [chunks min-x y min-z max-x max-z]
  (let [by (- y 0.001)
        x0 (dec (long (Math/floor min-x)))
        x1 (inc (long (Math/ceil max-x)))
        y0 (dec (long (Math/floor by)))
        y1 (inc (long (Math/ceil y)))
        z0 (dec (long (Math/floor min-z)))
        z1 (inc (long (Math/ceil max-z)))]
    (loop [x x0 z z0 sum (float 0) n 0]
      (cond
        (>= x x1)
        (if (zero? n) 0.0 (double (/ sum (float n))))
        (>= z z1) (recur (inc x) z0 sum n)
        :else
        (let [edges (+ (if (or (= x x0) (= x (dec x1))) 1 0)
                       (if (or (= z z0) (= z (dec z1))) 1 0))
              [sum n] (if (= edges 2)
                        [sum n]
                        (rub chunks x z y0 y1
                             min-x by min-z max-x y max-z
                             sum n edges))]
          (recur x (inc z) sum n))))))

(defn- judged [chunks pos half height slide]
  (let [x (double (v/x pos)) y (double (v/y pos))
        z (double (v/z pos))
        min-x (- x half) max-x (+ x half)
        min-z (- z half) max-z (+ z half)
        body-top (+ y height)
        under (underwater chunks min-x max-x body-top
                          min-z max-z (+ body-top 0.001))]
    (cond
      under {:status under :line body-top :slide slide}
      :else
      (if-let [line (water-line chunks min-x max-x y min-z max-z)]
        {:status :in-water :line line :slide slide}
        (let [f (slide-of chunks min-x y min-z max-x max-z)]
          (if (pos? f)
            {:status :on-land :line 0.0 :slide f}
            {:status :in-air :line 0.0 :slide slide}))))))

(defn- water-above
  [chunks min-x max-x top min-z max-z last-yd]
  (let [y0 (long (Math/floor top))
        y1 (long (Math/ceil (- top (double last-yd))))
        x0 (long (Math/floor min-x))
        x1 (long (Math/ceil max-x))
        z0 (long (Math/floor min-z))
        z1 (long (Math/ceil max-z))]
    (loop [y y0]
      (if (>= y y1)
        (double (math/f32 (inc y1)))
        (let [h (loop [x x0 h 0.0]
                  (if (or (>= x x1) (>= h 1.0))
                    h
                    (recur (inc x)
                           (loop [z z0 h h]
                             (if (or (>= z z1) (>= h 1.0))
                               h
                               (let [st (cell chunks x y z)]
                                 (recur (inc z)
                                        (if (block/water? st)
                                          (max h (surface chunks x y z st))
                                          h))))))))]
          (if (< h 1.0)
            (double (math/f32 (+ (double y) h)))
            (recur (inc y))))))))

(defn- drag ^double [status slide]
  (double (case status
            :in-water (float 0.9)
            :under-flowing (float 0.9)
            :under-water (float 0.45)
            :in-air (float 0.9)
            :on-land (float slide)
            (float 0.05))))

(defn- sink ^double [status]
  (if (= status :under-flowing) -7.0E-4 (- gravity)))

(defn- lift [status line y height]
  (case status
    :in-water (/ (- (double line) y) height)
    :under-water deep-lift
    0.0))

(defn- drifted [vel status slide line y height]
  (let [f (drag status slide)
        vy (+ (double (v/y vel)) (sink status))
        b (lift status line y height)
        vy (if (pos? b) (* (+ vy (* b (/ gravity 0.65))) 0.75) vy)]
    (v/v3 (* (double (v/x vel)) f) vy (* (double (v/z vel)) f))))

(defn- snapped [chunks pos vel half height last-yd]
  (let [x (double (v/x pos)) y (double (v/y pos))
        z (double (v/z pos))
        surface (water-above chunks (- x half) (+ x half)
                              (+ y height) (- z half) (+ z half)
                              last-yd)
        target (+ (math/fsub surface height) 0.101)]
    (when (phys/free? chunks pos half height 0.0 (- target y) 0.0)
      [(v/v3 x target z) (v/v3 (v/x vel) 0.0 (v/z vel))])))

(defn- floated [chunks pos vel half height old here last-yd]
  (let [{:keys [status slide]} here]
    (if (and (= old :in-air) (not= status :in-air)
             (not= status :on-land))
      (let [sat (snapped chunks pos vel half height last-yd)]
        (conj (or sat [pos vel]) :in-water slide))
      [pos (drifted vel status slide (:line here)
                    (double (v/y pos)) height)
       status slide])))

(defn- water-below? [chunks pos]
  (block/water?
    (cell chunks (long (Math/floor (v/x pos)))
          (dec (long (Math/floor (v/y pos))))
          (long (Math/floor (v/z pos))))))

(defn- fallen [chunks pos ya ground fall]
  (cond
    ground 0.0
    (and (neg? ya) (not (water-below? chunks pos)))
    (- fall (math/f32 ya))
    :else fall))

(defn- differ? [e k v]
  (let [o (get e k)]
    (if (#{:pos :vel} k) (not (v/same? o v)) (not= o v))))

(defn- kept [e m]
  (let [m (into {} (filter (fn [[k v]] (differ? e k v))) m)]
    (when (seq m) [:merge-entity m])))

(defn- step [chunks eid e]
  (let [half (size/half (:type e))
        height (size/height (:type e))
        old (:boat-status e)
        slide (double (or (:slide e) 0.0))
        last-yd (double (or (:last-yd e) 0.0))
        fall (double (or (:fall e) 0.0))
        here (judged chunks (:pos e) half height slide)
        [pos vel status slide]
        (floated chunks (:pos e) (:vel e) half height
                 old here last-yd)
        ya (double (v/y vel))
        mv (phys/move chunks pos vel half height)
        pos (phys/pos mv)
        vel (phys/vel mv)
        ground (boolean (phys/on-ground? mv))]
    (when-let [[tag m]
               (kept e {:pos pos :vel vel :on-ground ground
                        :fall (fallen chunks pos ya ground fall)
                        :boat-status status :slide slide
                        :last-yd (double (v/y vel))})]
      [tag eid m])))

(def ^:private ^:const eighth (/ Math/PI 8.0))

(def ^:private ^:const quarter (/ Math/PI 4.0))

(def ^:private ^:const circle (* 2.0 Math/PI))

(defn- spun ^double [^double angle on?]
  (if on? (mod (+ angle eighth) circle) 0.0))

(defn- crosses? [^double angle]
  (and (<= (mod angle circle) quarter)
       (>= (mod (+ angle eighth) circle) quarter)))

(defn- paddle-sound [e]
  (let [kind (case (:boat-status e)
               :on-land :entity.boat.paddle-land
               (:in-water :under-water :under-flowing)
               :entity.boat.paddle-water
               nil)]
    (when kind
      (out/all (out/sound kind (:pos e) 1.0 1.0 :neutral)))))

(defn- paddled [e]
  (let [l (double (:paddle-left e 0.0))
        r (double (:paddle-right e 0.0))
        lon (boolean (:left-paddle? e))
        ron (boolean (:right-paddle? e))]
    [(cond-> {}
       (or lon (pos? l)) (assoc :paddle-left (spun l lon))
       (or ron (pos? r)) (assoc :paddle-right (spun r ron)))
     (cond-> []
       (and lon (crosses? l)) (conj (paddle-sound e))
       (and ron (crosses? r)) (conj (paddle-sound e)))]))

(defn- cooled [e]
  (let [hurt (long (:hurt-time e 0))
        hull (double (:hull e 0.0))]
    (cond-> {}
      (pos? hurt) (assoc :hurt-time (dec hurt))
      (pos? hull) (assoc :hull (max 0.0 (dec hull))))))

(defn- turn [world [eid e]]
  (let [        d (if (entity/player?
                (get (:entities world) (first (:passengers e))))
            (when-not (v/same? (:vel e) [0.0 0.0 0.0])
              [:merge-entity eid {:vel (v/v3 0 0 0)}])
            (step (:chunks world) eid e))
        [angles sounds] (paddled e)
        wear (cooled e)
        body (merge angles wear)]
    (cond-> (into (vec (boat/passenger-deltas world eid e))
                  (concat (remove nil? sounds)
                          (boat/gather world eid e)))
      d (conj d)
      (seq body) (conj [:merge-entity eid body]))))

(defn turns
  "Returns the deltas of every boat in an active chunk."
  [world]
  (let [boats (areas/active-of-types world entity/boat-types)]
    (when (pos? (count boats))
      (into [] (mapcat #(turn world %)) boats))))
