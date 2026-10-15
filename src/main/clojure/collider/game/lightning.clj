(ns collider.game.lightning
  "Lightning bolts and where a storm strikes."
  (:require [collider.game.block.pois :as pois]
            [collider.game.entity :as entity]
            [collider.game.entity.size :as size]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.env.weather :as weather]
            [collider.world.space.column :as column]))

(set! *warn-on-reflection* true)

(def ^:private ^:const rod-reach 128)

(def ^:private ^:const body-reach 3.0)

(defn bolt
  "Returns a new lightning bolt at pos, made at tick t."
  [pos t]
  {:type :lightning-bolt :pos pos :vel [0.0 0.0 0.0]
   :yaw 0.0 :pitch 0.0 :born t :life 2
   :flashes (inc (random/below (random/of-key t pos :flashes) 3))})

(defn- dist-sq ^long [[ax ay az] [bx by bz]]
  (let [dx (- (long ax) (long bx)) dy (- (long ay) (long by))
        dz (- (long az) (long bz))]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- chunk-rods [w cx cz]
  (when (contains? (:chunks w) (chunk/pos->id cx cz))
    (mapcat #(pois/listed (:pois w) [cx (- % chunk/section-offset) cz]
                          :lightning-rod)
            (range chunk/section-count))))

(defn- rods-around [w [x _ z]]
  (let [r (inc (quot rod-reach 16))
        cx (bit-shift-right (long x) 4)
        cz (bit-shift-right (long z) 4)]
    (for [j (range (- cz r) (+ cz r 1)) i (range (- cx r) (+ cx r 1))
          p (chunk-rods w i j)]
      p)))

(defn- topmost? [chunks [x y z]]
  (= (long y) (column/surface-top chunks x z)))

(defn- rod-target [w center]
  (let [near? #(<= (dist-sq % center) (* rod-reach rod-reach))
        found (filter #(and (near? %) (topmost? (:chunks w) %))
                      (rods-around w center))]
    (when (seq found)
      (let [d #(dist-sq % center)
            [x y z] (reduce #(if (< (d %2) (d %1)) %2 %1) found)]
        [x (inc (long y)) z]))))

(defn- in-column? [[cx _ cz] e]
  (let [h (double (nth (size/box e) 0)) [x _ z] (:pos e)
        lo (- body-reach) hi (+ 1.0 body-reach)]
    (and (< (- (double x) h) (+ (long cx) hi))
         (> (+ (double x) h) (+ (long cx) lo))
         (< (- (double z) h) (+ (long cz) hi))
         (> (+ (double z) h) (+ (long cz) lo)))))

(defn- above? [[_ cy _] e]
  (> (+ (double (nth (:pos e) 1)) (double (nth (size/box e) 1)))
     (- (long cy) body-reach)))

(defn- struck-body? [world center e]
  (and (entity/living? e) (entity/alive? e)
       (not (entity/removed? e))
       (in-column? center e) (above? center e)
       (weather/can-see-sky? (:chunks world) (v/cell (:pos e)))))

(defn- body-target [world center]
  (let [es (filterv #(struck-body? world center %)
                    (vals (:entities world)))]
    (when (seq es)
      (let [r (random/of-key (:tick world) center :bolt-body)]
        (v/cell (:pos (nth es (random/below r (count es)))))))))

(defn target
  "Returns the cell a storm strikes for the random cell pos of level
  world. A rod on top of its column within reach draws it, else a
  living body under the sky near the column, else the top of the
  column."
  [world [x _ z]]
  (let [chunks (:chunks world)
        center [x (column/motion-blocking-height chunks x z) z]]
    (or (rod-target world center)
        (body-target world center)
        center)))

(def ^:private ^:const strike-odds 1.0E-5)

(defn- storm? [world]
  (and (weather/raining? world)
       (> (weather/thunder-level world) 0.9)))

(defn- storm-cell [world cid]
  (let [[cx cz] (chunk/id->pos cid)
        r #(random/below (random/of-key (:tick world) cid %) 16)]
    [(+ (* 16 (long cx)) (r :bolt-x)) 0
     (+ (* 16 (long cz)) (r :bolt-z))]))

(defn storm-bolt
  "Returns the bolt a storm strikes in chunk cid of level world this
  tick, or nil. Most ticks it strikes nowhere."
  [world cid]
  (when (and (storm? world)
             (< (random/of-key (:tick world) cid :bolt) strike-odds))
    (let [[x y z :as p] (target world (storm-cell world cid))]
      (when (weather/raining-at? world (:chunks world) p)
        (bolt [(+ (double x) 0.5) (double y) (+ (double z) 0.5)]
              (:tick world))))))
