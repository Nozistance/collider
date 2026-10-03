(ns collider.game.orb
  "Experience orbs, made, merged, moved and given. The functions take
  a roll that answers a number in [0, 1) for each key, in the order
  the entity draws them."
  (:require [collider.game.entity :as entity]
            [collider.game.entity.size :as size]
            [collider.game.experience :as xp]
            [collider.num :as num]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.motion :as motion]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(defn half
  ^double []
  (size/half :experience-orb))

(defn height
  ^double []
  (size/height :experience-orb))

(def lifetime
  "How many ticks an orb lasts."
  6000)

(def ^:private ^:const groups 40)

(def ^:private ^:const scan-period 20)

(def ^:private ^:const gravity 0.03)

(defn- eye ^double [] (size/pose-eye :experience-orb nil))

(defn- unit [d]
  (let [x (double (nth d 0)) y (double (nth d 1)) z (double (nth d 2))
        l (Math/sqrt (+ (* x x) (* y y) (* z z)))]
    (if (< l 1.0E-5) [0.0 0.0 0.0] [(/ x l) (/ y l) (/ z l)])))

(defn make
  "Returns an orb of value at pos with a random yaw and a random push
  turned to face roughly. The orb also moves half its size along that
  rough direction."
  ([pos value roll] (make pos [0.0 0.0 0.0] value roll))
  ([pos roughly ^long value roll]
   (let [yaw (num/f32 (* (num/f32 (roll :yaw)) 360.0))
         side #(* (- (* (double (roll %)) 0.2) 0.1) 2.0)
         push [(side :vx) (* (double (roll :vy)) 0.2 2.0) (side :vz)]
         push (if (neg? (v/dot roughly push)) (mapv - push) push)
         u (unit roughly)
         at (mapv #(+ (double %1) (* (double %2) 0.25)) pos u)]
     {:type :experience-orb :pos at :vel push :yaw yaw :pitch 0.0
      :on-ground false :value value :count 1 :age 0 :health 5.0})))

(defn- orb? [e] (= :experience-orb (:type e)))

(defn- jmod ^long [^long a] (rem a groups))

(defn- touches-cube? [p o]
  (let [px (double (nth p 0)) py (double (nth p 1))
        pz (double (nth p 2))
        ox (v/x o) oy (v/y o) oz (v/z o) w (half)]
    (and (< (- px 0.5) (+ ox w)) (> (+ px 0.5) (- ox w))
         (< (- py 0.5) (+ oy (height))) (> (+ py 0.5) oy)
         (< (- pz 0.5) (+ oz w)) (> (+ pz 0.5) (- oz w)))))

(defn- joins? [[eid e] ^long id ^long value pos]
  (and (zero? (jmod (- (long eid) id)))
       (== value (long (:value e)))
       (touches-cube? pos (:pos e))))

(defn- orbs-of [w]
  (sort-by key (filter (fn [[_ e]] (orb? e)) (:entities w))))

(defn- joined [w eid]
  (update-in w [:entities eid] assoc :count
             (inc (long (get-in w [:entities eid :count])))
             :age 0))

(defn- award-one [spawn w pos roughly value roll]
  (let [id (long (* groups (double (roll :group))))
        value (long value)
        hit (first (filter #(joins? % id value pos) (orbs-of w)))]
    (if hit
      (joined w (key hit))
      (spawn w (make pos roughly value roll)))))

(defn awarded
  "Returns the level w after amount points drop at pos as orbs that
  face roughly. Each orb joins a near one of its value and group, or
  spawns with spawn."
  [w spawn pos roughly amount roll]
  (reduce (fn [w [i value]]
            (award-one spawn w pos roughly value #(roll [i %])))
          w (map-indexed vector (xp/orb-values (long amount)))))

(defn- box-near? [a b]
  (let [ax (v/x a) ay (v/y a) az (v/z a)
        bx (v/x b) by (v/y b) bz (v/z b) w (half) h (height)]
    (and (< (- (- ax w) 0.5) (+ bx w))
         (> (+ (+ ax w) 0.5) (- bx w))
         (< (- ay 0.5) (+ by h))
         (> (+ (+ ay h) 0.5) by)
         (< (- (- az w) 0.5) (+ bz w))
         (> (+ (+ az w) 0.5) (- bz w)))))

(defn- mergeable? [eid e [oid o]]
  (and (not= (long eid) (long oid))
       (zero? (jmod (- (long oid) (long eid))))
       (== (long (:value e)) (long (:value o)))
       (box-near? (:pos e) (:pos o))))

(defn merged
  "Returns orb e and the ids of the orbs it takes in among the orbs
  [eid orb] around."
  [eid e orbs]
  (let [taken (filterv #(mergeable? eid e %) orbs)]
    [(reduce (fn [e [_ o]]
               (assoc e :count (+ (long (:count e)) (long (:count o)))
                        :age (min (long (:age e)) (long (:age o)))))
             e taken)
     (mapv key taken)]))

(defn- nearest [pos players]
  (reduce (fn [best [_ p :as entry]]
            (let [d (v/dist-sq (:pos p) pos)
                  closer? (or (nil? best) (< d (double (best 1))))]
              (if (and (< d 64.0) closer?)
                [entry d]
                best)))
          nil players))

(defn- chosen [pos players]
  (when-let [[[pid p]] (nearest pos players)]
    (when (entity/alive? p) pid)))

(defn followed
  "Returns the player orb e follows among players [eid player] that
  are not spectators."
  [e players]
  (let [cur (some (fn [[pid p]] (when (= pid (:follow e)) p))
                  players)]
    (if (and cur (<= (v/dist-sq (:pos cur) (:pos e)) 64.0))
      (:follow e)
      (chosen (:pos e) players))))

(defn- normal [x y z]
  (let [d (Math/sqrt (+ (* x x) (* y y) (* z z)))]
    (if (< d (num/f32 1.0E-5))
      [0.0 0.0 0.0]
      [(/ x d) (/ y d) (/ z d)])))

(defn pulled
  "Returns velocity vel of an orb at pos pulled towards player p."
  [vel pos p]
  (let [pp (:pos p)
        dx (- (v/x pp) (v/x pos))
        dy (- (+ (v/y pp) (/ (num/f32 (entity/eye-height p)) 2.0))
              (v/y pos))
        dz (- (v/z pp) (v/z pos))
        power (- 1.0 (/ (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
                        8.0))
        k (* power power 0.1)
        [nx ny nz] (normal dx dy dz)]
    [(+ (v/x vel) (* nx k)) (+ (v/y vel) (* ny k))
     (+ (v/z vel) (* nz k))]))

(defn- cell-state ^long [chunks x y z]
  (if (chunk/in-range? (long y))
    (long (chunk/at chunks [x y z]))
    0))

(defn eye-in-water?
  [chunks pos]
  (let [ey (+ (v/y pos) (eye))
        x (num/floor (v/x pos)) y (num/floor ey)
        z (num/floor (v/z pos))
        st (cell-state chunks x y z)]
    (boolean
      (when (block/water? st)
        (when-let [h (liquid/fluid-height-of chunks [x y z] st nil)]
          (<= ey (+ (double y) (double h))))))))

(defn in-lava?
  "Returns true when the block an orb at pos is in holds lava."
  [chunks pos]
  (let [x (num/floor (v/x pos)) y (num/floor (v/y pos))
        z (num/floor (v/z pos))]
    (block/lava? (cell-state chunks x y z))))

(defn colliding?
  "Returns true when an orb at pos, moved by d, meets a block."
  ([chunks pos] (colliding? chunks pos [0.0 0.0 0.0]))
  ([chunks pos d]
   (not (phys/free? chunks pos (half) (height)
                    (v/x d) (v/y d) (v/z d)))))

(defn swum
  "Returns velocity vel of an orb whose eye is in water."
  [vel]
  [(* (v/x vel) (num/f32 0.99))
   (min (+ (v/y vel) (num/f32 5.0E-4)) (num/f32 0.06))
   (* (v/z vel) (num/f32 0.99))])

(defn fallen
  "Returns velocity vel of an orb after one tick of gravity."
  [vel]
  [(v/x vel) (- (v/y vel) gravity) (v/z vel)])

(defn tossed
  "Returns the velocity lava throws an orb up with."
  [roll]
  (let [draw #(num/f32 (roll %))
        side #(num/fmul (num/fsub (draw [% 0]) (draw [% 1]))
                        (num/f32 0.2))]
    [(side :lx) (num/f32 0.2) (side :lz)]))

(def ^:private below-offset (num/f32 0.999999))

(defn- friction ^double [chunks pos sup on-ground]
  (let [air (num/f32 0.98)]
    (if on-ground
      (let [st (motion/below-state chunks pos sup below-offset)]
        (num/f32 (* air (motion/friction st))))
      air)))

(defn- supported [chunks mv]
  (when (phys/on-ground? mv)
    (phys/supporting-block chunks (phys/pos mv) (half))))

(defn- sped [chunks mv sup on-ground]
  (let [p (phys/pos mv)
        sf (motion/block-speed-factor chunks p sup)
        w (phys/vel mv)
        vel [(* (v/x w) sf) (v/y w) (* (v/z w) sf)]]
    (if on-ground (motion/stepped-speed chunks p vel) vel)))

(defn moved
  "Returns the position, velocity, ground flag and support of an orb
  after it moves by vel. stuck is what a cobweb left on the move."
  [chunks pos vel stuck]
  (let [push (if stuck (mapv * vel stuck) vel)
        mv (phys/move chunks pos push (half) (height))
        og (phys/on-ground? mv)
        sup (supported chunks mv)]
    [(phys/pos mv)
     (if stuck [0.0 0.0 0.0] (sped chunks mv sup og))
     og sup]))

(defn- bounced ^double [^double wy on-ground ^double fall]
  (if (and on-ground (< fall (- gravity))) (* (- fall) 0.4) wy))

(defn slowed
  "Returns velocity w of an orb at pos after friction, and the bounce
  of a fall faster than fall speed when it lands."
  [chunks pos w on-ground sup fall]
  (let [f (friction chunks pos sup on-ground)
        [wx wy wz] (mapv #(* (double %) f) w)]
    [wx (bounced wy on-ground (double fall)) wz]))
