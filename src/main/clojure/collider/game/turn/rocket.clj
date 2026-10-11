(ns collider.game.turn.rocket
  "The turn of a firework rocket. It speeds up as it climbs, slides
  along what stops it and goes off when its life runs out or it meets
  a body."
  (:require [collider.game.entity :as entity]
            [collider.game.entity.hurt :as hurt]
            [collider.game.entity.size :as size]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.projectile :as projectile]
            [collider.game.stack :as stack]
            [collider.num :as num]
            [collider.parallel :as par]
            [collider.vec :as v]
            [collider.world.blocks.motion :as motion]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(defn- half ^double [] (size/half :firework-rocket))

(defn- height ^double [] (size/height :firework-rocket))

(defn- sped-up [e]
  (let [k (if (:horizontal-collision e) 1.0 1.15)]
    [(* (double (v/x (:vel e))) k) (+ (double (v/y (:vel e))) 0.04)
     (* (double (v/z (:vel e))) k)]))

(defn- moved [world e d]
  (let [stuck (:stuck e)
        m (if stuck (mapv * d stuck) d)
        mv (phys/move (:chunks world) (:pos e) m (half) (height))
        at (phys/pos mv)]
    {:pos at :on-ground (phys/on-ground? mv)
     :horizontal-collision (or (not= (v/x m) (v/x (phys/vel mv)))
                (not= (v/z m) (v/z (phys/vel mv))))
     :stuck (motion/stuck-speed (:chunks world) at (half) (height))}))

(defn- stars ^long [e]
  (count (:explosions (stack/component (:stack e) :fireworks))))

(defn- source [world eid e at]
  (let [o (some->> (:owner e) (get (:entities world)))]
    {:type :fireworks :cause (when o (:owner e)) :direct eid :from at
     :player? (= :player (:type o)) :attacker o :direct-attacker e}))

(defn- seen? [world at t]
  (let [h (second (entity/box t))]
    (some (fn [^double k]
            (let [[x y z] (:pos t)
                  to [x (+ (double y) (* h k)) z]]
              (nil? (projectile/block-clip world at (mapv - to at)))))
          [0.0 0.5])))

(defn- near [world at]
  (for [[oid t] (par/keyed (:entities world))
        :when (and (projectile/hittable? t) (not (game-mode/spectator? t))
                   (<= (v/dist-sq at (:pos t)) 25.0)
                   (seen? world at t))]
    [oid t]))

(defn- distance
  "Returns how far t is from at, in floats as an entity measures it."
  ^double [at t]
  (let [[dx dy dz] (map #(num/f32 (- (double %1) (double %2))) at (:pos t))
        sq (num/f32 (+ (num/f32 (+ (num/f32 (* dx dx)) (num/f32 (* dy dy))))
                       (num/f32 (* dz dz))))]
    (num/f32 (Math/sqrt sq))))

(defn- blast-deltas [world eid e at]
  (let [n (stars e)]
    (when (pos? n)
      (let [amount (num/f32 (+ 5.0 (* 2.0 n)))
            src (source world eid e at)]
        (for [[oid t] (near world at)
              :let [d (distance at t)
                    hit (num/f32 (* amount (num/f32 (Math/sqrt (/ (- 5.0 d) 5.0)))))
                    ds (hurt/damage-deltas world oid t hit src)]
              x (when ds
                  (into ds (hurt/report-deltas
                             world oid (hurt/hurt-now world oid t ds))))]
          x)))))

(defn- explosion [world eid e at]
  (concat [(out/all (out/status eid :fireworks))]
          (blast-deltas world eid e at)
          [[:remove-entity eid]]))

(defn- launch [e at]
  (when (zero? (long (:life e 0)))
    [(out/all (out/sound :firework-rocket/launch at 3.0 1.0))]))

(defn step-deltas
  "Returns the deltas of the turn of firework rocket e of eid."
  [world eid e]
  (if (< (v/y (:pos e)) (chunk/void-y world))
    [[:remove-entity eid]]
    (let [left? (projectile/left-owner? world e (:vel e))
          d (sped-up e)
          hit (projectile/clip world eid (assoc e :left-owner? left?) d)
          m (moved world e d)
          [yaw pitch] (projectile/rotated e d)
          life (inc (long (:life e 0)))
          met? (or (= :entity (:kind hit))
                   (and (= :block (:kind hit)) (pos? (stars e))))
          at (:pos m)]
      (concat
        (when met? (explosion world eid e at))
        (launch e at)
        (cond met? nil
              (> life (long (:lifetime e 0))) (explosion world eid e at)
              :else [[:merge-entity eid
                      (assoc m :vel d :left-owner? left? :yaw yaw
                             :pitch pitch :life life
                             :age (inc (long (:age e 0))))]])))))
