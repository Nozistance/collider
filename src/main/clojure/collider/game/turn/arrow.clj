(ns collider.game.turn.arrow
  "The turn of an arrow in flight, in the ground and falling out of
  it."
  (:require [collider.game.entity :as entity]
            [collider.game.out :as out]
            [collider.game.projectile :as projectile]
            [collider.num :as num]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ^:private ^:const drag (num/f32 0.99))

(def ^:private ^:const water-drag (num/f32 0.6))

(def ^:private ^:const gravity 0.05)

(def ^:private ^:const set-back (num/f32 0.05))

(def ^:private ^:const shake-ticks 7)

(def ^:private ^:const lifetime 1200)

(def ^:private ^:const fall-margin 0.06)

(def ^:private still [0.0 0.0 0.0])

(defn- scaled [d ^double k]
  [(* k (v/x d)) (* k (v/y d)) (* k (v/z d))])

(defn- held? [st cell p]
  (some #(projectile/inside? (projectile/abs-box cell %) p)
        (block/collision-boxes st)))

(defn- falls? [world e]
  (let [[x y z] (:pos e) m fall-margin]
    (phys/box-free? (:chunks world)
                    [(- x m) (- y m) (- z m) (+ x m) (+ y m) (+ z m)]
                    (- y m))))

(defn- shaken ^long [e]
  (max 0 (dec (long (:shake e 0)))))

(defn- lying-deltas [world eid e st]
  (let [m {:in-ground? true :vel still :shake (shaken e)
           :age (inc (long (:age e 0))) :needs-sync? false}
        life (inc (long (:life e 0)))]
    (cond
      (and (not= (:last-state e) st) (falls? world e))
      [[:merge-entity eid (assoc m :in-ground? false :life 0)]]
      (>= life lifetime) [[:remove-entity eid]]
      :else [[:merge-entity eid (assoc m :life life)]])))

(defn- hit-sound [world eid at]
  (let [r (num/f32 (random/of-key (:tick world) eid :arrow-hit))
        d (num/f32 (+ (num/f32 (* r (num/f32 0.2))) (num/f32 0.9)))]
    (out/all (out/sound :arrow/hit at 1.0 (num/f32 (/ (num/f32 1.2) d))))))

(defn- stuck-at [at d]
  (mapv (fn [a c] (- (double a) (* set-back (Math/signum (double c)))))
        at d))

(defn- after-move [e d]
  (cond-> d
    (not (:in-water? e)) (scaled drag)
    (not (:in-ground? e)) (v/add [0.0 (- gravity) 0.0])))

(defn- wet [world e]
  (let [[half h] (entity/box e)
        f (liquid/fluid-info (:chunks world) (:pos e) half h (:vel e))]
    (cond-> {:in-water? (pos? (double (:water f)))}
      (not (:in-ground? e)) (assoc :vel (v/add (:vel e) (:push f))))))

(defn- moved [world e at d hit]
  (let [e (if hit
            (assoc e :pos (stuck-at at d) :vel still :in-ground? true
                   :last-state (chunk/at (:chunks world) (:cell hit))
                   :shake shake-ticks :crit? false :needs-sync? true)
            (assoc e :pos at :needs-sync? false))
        e (assoc e :vel (after-move e (:vel e)))]
    (merge e (wet world e))))

(def ^:private flown-keys
  [:pos :vel :yaw :pitch :left-owner? :shake :age :in-ground?
   :in-water? :last-state :crit? :needs-sync?])

(defn- flight-deltas [world eid e]
  (let [move (:vel e)
        d (if (:in-water? e) (scaled move water-drag) move)
        [yaw pitch] (projectile/rotated e move)
        span (projectile/span (:pos e) move)
        b (projectile/block-clip world (:pos e) span)
        at (if b (projectile/point (:pos e) span (b 0)) (v/add (:pos e) move))
        hit (when b {:cell (nth b 2)})
        e' (moved world
                  (assoc e :vel d :yaw yaw :pitch pitch
                         :left-owner? (projectile/left-owner? world e d)
                         :shake (shaken e) :age (inc (long (:age e 0))))
                  at d hit)]
    (if (< (v/y (:pos e')) (chunk/void-y world))
      [[:remove-entity eid]]
      (cond-> [[:merge-entity eid (select-keys e' flown-keys)]]
        hit (conj (hit-sound world eid (:pos e')))
        hit (into (projectile/block-hit-deltas world e (:cell hit)))))))

(defn step-deltas
  "Returns the deltas of the turn of arrow e of eid."
  [world eid e]
  (let [p (:pos e) cell (v/cell p)
        st (if (chunk/in-range? (nth cell 1))
             (chunk/at (:chunks world) cell)
             0)]
    (if (or (:in-ground? e) (held? st cell p))
      (lying-deltas world eid e st)
      (flight-deltas world eid e))))

(defn reach
  "Returns the box [x0 z0 x1 z1] that arrow e sweeps in its move
  this tick."
  [e]
  (let [p (:pos e) q (if (:in-ground? e) p (v/add p (:vel e)))]
    [(min (v/x p) (v/x q)) (min (v/z p) (v/z q))
     (max (v/x p) (v/x q)) (max (v/z p) (v/z q))]))
