(ns collider.game.mob.bat
  "The bat, which hangs under blocks and flutters about."
  (:require [collider.game.level :as level]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.sense :as sense]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.num :as num]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn roll
  "Returns a whole number from 0 below n that tick t, bat eid and key
  k draw."
  ^long [t eid k ^long n]
  (random/below (random/rnd t eid k) n))

(defn- cell [p] (mapv #(num/floor (double %)) p))

(defn- conductor? [world [x y z]]
  (block/conductor? (sense/block-at world x (inc (long y)) z)))

(def ^:private ^:const wake-range 4.0)

(defn- wakes? [world e]
  (some (fn [[_ p]]
          (and (game-mode/seen? p)
               (sense/in-range? (:pos e) p wake-range)
               (animal/in-sight? world e p)))
        (level/player-entries world)))

(defn- woken [eid e at]
  [(assoc e :resting? false)
   [[:merge-entity eid {:resting? false}]
    (out/all (out/level-event :sound-bat-liftoff at))]])

(defn- turned [eid e t]
  (if (zero? (roll t eid :turn 200))
    (assoc e :head-yaw (double (roll t eid :head 360)))
    e))

(defn- hung [world eid e t at]
  (if (conductor? world at)
    (let [e (turned eid e t)]
      (if (wakes? world e) (woken eid e at) [e nil]))
    (woken eid e at)))

(defn- gone? [world [x y z]]
  (or (not (block/air? (sense/block-at world x y z)))
      (<= (long y) (chunk/level-min-y world))))

(defn- near? [g p]
  (< (v/dist-sq (mapv #(+ (long %) 0.5) g) p) 4.0))

(defn- spread [c t eid a b]
  (- (+ (double c) (roll t eid a 7)) (roll t eid b 7)))

(defn- drawn [t eid [x y z]]
  (cell [(spread x t eid :x1 :x2)
         (- (+ (double y) (roll t eid :y 6)) 2.0)
         (spread z t eid :z1 :z2)]))

(defn- target-of [world eid e t]
  (let [g (:bat-target e)
        g (when-not (and g (gone? world g)) g)]
    (if (or (nil? g) (zero? (roll t eid :retarget 30))
            (near? g (:pos e)))
      (drawn t eid (:pos e))
      g)))

(defn- flown [world eid e t at]
  (let [g (target-of world eid e t)
        rests? (and (zero? (roll t eid :rest 100))
                    (conductor? world at))
        m (cond-> {}
            (not= g (:bat-target e)) (assoc :bat-target g)
            rests? (assoc :resting? true))]
    [(merge (assoc e :flap g) m)
     (when (seq m) [[:merge-entity eid m]])]))

(defn custom-step
  "Returns living bat e after its own server step at tick t, with its
  deltas. A hanging bat wakes without a block to hang from or with a
  player near. A flying one picks where to flutter and may hang."
  [world eid e t]
  (let [at (cell (:pos e))]
    (if (:resting? e)
      (hung world eid e t at)
      (flown world eid e t at))))

(def ^:private ^:const pull (double (float 0.1)))

(def ^:private ^:const climb (double (float 0.7)))

(defn- toward ^double [^double v ^double d ^double s]
  (+ v (* (- (* (Math/signum d) s) v) pull)))

(defn- yaw-to ^double [^double yaw vel]
  (let [r (num/degrees (num/atan2 (v/z vel) (v/x vel)))
        d (num/fsub (num/f32 r) 90.0)]
    (num/f32 (+ yaw (num/wrap-degrees (num/fsub d yaw))))))

(defn drive
  "Returns the speed vel of bat e turned toward where it flutters this
  tick, and its yaw along that speed, or nil when it does not."
  [e vel]
  (when-let [[x y z] (:flap e)]
    (let [p (:pos e)
          d (fn [c ^double o a] (- (+ (long c) o) (double a)))
          w (v/v3 (toward (v/x vel) (d x 0.5 (v/x p)) 0.5)
                  (toward (v/y vel) (d y 0.1 (v/y p)) climb)
                  (toward (v/z vel) (d z 0.5 (v/z p)) 0.5))]
      [w (yaw-to (double (:yaw e)) w)])))

(def ^:private ^:const height (double (float 0.9)))

(defn- hung-y ^double [^double y] (- (+ (Math/floor y) 1.0) height))

(defn ai-step
  "Returns bat e at the end of its tick. A hanging bat stops and
  clings to the top of its block, a flying one loses vertical speed."
  [_ e _]
  (let [p (:pos e) vel (:vel e)
        e (if (:resting? e)
            (assoc e :vel (v/v3 0.0 0.0 0.0)
                   :pos (v/v3 (v/x p) (hung-y (v/y p)) (v/z p)))
            (assoc e :vel
                   (v/v3 (v/x vel) (* (v/y vel) 0.6) (v/z vel))))]
    [(assoc e :flap nil) nil]))

(defn hushed?
  "Returns true when bat e keeps its ambient sound to itself at tick
  t. A hanging bat is heard one time in four."
  [e t eid]
  (and (:resting? e) (pos? (roll t eid :hush 4))))
