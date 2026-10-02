(ns collider.game.systems.damage
  "The hurts of players and items each tick."
  (:require [collider.game.deltas :as deltas]
            [collider.game.entity.hurt :as hurt]
            [collider.game.mob.mobs :as mobs]
            [collider.world.chunk :as chunk]
            [collider.game.areas :as areas]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defn- chunk-x ^long [^double a]
  (bit-shift-right (long (Math/floor a)) 4))

(defn- has-chunk? [chunks x z]
  (some? (get chunks (chunk/pos->id x z))))

(defn- loaded-near? [world e]
  (let [chunks (:chunks world) p (:pos e)
        x0 (chunk-x (- (v/x p) 0.5)) x1 (chunk-x (+ (v/x p) 0.5))
        z0 (chunk-x (- (v/z p) 0.5)) z1 (chunk-x (+ (v/z p) 0.5))]
    (or (has-chunk? chunks x0 z0)
        (and (not= x0 x1) (has-chunk? chunks x1 z0))
        (and (not= z0 z1) (has-chunk? chunks x0 z1))
        (and (not= x0 x1) (not= z0 z1)
             (has-chunk? chunks x1 z1)))))

(defn- idle? [world e]
  (let [health (double (or (:health e) 0.0))]
    (and (pos? health)
         (>= (v/y (:pos e)) (chunk/void-y world))
         (not (pos? (long (or (:fire e) 0))))
         (not (:burning? e))
         (zero? (long (or (:hurt-resist e) 0)))
         (nil? (:landed e))
         (nil? (:struck-by e))
         (>= health (double (or (:health-sent e) health))))))

(defn- busy-deltas [world eid e]
  (let [ds (concat (hurt/timer-deltas eid e)
                   (hurt/landing-deltas world eid e)
                   (hurt/fire-deltas world eid e))]
    (->> (concat [[:rest eid]] (hurt/burnt-deltas world eid e) ds)
         (hurt/hurt-now world eid e)
         (hurt/report-deltas world eid)
         (concat ds))))

(defn- mob-deltas [world eid e]
  (if (idle? world e)
    (hurt/fire-deltas world eid e)
    (busy-deltas world eid e)))

(defn- living-deltas [world eid e]
  (if (contains? #{:item :experience-orb} (:type e))
    (hurt/item-deltas world eid e)
    (mob-deltas world eid e)))

(defn- stirred? [world e]
  (if (= :player (:type e))
    (loaded-near? world e)
    (pos? (hurt/probe world e))))

(defn- live? [world [_ e]]
  (and (some? (:health e))
       (or (not (idle? world e)) (stirred? world e))))

(defn- ticking? [active e]
  (or (= :player (:type e)) (areas/active-at? active (:pos e))))

(defn- living [world]
  (let [active (areas/active-chunks world)
        due? (fn [[_ e :as entry]]
               (and (ticking? active e)
                    (not (mobs/mob-type? (:type e)))
                    (live? world entry)))]
    (comp (filter due?)
          (mapcat (fn [[eid e]] (living-deltas world eid e))))))

(def ^:private ^:const living-leaf 64)

(defn- own-tick? [e]
  (let [k (:type e)]
    (or (mobs/mob-type? k) (contains? #{:item :experience-orb} k))))

(defn burning
  "Returns the deltas of every player from the start of
  LivingEntity.baseTick: fire, then the void. They come after the
  turns of the entities, where a player counts down its hurt
  resistance (ServerPlayer.tick:614); a mob does all of it there."
  {:wake {:keys [:entities]}}
  [world _d]
  (let [active (areas/active-chunks world)
        due? (fn [[_ e]]
               (and (ticking? active e) (not (own-tick? e))
                    (hurt/based? world e)))
        burnt (fn [[eid e]] (hurt/burnt-deltas world eid e))
        xf (comp (filter due?) (mapcat burnt))]
    (deltas/of-vec (deltas/select xf (:entities world)))))

(defn damage
  "Returns the deltas of every player and item this tick.
  A mob takes its own in its turn."
  {:wake {:keys [:entities]}}
  [world _d]
  (deltas/of-vec
    (deltas/select (living world) (:entities world) living-leaf)))
