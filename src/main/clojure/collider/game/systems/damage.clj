(ns collider.game.systems.damage
  "The hurts of players and items each tick."
  (:require [collider.game.apply :as apply]
            [collider.game.areas :as areas]
            [collider.game.block.screen :as screen]
            [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.entity.arrows :as arrows]
            [collider.game.entity.hurt :as hurt]
            [collider.num :as num]
            [collider.parallel :as par]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private loose-types #{:item :experience-orb})

(defn- chunk-coord ^long [^double a]
  (bit-shift-right (num/floor a) 4))

(defn- has-chunk? [chunks x z]
  (some? (get chunks (chunk/pos->id x z))))

(defn- loaded-near? [world e]
  (let [chunks (:chunks world) p (:pos e)
        x0 (chunk-coord (- (v/x p) 0.5))
        x1 (chunk-coord (+ (v/x p) 0.5))
        z0 (chunk-coord (- (v/z p) 0.5))
        z1 (chunk-coord (+ (v/z p) 0.5))]
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

(defn- removal-deltas [world eid e]
  (when-let [ds (seq (hurt/removal-deltas world eid e))]
    (concat ds (screen/removed-deltas world eid e))))

(defn- busy-deltas [world eid e]
  (let [ds (concat (hurt/timer-deltas world eid e)
                   (removal-deltas world eid e)
                   (hurt/landing-deltas world eid e)
                   (hurt/fire-deltas world eid e))]
    (->> (concat (hurt/burnt-deltas world eid e) ds)
         (hurt/hurt-now world eid e)
         (hurt/report-deltas world eid)
         (concat ds))))

(defn- mob-deltas [world eid e]
  (if (idle? world e)
    (hurt/fire-deltas world eid e)
    (busy-deltas world eid e)))

(defn- body-deltas [world eid e]
  (if (contains? loose-types (:type e))
    (hurt/item-deltas world eid e)
    (mob-deltas world eid e)))

(defn- stirred? [world e]
  (if (entity/player? e)
    (loaded-near? world e)
    (hurt/stirred? world e)))

(defn- live? [world [_ e]]
  (and (some? (:health e))
       (or (not (idle? world e)) (stirred? world e))))

(defn- ticking? [active e]
  (or (entity/player? e) (areas/active-at? active (:pos e))))

(defn- due [world]
  (let [active (areas/active-chunks world)]
    (fn [[_ e :as entry]]
      (and (ticking? active e)
           (contains? loose-types (:type e))
           (live? world entry)))))

(defn- bodies [world due?]
  (comp (filter due?)
        (remove (fn [[_ e]] (hurt/by-pot? world e)))
        (mapcat (fn [[eid e]] (body-deltas world eid e)))))

(defn- in-turn
  "Returns the deltas of the items and orbs in cauldrons of water or
  snow, each on the world the ones before it left."
  [world due?]
  (first
    (reduce (fn [[ds w] [eid e]]
              (let [d (vec (body-deltas w eid e))]
                [(into ds d) (if (seq d) (first (apply/applied w d)) w)]))
            [[] world]
            (filter (fn [[_ e :as entry]]
                      (and (due? entry) (hurt/by-pot? world e)))
                    (:entities world)))))

(def ^:private ^:const bodies-leaf 64)

(defn player-deltas
  "Returns the deltas of the fire and the void of player eid in its
  base tick, with the hurts they report."
  [world [eid e :as entry]]
  (when (live? world entry)
    (-> (vec (hurt/burnt-deltas world eid e))
        (into (hurt/wall-deltas world eid e))
        (into (hurt/air-deltas world eid e))
        (into (hurt/touch-deltas world eid e))
        (into (body-deltas world eid e))
        (into (arrows/loose-deltas eid e)))))

(defn damage
  "Returns the deltas of every item and orb this tick. A mob takes
  its own in its turn, a player in its tick after the level."
  {:wake {:keys [:entities]}}
  [world _d]
  (let [due? (due world)]
    (deltas/of-vec
      (into (par/select (bodies world due?) (:entities world) bodies-leaf)
            (in-turn world due?)))))
