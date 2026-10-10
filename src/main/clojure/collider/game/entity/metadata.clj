(ns collider.game.entity.metadata
  "The metadata an entity shows a client."
  (:require [collider.game.effect :as effect]
            [collider.game.entity :as entity]
            [collider.game.hanging :as hanging]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mode :as game-mode]))

(set! *warn-on-reflection* true)

(defn- thrown-metadata [e] {:stack (:stack e)})

(defn- orb-metadata [e]
  (cond-> {:value (:value e)} (:burning? e) (assoc :burning? true)))

(defn- item-metadata [e]
  (cond-> {:stack (:stack e)}
          (:burning? e) (assoc :burning? true)))

(defn- facing-metadata [e]
  (let [f (:facing e)]
    (if (= :south f) {} {:facing f})))

(defn- painting-metadata [e]
  (let [v (:variant e)]
    (cond-> (facing-metadata e)
      (not= v (hanging/default-variant)) (assoc :variant v))))

(defn- frame-metadata [e]
  (let [r (long (or (:rotation e) 0))]
    (cond-> (facing-metadata e)
      (:stack e) (assoc :stack (:stack e))
      (pos? r) (assoc :rotation r))))

(def ^:private simple-metadata
  (merge
    {:painting painting-metadata
     :item-frame frame-metadata
     :glow-item-frame frame-metadata
     :item item-metadata
     :experience-orb orb-metadata
     :tnt (fn [e] {:fuse (:fuse e)})
     :falling-block (fn [e] {:start (:start e)})
     :area-effect-cloud
     (fn [e] {:radius (:radius e) :color (:color e)
              :waiting? (boolean (:waiting? e))})}
    (zipmap entity/thrown-types (repeat thrown-metadata))
    {:wind-charge (constantly {})}))

(defn- using-hand [e]
  (if (:using-item? e) (or (get-in e [:using :hand]) :main) false))

(defn- player-metadata [e]
  (cond-> {:burning?    (boolean (:burning? e))
           :sneaking?   (boolean (:sneaking? e))
           :sprinting?  (boolean (:sprinting? e))
           :using-item? (using-hand e)
           :swimming?   (boolean (:swimming? e))
           :pose        (or (:pose e) :standing)
           :skin-parts  (long (or (:skin-parts e) 0))}
          (:sleeping e)
          (assoc :sleeping-pos (get-in e [:sleeping :pos]))
          (pos? (double (:absorption e 0.0)))
          (assoc :absorption (:absorption e))
          (not (zero? (long (:score e 0))))
          (assoc :score (:score e))
          (game-mode/spectator? e) (assoc :invisible? true)))

(defn- effect-particles [e fx]
  (when-not (game-mode/spectator? e)
    (not-empty (effect/particles fx))))

(defn- effect-metadata [e fx]
  (let [ps (effect-particles e fx)]
    (cond-> {}
      ps (assoc :effect-particles ps)
      (effect/all-ambient? fx) (assoc :effect-ambience true)
      (contains? fx :invisibility) (assoc :invisible? true)
      (contains? fx :glowing) (assoc :glowing? true))))

(defn- own-metadata [e]
  (if-let [f (simple-metadata (:type e))]
    (f e)
    (if (mobs/mob-type? (:type e))
      (mobs/metadata e)
      (player-metadata e))))

(defn of
  [e]
  (let [m (cond-> (own-metadata e)
            (not= 300 (long (:air e 300))) (assoc :air-supply (:air e))
            (pos? (long (or (:ticks-frozen e) 0)))
            (assoc :ticks-frozen (:ticks-frozen e)))
        fx (:effects e)]
    (cond (seq fx) (merge m (effect-metadata e fx))
          (:ambience e) (assoc m :effect-ambience true)
          :else m)))
