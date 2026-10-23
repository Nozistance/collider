(ns collider.game.ride
  "A boat its only player takes off the level and brings back."
  (:require [collider.game.entity :as entity]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(def ^:private boat-keys
  [:type :yaw :pitch :on-ground :cargo :hull :fall :boat-status
   :slide :last-yd :hurt-time :hurt-dir :paddle-left :paddle-right])

(defn- nums [p]
  (if (v/v3? p) [(v/x p) (v/y p) (v/z p)] p))

(defn- snapshot [e]
  (into {} (remove (comp nil? val))
        (assoc (select-keys e boat-keys)
               :pos (nums (:pos e))
               :vel (nums (or (:vel e) [0.0 0.0 0.0])))))

(defn- root-id [entities id]
  (loop [id id seen #{}]
    (if-let [up (:vehicle (get entities id))]
      (if (seen up) id (recur up (conj seen id)))
      id)))

(defn- player? [entities id]
  (entity/player? (get entities id)))

(defn- sole? [entities id]
  (let [e (get entities id)
        ps (:passengers e)]
    (and (entity/boat? (:type e))
         (seq ps)
         (= 1 (count (filter #(player? entities %) ps))))))

(defn- seat [entities tick id]
  (let [e (get entities id)]
    (if (entity/player? e) :self (entity/saved e tick))))

(defn carried
  "Returns the boat of player eid when that player is its only one.
  Any other vehicle gives nil. Tick is when its riders are saved."
  [entities tick eid]
  (when-let [vid (:vehicle (get entities eid))]
    (let [id (root-id entities vid)]
      (when (sole? entities id)
        {:boat (snapshot (get entities id))
         :seats (mapv #(seat entities tick %)
                      (:passengers (get entities id)))}))))

(defn- absent-player? [entities id]
  (let [e (get entities id)]
    (or (nil? e) (entity/player? e))))

(defn- carried-boat? [entities e]
  (let [ps (:passengers e)]
    (and (entity/boat? (:type e))
         (seq ps)
         (= 1 (count (filter #(absent-player? entities %) ps))))))

(defn- carried-ids [entities id e]
  (when (carried-boat? entities e)
    (cons id (remove #(absent-player? entities %) (:passengers e)))))

(defn withheld-ids
  "Returns ids a chunk save omits.
  They travel with their one player, already gone from the chunk."
  [entities]
  (let [es (if (map? entities) entities (into {} entities))]
    (into #{} (mapcat (fn [[id e]] (carried-ids es id e))) es)))

(defn- riders [entities id]
  (cons id (mapcat #(riders entities %)
                   (:passengers (get entities id)))))

(defn drop-carried
  "Returns w after player eid takes its boat and riders away."
  [w eid]
  (let [es (:entities w)
        id (root-id es (:vehicle (get es eid)))
        gone (remove #(player? es %) (riders es id))]
    (assoc w :entities (reduce dissoc es gone))))

(defn- seat-spawn [w tick player-eid seat spawn]
  (if (map? seat)
    (spawn w (entity/loaded seat tick))
    [w player-eid]))

(defn- placed-seats [w tick player-eid seats spawn]
  (reduce (fn [[w ids] seat]
            (let [[w id] (seat-spawn w tick player-eid seat spawn)]
              [w (conj ids id)]))
          [w []] seats))

(defn- tie [w boat-id ids]
  (reduce (fn [w id]
            (cond-> w
              (get-in w [:entities id])
              (update-in [:entities id] assoc :vehicle boat-id)))
          w ids))

(defn- restore [w player-eid player parked spawn]
  (let [tick (long (:tick w 0))
        [w ids] (placed-seats w tick player-eid (:seats parked) spawn)
        spec (assoc (:boat parked) :passengers ids)
        [w boat-id] (spawn w spec)]
    [(tie w boat-id ids) (assoc player :vehicle boat-id)]))

(defn mount
  "Returns [world player] with player seated in parked.
  spawn takes the world and an entity and returns [world eid]."
  [w player-eid player parked spawn]
  (if (map? parked)
    (restore w player-eid player parked spawn)
    [w player]))
