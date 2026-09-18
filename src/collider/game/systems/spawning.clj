(ns collider.game.systems.spawning
  "Placement of joining and respawning players."
  (:require [collider.game.state :as state]
            [collider.game.systems.chunks :as chunks]
            [collider.game.systems.damage :as damage]
            [collider.world.chunk :as chunk]
            [collider.world.space.spawn :as spawn]))

(set! *warn-on-reflection* true)

(defn- radius ^long [w]
  (long (get-in w [:rules :respawn-radius] 10)))

(defn- search-ids [w]
  (spawn/search-chunk-ids (:world-spawn w) (radius w)))

(def ^:private ^:const arrival-radius 3)

(defn- loaded? [w ids] (every? #(contains? (:chunks w) %) ids))

(defn- found [w req]
  (let [{:keys [chunks world-spawn]} w]
    (spawn/find-spawn chunks world-spawn (radius w) (:seed req))))

(defn- arrival-ids [pos]
  (let [[cx cz] (chunk/id->pos (chunk/pos-chunk pos))]
    (chunk/around-ids (long cx) (long cz) arrival-radius)))

(defn- join-pos [w req]
  (or (:pos req) (when (loaded? w (search-ids w)) (found w req))))

(defn- join-ids [w req]
  (if-let [pos (join-pos w req)]
    (concat (when-not (:pos req) (search-ids w)) (arrival-ids pos))
    (search-ids w)))

(defn- need-ids [w eid req]
  (if (:respawn? req)
    (concat (damage/respawn-chunk-ids (get-in w [:entities eid]))
            (search-ids w))
    (join-ids w req)))

(defn- respawned [w eid req]
  (let [e (get-in w [:entities eid])
        bed (damage/bed-respawn (:chunks w) e)
        [pos yaw pitch] (or bed [(found w req) 0.0 0.0])
        lost? (and (nil? bed) (some? (damage/respawn-config e)))]
    (conj (damage/respawn-deltas w eid [pos yaw pitch lost?])
          [:spawn-progress eid nil])))

(defn- placed [w eid req]
  (if (:respawn? req)
    (respawned w eid req)
    [[:player-placed eid (:name req) (or (:pos req) (found w req))]]))

(defn- loading [[w acc] need]
  (let [ds (vec (chunks/loading-deltas w need))
        w' (if (seq ds) (first (state/apply-deltas w ds)) w)]
    [w' (into acc ds)]))

(defn- settle [[world acc] [eid req]]
  (let [[w acc] (loading [world acc] (need-ids world eid req))
        [w acc] (loading [w acc] (need-ids w eid req))
        need (vec (need-ids w eid req))]
    [w (into acc (if (loaded? w need)
                   (placed w eid req)
                   [[:spawn-progress eid (assoc req :need need)]]))]))

(defn placing
  "Places the players whose spawn chunks are loaded.
  They are the joining and respawning ones. The chunks the
  others wait for are loaded."
  [world _]
  (second (reduce settle [world []] (sort-by key (:spawning world)))))
