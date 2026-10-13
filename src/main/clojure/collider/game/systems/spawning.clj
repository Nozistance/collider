(ns collider.game.systems.spawning
  "Placement of joining and respawning players."
  (:require [collider.game.deltas :as deltas]
            [collider.game.apply :as apply]
            [collider.game.level :as level]
            [collider.game.player :as player]
            [collider.game.respawn :as respawn]
            [collider.game.systems.chunks :as chunks]
            [collider.world.chunk :as chunk]
            [collider.world.space.spawn :as spawn]))

(set! *warn-on-reflection* true)

(defn- radius ^long [w]
  (long (get-in w [:rules :respawn-radius] 10)))

(defn- suggestion [w req]
  (if (:respawn? req) (player/respawn-at w) (:world-spawn w)))

(defn- search-ids [w req]
  (spawn/search-chunk-ids (suggestion w req) (radius w)))

(def ^:private ^:const arrival-radius 3)

(defn- loaded? [w ids] (every? #(contains? (:chunks w) %) ids))

(defn- found [w req]
  (spawn/find-spawn (:chunks w) (suggestion w req) (radius w)
                    (:seed req)))

(defn- arrival-ids [pos]
  (let [[cx cz] (chunk/id->pos (chunk/pos-chunk pos))]
    (chunk/around-ids (long cx) (long cz) arrival-radius)))

(defn- join-pos [w req]
  (or (:pos req)
      (when (loaded? w (search-ids w req)) (found w req))))

(defn- join-ids [w req]
  (if-let [pos (join-pos w req)]
    (concat (when-not (:pos req) (search-ids w req))
            (arrival-ids pos))
    (search-ids w req)))

(defn- loading [[w acc] need]
  (let [ds (vec (chunks/loading-deltas w need))
        w' (if (seq ds) (first (apply/applied w ds)) w)]
    [w' (into acc ds)]))

(defn- waiting [eid req need]
  [[:spawn-progress eid (assoc req :need need)]])

(defn- joined [[world acc] [eid req]]
  (let [[w acc] (loading [world acc] (join-ids world req))
        [w acc] (loading [w acc] (join-ids w req))
        need (vec (join-ids w req))]
    [w (into acc (if (loaded? w need)
                   [[:player-placed eid (:name req)
                     (or (:pos req) (found w req))]]
                   (waiting eid req {(:dim req :overworld) need})))]))

(defn- view [w dim eid]
  (if (= dim (:dim w :overworld))
    w
    (-> (level/level (:server w) dim)
        (assoc :server (:server w))
        (assoc-in [:entities eid] (get-in w [:entities eid])))))

(defn- respawn-need [w eid e req]
  (let [dim (player/respawn-dimension w)]
    (merge-with concat
                (when-let [cfg (respawn/respawn-config e)]
                  {(:dimension cfg) (respawn/respawn-chunk-ids e)})
                {dim (search-ids (view w dim eid) req)})))

(defn- target [w vs e req]
  (let [cfg (respawn/respawn-config e)
        bed (when cfg
              (respawn/bed-respawn (:chunks (vs (:dimension cfg))) e))
        dim (if bed (:dimension cfg) (player/respawn-dimension w))
        v (vs dim)
        [yaw pitch] (player/spawn-turn v)]
    [dim (conj (or bed [(found v req) yaw pitch])
               (and (nil? bed) (some? cfg)))]))

(defn- handed [home dim ds]
  (cond (empty? ds) []
        (= home dim) ds
        :else [[:level-deltas dim ds]]))

(defn- respawned [w vs eid e req]
  (let [home (:dim w :overworld)
        [dim [pos yaw pitch :as at]] (target w vs e req)
        away? (not= home dim)
        ds (cond->> (respawn/respawn-deltas (vs dim) eid at)
             away? (remove #(identical? :teleport (nth % 0))))
        ds (conj (vec ds) [:spawn-progress eid nil])]
    (cond-> (handed home dim ds)
      away? (conj [:change-dimension eid dim pos yaw pitch]))))

(defn- loaded-in [world eid need]
  (reduce-kv (fn [m dim ids]
               (assoc m dim (loading [(view world dim eid) []] ids)))
             {} need))

(defn- ready? [vs need]
  (every? (fn [[dim ids]] (loaded? (vs dim) ids)) need))

(defn- revived [[world acc] [eid req]]
  (let [home (:dim world :overworld)
        e (get-in world [:entities eid])
        need (respawn-need world eid e req)
        loaded (loaded-in world eid need)
        vs (update-vals loaded first)]
    [(first (loaded home [world]))
     (-> acc
         (into (mapcat #(handed home (key %) (second (val %)))) loaded)
         (into (if (ready? vs need)
                 (respawned world vs eid e req)
                 (waiting eid req need))))]))

(defn- settle [acc [_ req :as x]]
  (if (:respawn? req) (revived acc x) (joined acc x)))

(defn- here? [world [_ req]]
  (= (:dim world :overworld) (:dim req :overworld)))

(defn arrival
  "Returns the deltas that place player eid of request req in level
  world when its chunks are loaded, or that load them. A request of
  another level gives nil."
  [world eid req]
  (when (here? world [eid req])
    (second (settle [world []] [eid req]))))

(defn placing
  "Places the players of this level whose spawn chunks are loaded.
  They are the joining and respawning ones. The chunks the others wait
  for are loaded."
  {:wake {:keys [:spawning]}}
  [world _]
  (let [reqs (filter #(here? world %)
                     (sort-by key (:spawning world)))]
    (deltas/of-vec (second (reduce settle [world []] reqs)))))
