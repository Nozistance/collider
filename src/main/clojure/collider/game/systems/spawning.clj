(ns collider.game.systems.spawning
  "Placement of joining and respawning players."
  (:require [collider.game.deltas :as deltas]
            [collider.game.apply :as apply]
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

(defn- need-ids [w eid req]
  (if (:respawn? req)
    (concat (respawn/respawn-chunk-ids (get-in w [:entities eid]))
            (search-ids w req))
    (join-ids w req)))

(defn- respawned [w eid req]
  (let [e (get-in w [:entities eid])
        bed (respawn/bed-respawn (:chunks w) e)
        [yaw pitch] (player/spawn-turn w)
        [pos yaw pitch] (or bed [(found w req) yaw pitch])
        lost? (and (nil? bed) (some? (respawn/respawn-config e)))]
    (conj (respawn/respawn-deltas w eid [pos yaw pitch lost?])
          [:spawn-progress eid nil])))

(defn- placed [w eid req]
  (if (:respawn? req)
    (respawned w eid req)
    [[:player-placed eid (:name req) (or (:pos req) (found w req))]]))

(defn- loading [[w acc] need]
  (let [ds (vec (chunks/loading-deltas w need))
        w' (if (seq ds) (first (apply/deltas w ds)) w)]
    [w' (into acc ds)]))

(defn- settle [[world acc] [eid req]]
  (let [[w acc] (loading [world acc] (need-ids world eid req))
        [w acc] (loading [w acc] (need-ids w eid req))
        need (vec (need-ids w eid req))]
    [w (into acc (if (loaded? w need)
                   (placed w eid req)
                   [[:spawn-progress eid (assoc req :need need)]]))]))

(defn- here? [world [_ req]]
  (= (:dim world :overworld) (:dim req :overworld)))

(defn placing
  "Places the players of this level whose spawn chunks are loaded.
  They are the joining and respawning ones. The chunks the others wait
  for are loaded."
  {:wake {:keys [:spawning]}}
  [world _]
  (let [reqs (filter #(here? world %)
                     (sort-by key (:spawning world)))]
    (deltas/of-vec (second (reduce settle [world []] reqs)))))
