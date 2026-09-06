(ns collider.game.systems.chunks
  (:require [collider.game.state :as state]
            [collider.game.out :as out]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def view-radius 7)
(def ^:const start-rate 9.0)
(def ^:const min-rate 0.01)
(def ^:const max-rate 64.0)
(def ^:const max-unacked 10)

(defn chunk-coord ^long [^double c]
  (bit-shift-right (long (Math/floor c)) 4))

(defn wanted-chunks [world cp]
  (let [r (long (get-in world [:config :view-distance] view-radius))
        [cx cz] (chunk/id->pos cp)]
    (into #{} (chunk/around-ids (long cx) (long cz) r))))

(defn- own-column? [world eid sent-chunks cp]
  (or (contains? (or sent-chunks #{}) cp)
      (nil? (:writable world))
      (contains? (:writable world) eid)))

(defn- writable? [world eid]
  (if-let [w (:writable world)] (contains? w eid) true))

(defn- nearest-first [ids cp]
  (let [[pcx pcz] (chunk/id->pos cp)]
    (sort-by (fn [id]
               (let [[cx cz] (chunk/id->pos id)
                     dx (- (long cx) (long pcx))
                     dz (- (long cz) (long pcz))]
                 [(+ (* dx dx) (* dz dz)) id]))
             ids)))

(defn- restream-deltas
  "Chunks for the player this tick, paced as vanilla PlayerChunkSender: a quota
   of :chunk-rate chunks per tick, at most :batches-max batches the client has
   not acknowledged with chunk-batch-received."
  [world eid cp {:keys [chunk-pos sent-chunks chunk-rate chunk-quota batches-unacked batches-max]}]
  (let [want    (wanted-chunks world cp)
        add-all (vec (remove #(contains? sent-chunks %) want))
        drop    (sort (remove #(contains? want %) sent-chunks))
        rate    (double (or chunk-rate start-rate))
        unacked (long (or batches-unacked 0))
        blocked (or (>= unacked (long (or batches-max 1))) (not (writable? world eid)))
        quota   (if blocked 0.0 (min (+ (double (or chunk-quota 0.0)) rate) (max 1.0 rate)))
        n       (min (long (Math/floor quota)) (count add-all))
        add     (into [] (take n) (nearest-first add-all cp))
        pending (when (> (count add-all) n) true)]
    (concat
     (when (or (not= cp chunk-pos) (seq add) (seq drop))
       [[:merge-entity eid {:chunk-pos cp :chunks-pending? pending}]
        [:chunks-sent eid add drop]])
     (cond
       (seq add) [[:merge-entity eid {:chunk-quota (- quota n) :batches-unacked (inc unacked)}]]
       (and (not blocked) pending) [[:merge-entity eid {:chunk-quota quota}]]))))

(defn- spawn-look-deltas [_ eid pos yaw pitch]
  (let [[sx sy sz] (or pos state/spawn-pos)]
    [(out/to eid (out/teleport [sx sy sz] (or yaw 0.0) (or pitch 0.0)))
     [:merge-entity eid {:needs-spawn? nil}]]))

(defn- stream-deltas [world [eid {:keys [pos yaw pitch chunk-pos sent-chunks needs-spawn? chunks-pending?] :as p}]]
  (let [[x _ z] pos
        cp (chunk/pos->id (chunk-coord x) (chunk-coord z))]
    (concat
     (when (or (not= cp chunk-pos) chunks-pending?)
       (restream-deltas world eid cp p))
     (when (and needs-spawn? (own-column? world eid sent-chunks cp))
       (spawn-look-deltas world eid pos yaw pitch)))))

(defn chunk-streaming [world _events]
  (mapv (fn [entry] #(stream-deltas world entry))
        (state/player-entries world)))
