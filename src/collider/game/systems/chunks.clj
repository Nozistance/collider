(ns collider.game.systems.chunks
  "Chunk loading, streaming to players and unloading."
  (:require [clojure.data.int-map :as i]
            [collider.game.schema :as schema]
            [collider.game.state :as state]
            [collider.game.out :as out]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def view-radius 7)

(def ^:const start-rate 9.0)

(def ^:private ^:const loaded-border 2)

(defn view-distance
  "Returns the view distance of the server in chunks."
  ^long [world]
  (-> (long (get-in world [:config :view-distance] view-radius))
      (max 2)
      (min 32)))

(defn- tracked? [^long v ^long dx ^long dz]
  (let [ax (max 0 (- (Math/abs dx) 2))
        az (max 0 (- (Math/abs dz) 2))]
    (< (+ (* ax ax) (* az az)) (* v v))))

(defn wanted-chunks
  "Returns the ids of the chunks a player in chunk cp sees."
  [world cp]
  (let [v (view-distance world)
        [cx cz] (chunk/id->pos cp)
        span (range (- -1 v) (+ v 2))]
    (into #{}
          (for [dx span
                dz span
                :when (tracked? v dx dz)]
            (chunk/pos->id (+ (long cx) (long dx))
                           (+ (long cz) (long dz)))))))

(defn loading-deltas
  "Returns the deltas that bring the absent chunks among ids into the
  world. A saved chunk is asked for and arrives in a later tick. Any
  other chunk is generated now."
  [world ids]
  (for [id (set ids)
        :when (not (contains? (:chunks world) id))
        :when (not (contains? (:loading world) id))
        d (if (contains? (:stored world) id)
            [[:chunk-requested id] (out/all (out/load-chunk id))]
            [[:add-chunk id (gen/flat-chunk)]])]
    d))

(defn- restore-deltas [d]
  (for [[tag id payload] (:input d) :when (= :chunk-loaded tag)]
    [:restore-chunk id (or payload {:chunk (gen/flat-chunk)})]))

(defn- player-zone [world [_ p]]
  (when-let [pos (:pos p)]
    (let [[cx cz] (chunk/id->pos (chunk/pos-chunk pos))]
      (chunk/around-ids (long cx) (long cz)
                        (+ (view-distance world) loaded-border)))))

(defn needed-ids
  "Returns the ids of the chunks the world keeps loaded. They are the
  chunks around its players and the chunks joining and respawning
  players wait for."
  [world]
  (let [players (state/player-entries world)]
    (into (i/int-set)
          (concat (mapcat #(player-zone world %) players)
                  (mapcat :need (vals (:spawning world)))))))

(defn- writable? [world eid]
  (if-let [w (:writable world)] (contains? w eid) true))

(defn- own-column? [world eid sent-chunks cp]
  (or (contains? (or sent-chunks #{}) cp)
      (writable? world eid)))

(defn- nearest-first [ids cp]
  (let [[pcx pcz] (chunk/id->pos cp)]
    (sort-by (fn [id]
               (let [[cx cz] (chunk/id->pos id)
                     dx (- (long cx) (long pcx))
                     dz (- (long cz) (long pcz))]
                 [(+ (* dx dx) (* dz dz)) id]))
             ids)))

(defn- blocked? [world eid ^long unacked batches-max]
  (or (>= unacked (long (or batches-max 1)))
      (not (writable? world eid))))

(defn- stream-quota ^double [chunk-rate chunk-quota blocked]
  (let [rate (double (or chunk-rate start-rate))
        quota (+ (double (or chunk-quota 0.0)) rate)]
    (if blocked 0.0 (min quota (max 1.0 rate)))))

(defn- stream-plan [world eid cp p]
  (let [{:keys [sent-chunks chunk-rate chunk-quota batches-max]} p
        want (wanted-chunks world cp)
        missing (vec (remove #(contains? sent-chunks %) want))
        ready (filterv #(contains? (:chunks world) %) missing)
        unacked (long (or (:batches-unacked p) 0))
        blocked (blocked? world eid unacked batches-max)
        quota (stream-quota chunk-rate chunk-quota blocked)
        n (min (long (Math/floor quota)) (count ready))]
    {:add     (into [] (take n) (nearest-first ready cp))
     :drop    (sort (remove #(contains? want %) sent-chunks))
     :n       n :quota quota :unacked unacked :blocked blocked
     :pending (when (> (count missing) n) true)}))

(defn- quota-deltas [eid plan]
  (let [{:keys [add n quota unacked blocked pending]} plan]
    (cond
      (seq add)
      [[:merge-entity eid {:chunk-quota     (- (double quota) (long n))
                           :batches-unacked (inc (long unacked))}]]
      (and (not blocked) pending)
      [[:merge-entity eid {:chunk-quota quota}]])))

(defn- restream-deltas [world eid cp p]
  (let [plan (stream-plan world eid cp p)
        {:keys [add drop pending]} plan]
    (concat
      (when (or (not= cp (:chunk-pos p)) (seq add) (seq drop))
        [[:merge-entity eid {:chunk-pos cp :chunks-pending? pending}]
         [:chunks-sent eid add drop]])
      (quota-deltas eid plan))))

(defn- spawn-look-deltas [eid pos yaw pitch]
  (let [[sx sy sz] (or pos state/spawn-pos)
        look (out/teleport [sx sy sz] (or yaw 0.0) (or pitch 0.0))]
    [(out/to eid look)
     [:merge-entity eid {:needs-spawn? nil}]]))

(defn- stream-deltas [world [eid p]]
  (let [{:keys [pos yaw pitch sent-chunks needs-spawn?]} p
        cp (chunk/pos-chunk pos)]
    (concat
      (when (or (not= cp (:chunk-pos p)) (:chunks-pending? p))
        (restream-deltas world eid cp p))
      (when (and needs-spawn? (own-column? world eid sent-chunks cp))
        (spawn-look-deltas eid pos yaw pitch)))))

(defn chunk-streaming [world d]
  (conj (mapv (fn [entry] #(stream-deltas world entry))
              (state/player-entries world))
        #(concat (restore-deltas d)
                 (loading-deltas world (needed-ids world)))))

(defn- unload-deltas [world id]
  [[:unload-chunk id]
   (out/all (out/store-chunk id (schema/chunk-payload world id)))])

(defn unloading
  "Unloads the chunks the world no longer needs and stores them as the
  tick left them."
  [world _]
  (when (get-in world [:config :unload-chunks?])
    (let [keep? (needed-ids world)]
      (into []
            (comp (remove #(contains? keep? %))
                  (mapcat #(unload-deltas world %)))
            (keys (:chunks world))))))
