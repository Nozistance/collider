(ns collider.game.systems.chunks
  "Chunk loading, streaming to players and unloading."
  (:require [clojure.data.int-map :as i]
            [collider.game.out :as out]
            [collider.game.schema :as schema]
            [collider.game.state :as state]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def ^:const start-rate 9.0)

(defn view-distance
  "Returns the view distance of the server in chunks."
  ^long [world]
  (state/view-radius world))

(defn player-radius
  "Returns the view distance a player is sent chunks for: what the
  client asked for, held between two and the server value."
  ^long [world p]
  (-> (long (or (:view-distance p) 2))
      (max 2)
      (min (view-distance world))))

(defn wanted-chunks
  "Returns the ids of the chunks a player in chunk cp sees."
  [world cp ^long r]
  (let [[cx cz] (chunk/id->pos cp)]
    (into #{} (chunk/tracked-ids (long cx) (long cz) r))))

(defn loading-deltas
  "Returns the deltas that bring the absent chunks into the world.
  The chunks come from ids. A saved chunk is asked for and
  arrives in a later tick. Any other chunk is generated now."
  [world ids]
  (for [id (set ids)
        :when (not (contains? (:chunks world) id))
        :when (not (contains? (:loading world) id))
        d (if (contains? (:stored world) id)
            [[:chunk-requested id] (out/all (out/load-chunk id))]
            [[:add-chunk id (gen/flat-chunk (:dim world))]])]
    d))

(def ^:private ^:const unknown-timeout 1)

(defn read-absent
  "Returns the payload of a chunk read while it is absent.
  A saved chunk is read from the store there and then; any
  other chunk is generated."
  [world id]
  (or (when (contains? (:stored world) id)
        (when-let [read (:read-chunk world)]
          (read (:dim world) id)))
      {:chunk (gen/flat-chunk (:dim world))}))

(defn read-absent-deltas
  "Returns the deltas that put the chunks read this way in place.
  Their ticket keeps them one further tick and no longer."
  [payloads]
  (mapcat (fn [[id payload]]
            [[:restore-chunk id payload]
             [:chunk-ticket id unknown-timeout]])
          payloads))

(defn- restore-deltas [world d]
  (let [dim (:dim world)]
    (for [[tag _ id payload] (:input d)
          :when (= :chunk-loaded tag)]
      [:restore-chunk id
       (or payload {:chunk (gen/flat-chunk dim)})])))

(defn needed-ids
  "Returns the ids of the chunks the world keeps loaded. They are the
  chunks around its players and the chunks joining and respawning
  players wait for."
  [world]
  (into (state/loaded-zone world)
        (mapcat :need (vals (:spawning world)))))

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

(defn- stream-plan [world eid cp r p]
  (let [{:keys [sent-chunks chunk-rate chunk-quota batches-max]} p
        want (wanted-chunks world cp r)
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

(defn- spent-quota [quota ^long n ^long unacked]
  {:chunk-quota (- (double quota) n)
   :batches-unacked (inc unacked)})

(defn- quota-deltas [eid plan]
  (let [{:keys [add n quota unacked blocked pending]} plan]
    (cond
      (seq add)
      [[:merge-entity eid (spent-quota quota n unacked)]]
      (and (not blocked) pending)
      [[:merge-entity eid {:chunk-quota quota}]])))

(defn- restream-deltas [world eid cp r p]
  (let [plan (stream-plan world eid cp r p)
        {:keys [add drop pending]} plan
        moved? (not= cp (:chunk-pos p))]
    (concat
      (when (or moved? (not= r (:chunk-view p)) (seq add) (seq drop))
        [[:merge-entity eid
          {:chunk-pos cp :chunk-view r :chunks-pending? pending}]
         [:chunks-sent eid add drop (when moved? cp)]])
      (quota-deltas eid plan))))

(defn- spawn-look-deltas [eid pos yaw pitch]
  (let [[sx sy sz] (or pos state/spawn-pos)
        look (out/teleport [sx sy sz] (or yaw 0.0) (or pitch 0.0))]
    [(out/to eid look)
     [:merge-entity eid {:needs-spawn? nil}]]))

(defn- stream-deltas [world [eid p]]
  (let [{:keys [pos yaw pitch sent-chunks needs-spawn?]} p
        cp (chunk/pos-chunk pos)
        r (player-radius world p)]
    (concat
      (when (or (not= cp (:chunk-pos p)) (not= r (:chunk-view p))
                (:chunks-pending? p))
        (restream-deltas world eid cp r p))
      (when (and needs-spawn? (own-column? world eid sent-chunks cp))
        (spawn-look-deltas eid pos yaw pitch)))))

(defn chunk-loading
  "Brings in the chunks the players need."
  [world _d]
  [#(loading-deltas world (needed-ids world))])

(defn chunk-streaming
  "Sends chunks to the players and takes in the saved ones that came
  back. A body returning with its chunk waits for the next tick."
  [world d]
  (conj (mapv (fn [entry] #(stream-deltas world entry))
              (state/player-entries world))
        #(restore-deltas world d)))

(defn- unload-deltas [world id]
  [[:unload-chunk id]
   (out/all (out/store-chunk id (schema/chunk-payload world id)))])

(defn- purged
  "Returns the tickets left after one tick of their life.
  A ticket goes when its count would fall below zero."
  [tickets]
  (reduce-kv (fn [m id n]
               (if (pos? (long n)) (assoc m id (dec (long n))) m))
             (i/int-map) tickets))

(defn- dropped-ids [world held]
  (let [keep? (needed-ids world)]
    (into [] (remove #(or (contains? keep? %) (contains? held %)))
          (keys (:chunks world)))))

(defn unloading
  "Drops the chunks the world no longer needs and ages the tickets
  of the chunks a mid-tick read brought in.
  The chunks are stored as the last tick left them."
  [world _]
  (let [old (or (:unknown world) (i/int-map))
        held (purged old)]
    (into (if (= held old) [] [[:purge-tickets held]])
          (when (get-in world [:config :unload-chunks?])
            (mapcat #(unload-deltas world %)
                    (dropped-ids world held))))))
