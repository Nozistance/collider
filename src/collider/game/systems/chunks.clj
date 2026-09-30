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
  "Returns the view distance a player is sent chunks for.
  It is what the client asked for, held between two and the
  server value."
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
  The chunks come from ids. A saved chunk is asked for and arrives in
  a later tick. Any other chunk is generated now."
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
  A saved chunk is read from the store at once. Any other chunk
  is generated."
  [world id]
  (or (when (contains? (:stored world) id)
        (when-let [read (:read-chunk world)]
          (read (:dim world) id)))
      {:chunk (gen/flat-chunk (:dim world))}))

(defn read-absent-deltas
  "Returns the deltas that put chunks read while absent in place.
  Their ticket keeps them one more tick."
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
  "Returns the ids of the chunks the world keeps loaded.
  They are the chunks around its players and the chunks joining and
  respawning players wait for."
  [world]
  (into (state/loaded-zone world)
        (mapcat :need (vals (:spawning world)))))

(defn- writable? [world eid]
  (if-let [w (:writable world)] (contains? w eid) true))

(defn- id-x ^long [^long id] (unchecked-int (bit-shift-right id 32)))

(defn- id-z ^long [^long id] (unchecked-int id))

(defn- sees? [^long cp ^long r ^long id]
  (chunk/tracked? r (- (id-x id) (id-x cp)) (- (id-z id) (id-z cp))))

(defn- dist-sq ^long [^long cp ^long id]
  (let [dx (- (id-x id) (id-x cp)) dz (- (id-z id) (id-z cp))]
    (+ (* dx dx) (* dz dz))))

(defn- nearest-first [ids ^long cp]
  (sort (fn [^long a ^long b]
          (let [c (Long/compare (dist-sq cp a) (dist-sq cp b))]
            (if (zero? c) (Long/compare a b) c)))
        ids))

(defn- blocked? [world eid ^long unacked batches-max]
  (or (>= unacked (long (or batches-max 1)))
      (not (writable? world eid))))

(defn- stream-quota ^double [chunk-rate chunk-quota blocked]
  (let [rate (double (or chunk-rate start-rate))
        quota (+ (double (or chunk-quota 0.0)) rate)]
    (if blocked 0.0 (min quota (max 1.0 rate)))))

(defn- stream-plan [world eid cp r p]
  (let [{:keys [sent-chunks chunk-rate chunk-quota batches-max]} p
        [cx cz] (chunk/id->pos cp)
        missing (into [] (remove #(contains? sent-chunks %))
                      (chunk/tracked-ids cx cz r))
        ready (filterv #(contains? (:chunks world) %) missing)
        unacked (long (or (:batches-unacked p) 0))
        blocked (blocked? world eid unacked batches-max)
        quota (stream-quota chunk-rate chunk-quota blocked)
        n (min (long (Math/floor quota)) (count ready))]
    {:add     (into [] (take n) (nearest-first ready cp))
     :drop    (sort (remove #(sees? cp r %) sent-chunks))
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

(defn- stalled? [world eid cp r p]
  (and (= cp (:chunk-pos p)) (= r (:chunk-view p))
       (blocked? world eid (long (or (:batches-unacked p) 0))
                 (:batches-max p))
       (every? #(sees? cp r %) (:sent-chunks p))))

(defn- streams? [p cp r]
  (or (not= cp (:chunk-pos p)) (not= r (:chunk-view p))
      (:chunks-pending? p)))

(defn- stream-deltas [world [eid p]]
  (let [cp (chunk/pos-chunk (:pos p))
        r (player-radius world p)]
    (when (and (streams? p cp r)
               (not (stalled? world eid cp r p)))
      (restream-deltas world eid cp r p))))

(defn- stream-thunk [world [_ p :as entry]]
  (when (streams? p (chunk/pos-chunk (:pos p))
                  (player-radius world p))
    #(stream-deltas world entry)))

(defn- loaded-event? [ev] (= :chunk-loaded (nth ev 0)))

(defn- loads? [world]
  (or (seq (state/absent-chunks world))
      (not-every? #(contains? (:chunks world) %)
                  (mapcat :need (vals (:spawning world))))))

(defn chunk-loading
  "Brings in the chunks the players need."
  {:wake {:types #{:player} :keys [:spawning]}}
  [world _d]
  [#(when (loads? world) (loading-deltas world (needed-ids world)))])

(defn chunk-streaming
  "Sends chunks to the players and takes in the saved ones.
  A body returning with its chunk waits for the next tick."
  {:wake {:types #{:player} :events #{:chunk-loaded}}}
  [world d]
  (cond-> (into [] (keep #(stream-thunk world %))
                (state/player-entries world))
    (some loaded-event? (:input d)) (conj #(restore-deltas world d))))

(defn- arrived [world d]
  (keep #(when-let [e (get-in world [:entities (nth % 1)])]
           [(nth % 1) e])
        (state/changes-of d)))

(defn arrival-streaming
  "Sends their first chunks to the players who entered the level.
  They get them as the tick ends for them."
  {:wake {:deltas #{:change-dimension}}}
  [world d]
  (mapv (fn [entry] #(stream-deltas world entry))
        (arrived world d)))

(defn- unload-deltas [world id]
  [[:unload-chunk id]
   (out/all (out/store-chunk id (schema/chunk-payload world id)))])

(defn- purged [tickets]
  (reduce-kv (fn [m id n]
               (if (pos? (long n)) (assoc m id (dec (long n))) m))
             (i/int-map) tickets))

(defn- dropped-ids [world held]
  (let [keep? (needed-ids world)]
    (into [] (remove #(or (contains? keep? %) (contains? held %)))
          (keys (:chunks world)))))

(defn unloading
  "Drops the chunks the world no longer needs.
  It ages the tickets of chunks read mid-tick. The chunks are stored
  as the last tick left them."
  {:wake {:keys [:unknown [:config :unload-chunks?]]}}
  [world _]
  (let [old (or (:unknown world) (i/int-map))
        held (purged old)]
    (into (if (= held old) [] [[:purge-tickets held]])
          (when (get-in world [:config :unload-chunks?])
            (mapcat #(unload-deltas world %)
                    (dropped-ids world held))))))
