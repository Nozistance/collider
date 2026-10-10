(ns collider.game.systems.chunks
  "Chunk loading, streaming to players and unloading."
  (:require [collider.data.long-map :as lm]
            [collider.game.entity :as entity]
            [collider.game.deltas :as deltas]
            [collider.game.out :as out]
            [collider.game.schema :as schema]
            [collider.game.areas :as areas]
            [collider.game.level :as level]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen])
  (:import (collider.world ChunkIndex)))

(set! *warn-on-reflection* true)

(def ^:private ^:const start-rate 9.0)

(defn player-radius
  "Returns the view distance a player is sent chunks for.
  It is what the client asked for, held between two and the
  server value."
  ^long [world p]
  (-> (long (or (:view-distance p) 2))
      (max 2)
      (min (areas/view-radius world))))

(defn- generated [world]
  (gen/flat-chunk (:dim world)))

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
            [[:add-chunk id (generated world)]])]
    d))

(defn- restore-deltas [world d]
  (for [[tag _ id payload] (:input d)
        :when (= :chunk-loaded tag)]
    [:restore-chunk id (or payload {:chunk (generated world)})]))

(defn- writable? [world eid]
  (if-let [w (:writable world)] (contains? w eid) true))

(defn- dx ^long [^long cp ^long id]
  (- (ChunkIndex/x id) (ChunkIndex/x cp)))

(defn- dz ^long [^long cp ^long id]
  (- (ChunkIndex/z id) (ChunkIndex/z cp)))

(defn- sees? [^long cp ^long r ^long id]
  (chunk/tracked? r (dx cp id) (dz cp id)))

(defn- dist-sq ^long [^long cp ^long id]
  (let [dx (dx cp id) dz (dz cp id)]
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

(defn- dropped [cp r sent]
  (sort (remove #(sees? cp r %) sent)))

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
     :drop    (dropped cp r sent-chunks)
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
      (when (or moved? (not= r (:chunk-view p)) (seq add) (seq drop)
                (not= pending (:chunks-pending? p)))
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

(defn- view-deltas [world [eid p]]
  (let [cp (chunk/pos-chunk (:pos p))
        r (player-radius world p)
        moved? (not= cp (:chunk-pos p))]
    (when (or moved? (not= r (:chunk-view p)))
      [[:merge-entity eid
        {:chunk-pos cp :chunk-view r :chunks-pending? true}]
       [:chunks-sent eid [] (dropped cp r (:sent-chunks p))
        (when moved? cp)]])))

(defn- streaming? [world [_ p]]
  (and (not (entity/removed? p))
       (streams? p (chunk/pos-chunk (:pos p)) (player-radius world p))))

(defn- loaded-event? [ev] (= :chunk-loaded (nth ev 0)))

(defn- loads? [world]
  (or (seq (areas/absent-chunks world))
      (not-every? #(contains? (:chunks world) %)
                  (mapcat :need (vals (:spawning world))))))

(defn chunk-loading
  "Brings in the chunks the players need and takes in the saved ones."
  {:wake {:types #{:player} :keys [:spawning]
          :events #{:chunk-loaded}}}
  [world d]
  (deltas/of-vec
    (concat (when (some loaded-event? (:input d))
              (restore-deltas world d))
            (when (loads? world)
              (loading-deltas world (areas/needed-ids world))))))

(defn chunk-views
  "Moves the views of the players to their chunks.
  The chunks out of view are forgotten, the new ones wait."
  {:wake {:types #{:player}}}
  [world _d]
  (deltas/of-vec
    (into [] (comp (remove #(entity/removed? (val %)))
                   (mapcat #(view-deltas world %)))
          (level/player-entries world))))

(defn chunk-streaming
  "Sends chunks to the players as the tick ends."
  {:wake {:types #{:player}}}
  [world _d]
  (deltas/fold #(stream-deltas world %)
               (into [] (filter #(streaming? world %))
                     (level/player-entries world))))

(defn- stored-deltas [world groups id]
  (let [p (schema/chunk-payload world id (get groups id))]
    [[:unload-chunk id] (out/all (out/store-chunk id p))]))

(defn- unload-deltas [world ids]
  (let [es (:entities world)
        groups (when (seq ids) (schema/chunk-entities es))]
    (mapcat #(stored-deltas world groups %) ids)))

(defn- purged [tickets]
  (reduce-kv (fn [m id n]
               (if (pos? (long n)) (assoc m id (dec (long n))) m))
             (lm/long-map) tickets))

(defn- dropped-ids [world held]
  (let [keep? (areas/needed-ids world)]
    (into [] (remove #(or (contains? keep? %) (contains? held %)))
          (keys (:chunks world)))))

(defn unloading
  "Drops the chunks the world no longer needs.
  It ages the tickets of chunks read mid-tick. The chunks are stored
  as the last tick left them."
  {:wake {:keys [:unknown [:config :unload-chunks?]]}}
  [world _]
  (let [old (or (:unknown world) (lm/long-map))
        held (purged old)]
    (deltas/of-vec
      (into (if (= held old) [] [[:purge-tickets held]])
            (when (get-in world [:config :unload-chunks?])
              (unload-deltas world (dropped-ids world held)))))))
