(ns collider.game.tick
  "The tick and its ticker."
  (:require [clojure.data.int-map :as i]
            [collider.game.state :as state]
            [collider.game.schema :as schema]
            [collider.game.deltas :as deltas]
            [collider.game.detector :as detector]
            [collider.log :as log]
            [collider.game.systems.block.updates :as block-updates]
            [collider.game.systems.blocks :as blocks]
            [collider.game.systems.brewing :as brewing]
            [collider.game.systems.campfires :as campfires]
            [collider.game.systems.chat :as chat]
            [collider.game.systems.chunks :as chunks]
            [collider.game.systems.consume :as consume]
            [collider.game.systems.containers :as containers]
            [collider.game.systems.daynight :as daynight]
            [collider.game.systems.dripleaf :as dripleaf]
            [collider.game.systems.explosions :as explosions]
            [collider.game.systems.falling :as falling]
            [collider.game.systems.furnaces :as furnaces]
            [collider.game.systems.inventory :as inventory]
            [collider.game.systems.items :as items]
            [collider.game.systems.jukebox :as jukebox]
            [collider.game.systems.keepalive :as keepalive]
            [collider.game.systems.mobs :as mobs]
            [collider.game.systems.packets :as packets]
            [collider.game.systems.players :as players]
            [collider.game.systems.pose :as pose]
            [collider.game.systems.projectiles :as projectiles]
            [collider.game.systems.random.tick :as random-tick]
            [collider.game.systems.signs :as signs]
            [collider.game.systems.sleep :as sleep]
            [collider.game.systems.spawning :as spawning]
            [collider.game.systems.tnt :as tnt]
            [collider.game.systems.weather :as weather-system]
            [collider.game.systems.damage :as damage])
  (:import (collider.game.deltas Deltas)
           (java.util Arrays)
           (java.util.concurrent ConcurrentLinkedQueue)
           (java.util.concurrent.atomic AtomicBoolean AtomicLong)))

(set! *warn-on-reflection* true)

(def packet-systems
  "The systems a player event drives. Vanilla runs them all
  before the level tick."
  [#'chunks/chunk-streaming
   #'players/player-list
   #'players/players
   #'blocks/block-edits
   #'packets/by-player
   #'sleep/sleep
   #'inventory/inventory
   #'containers/containers
   #'chat/chat
   #'keepalive/keepalive])

(def entity-systems
  "The systems that step the entities of the level."
  [#'items/items
   #'falling/falling-blocks
   #'mobs/mobs-system
   #'tnt/tnt-system
   #'projectiles/projectiles
   #'damage/damage])

(def systems
  "Every system the level tick drives off the world it is given."
  (into packet-systems entity-systems))

(def phases [[#'spawning/placing]
             [#'chunks/chunk-loading]
             packet-systems
             [#'chunks/arrival-streaming]
             [#'consume/consume]
             [#'pose/pose]
             [#'daynight/daynight]
             [#'block-updates/block-updates
              #'dripleaf/dripleaf-tilt]
             [#'block-updates/fluid-updates]
             [#'chunks/unloading
              #'random-tick/random-ticks
              #'weather-system/weather]
             [#'block-updates/block-flush
              #'players/late-tracking]
             entity-systems
             [#'explosions/explosions]
             [#'items/pickups #'containers/broadcast]
             [#'furnaces/furnace-cooking
              #'campfires/campfire-cooking
              #'brewing/brewing
              #'jukebox/jukebox-songs
              #'signs/sign-editors]
             [#'blocks/acks]
             [#'detector/observe]])

(def server-systems
  "The systems of phases that run once for the whole server, not
  in each level. They see the players of every level."
  #{#'players/player-list #'daynight/daynight})

(def dims
  "The dimensions the tick runs, in a fixed order."
  schema/dims)

(def ^:private home (first dims))

(defn- event-dim [world [tag x]]
  (case tag
    :player-join home
    :chunk-loaded x
    (or (state/dim-of world x) home)))

(defn- input-of [dim events]
  (if (= home dim)
    (deltas/input events)
    (assoc deltas/empty-deltas :input (vec events))))

(defn- begin [world events]
  (let [by (group-by #(event-dim world %) events)
        ds (into {} (map (fn [dim] [dim (input-of dim (by dim))]))
                 dims)]
    [(reduce (fn [w dim] (state/enter w dim (ds dim))) world dims)
     ds]))

(defn- awaited? [world dim]
  (some #(= dim (:dim (val %))) (:spawning world)))

(defn- asleep? [world dim]
  (and (not= home dim)
       (state/idle? (get-in world [:levels dim]))
       (not (awaited? world dim))))

(defn- server-job [s [view d]]
  #(deltas/with-dim (deltas/run [(fn [] (s view d))]) nil))

(defn- job [lv d server dim s]
  (if (server-systems s)
    (when (= home dim) (server-job s @server))
    #(s lv d)))

(defn- level-deltas [world ds server phase dim]
  (if (asleep? world dim)
    deltas/empty-deltas
    (let [lv (assoc (state/level world dim) :server world)
          d (get ds dim)]
      (-> (deltas/run (into [] (keep #(job lv d server dim %)) phase))
          (deltas/with-dim dim)))))

(defn- merged [ds] (reduce deltas/merge (map ds dims)))

(defn- away [world eid]
  (let [dim (state/dim-of world eid)]
    (when (and dim (not= home dim)) dim)))

(defn- stray-dim [world delta]
  (when (identical? :remove-entity (nth delta 0))
    (away world (nth delta 1))))

(defn- part [ws es]
  (deltas/->Deltas (vec ws) (into (i/int-map) es) [] []))

(defn- rehomed [world ^Deltas d]
  (let [ws (group-by #(stray-dim world %) (deltas/world-of d))
        es (group-by #(away world (key %)) (deltas/entities-of d))
        dims' (disj (into (set (keys ws)) (keys es)) nil)]
    (into {home (assoc (part (ws nil) (es nil))
                  :out (deltas/out-of d) :input (deltas/input-of d))}
          (map (fn [dim] [dim (part (ws dim) (es dim))]))
          dims')))

(defn- strays? [world ^Deltas d]
  (or (some #(away world %) (keys (deltas/entities-of d)))
      (some #(stray-dim world %) (deltas/world-of d))))

(defn- relocated [world pd]
  (let [d (pd home)]
    (if (strays? world d)
      (merge-with deltas/merge (assoc pd home deltas/empty-deltas)
                  (rehomed world d))
      pd)))

(defn- phase-deltas [world ds phase]
  (let [server (delay [(state/server-view world) (merged ds)])
        of (fn [dim] [dim (level-deltas world ds server phase dim)])
        pd (into {} (map of) dims)]
    (if (some server-systems phase) (relocated world pd) pd)))

(def ^:private left-behind #{:chunks-sent :tracking})

(defn- left-behind?
  "Tells whether entity delta d is chunk work of the level a player
  leaves: sending, tracking, or the batch quota spent on them."
  [d]
  (let [tag (nth d 0)]
    (or (contains? left-behind tag)
        (and (identical? :merge-entity tag)
             (contains? (nth d 2) :chunk-quota)))))

(defn- stay [ds]
  (filterv #(not (left-behind? %)) ds))

(defn- departed
  "Returns d without the chunk and tracking work of the players
  who leave the level in it. That work is for the level left."
  ^Deltas [^Deltas d changes]
  (let [gone (map #(nth % 1) changes)
        es (deltas/entities-of d)
        kept (fn [m eid]
               (if-let [v (get m eid)] (assoc m eid (stay v)) m))
        es' (reduce kept es gone)]
    (assoc d :entities es')))

(defn- arrivals [ds changes]
  (reduce (fn [ds c]
            (update ds (nth c 2) deltas/merge
                    (deltas/->Deltas [c] (i/int-map) [] [])))
          ds changes))

(defn- crossing [[world ds] dim ^Deltas d changes]
  (let [d (departed d changes)]
    [(state/cross (state/apply-in world dim d) dim changes)
     (arrivals (update ds dim deltas/merge d) changes)]))

(declare step)

(defn- handed
  "Returns [world ds] with the deltas d hands to other levels applied
  there, after d itself."
  [acc ^Deltas d]
  (reduce (fn [acc [dim sub]]
            (let [sd (deltas/add deltas/empty-deltas sub)]
              (step acc dim (deltas/with-dim sd dim))))
          acc (state/handoffs-of d)))

(defn- own-step [[world ds] dim d]
  (let [changes (state/changes-of d)]
    (if (seq changes)
      (crossing [world ds] dim d changes)
      [(if (deltas/inert? d) world (state/apply-in world dim d))
       (update ds dim deltas/merge d)])))

(defn- step [acc dim d]
  (if (identical? deltas/empty-deltas d)
    acc
    (handed (own-step acc dim d) d)))

(defn- run-phase [[world ds] phase]
  (let [pd (phase-deltas world ds phase)]
    (reduce (fn [acc dim] (step acc dim (pd dim))) [world ds] dims)))

(defn tick
  "Returns the world and the deltas after one tick of the events."
  [world events]
  (let [[world' ds] (reduce run-phase (begin world events) phases)]
    [world' (merged ds)]))

(def ^:private ^:const nominal-tick-ns 50000000)

(def ^:private ^:const window-size 4096)

(def ^:private ^:const tps-window 100)

(defn- drain! [^ConcurrentLinkedQueue q]
  (loop [acc (transient [])]
    (if-some [e (.poll q)]
      (recur (conj! acc e))
      (persistent! acc))))

(defn- record! [^longs window ^AtomicLong counter ^long elapsed]
  (let [i (int (rem (.getAndIncrement counter) window-size))]
    (aset window i elapsed)))

(defn- empty-ticks ^long [^long n world]
  (if (and (empty? (:players world)) (empty? (:spawning world)))
    (inc n)
    0))

(defn- paused? [opts ^long n]
  (let [s (long (or (:pause-when-empty-seconds opts) 0))]
    (when (and (pos? s) (= n (* 20 s)))
      (log/info "no players for" s "s, world paused"
                "until someone joins")
      (when-let [f (:on-pause opts)] (f)))
    (and (pos? s) (>= n (* 20 s)))))

(defn- idle? [opts n ^ConcurrentLinkedQueue queue]
  (and (paused? opts n) (.isEmpty queue)))

(defn- percentiles [^longs window ^AtomicLong counter]
  (let [n (int (min (.get counter) window-size))]
    (when (pos? n)
      (let [arr (Arrays/copyOf window n)]
        (Arrays/sort arr)
        {:ticks  (.get counter)
         :p50-ms (/ (aget arr (quot n 2)) 1e6)
         :p99-ms (/ (aget arr (min (dec n) (int (* n 0.99)))) 1e6)
         :max-ms (/ (aget arr (dec n)) 1e6)}))))

(defn- tps-of ^double [^longs stamps ^long i ^long now ^long tps]
  (let [n (min (inc i) tps-window)]
    (if (< n 2)
      (double tps)
      (let [past (aget stamps (int (rem (- (inc i) n) tps-window)))]
        (if (<= now past)
          (double tps)
          (min (double tps)
               (/ (* 1.0E9 (dec n)) (- now past))))))))

(defn- pace ^long [^long next-ns ^long step-ns]
  (let [target (+ next-ns step-ns)
        now (System/nanoTime)
        target (if (> (- now target) 1000000000) now target)
        sleep (quot (- target now) 1000000)]
    (when (pos? sleep) (^[long] Thread/sleep sleep))
    target))

(defn- tick-input [world-atom perf io]
  (-> @world-atom
      (assoc :time-ms (System/currentTimeMillis))
      (cond-> perf (assoc :perf perf)
              io (merge io))))

(defn- safe-tick [world events]
  (try (tick world events)
       (catch Throwable t
         (log/error-with "tick error:" t)
         [world deltas/empty-deltas])))

(defn- send-out! [deliver! world ^Deltas deltas]
  (when (or (seq (deltas/out-of deltas))
            (pos? (count (deltas/entities-of deltas))))
    (try (deliver! world deltas)
         (catch Throwable t (log/error-with "deliver error:" t)))))

(defn- run-tick! [world-atom ^ConcurrentLinkedQueue queue deliver!
                  perf io-input]
  (let [events (drain! queue)
        world (tick-input world-atom perf (when io-input (io-input)))
        [world' deltas] (safe-tick world events)]
    (reset! world-atom world')
    (send-out! deliver! world' deltas)))

(defn- ticker-state [_cfg]
  {:window  (long-array window-size)
   :counter (AtomicLong. 0)
   :stamps  (long-array tps-window)})

(defn- perf-of [{:keys [window counter ^longs stamps]} i t0 tps]
  (when (zero? (rem (long i) 20))
    (let [base (or (percentiles window counter) {})]
      (assoc base :tps (tps-of stamps i t0 tps)))))

(defn- one-tick! [{:keys [window counter]} world-atom queue deliver!
                  perf t0 io-input]
  (run-tick! world-atom queue deliver! perf io-input)
  (record! window counter (- (System/nanoTime) t0)))

(defn- timed-tick! [st i perf world-atom queue deliver! opts]
  (let [t0 (System/nanoTime)
        p (or (perf-of st i t0 20) perf)]
    (aset ^longs (:stamps st) (int (rem (long i) tps-window)) t0)
    (one-tick! st world-atom queue deliver! p t0 (:io-input opts))
    p))

(defn- running? [^AtomicBoolean running] (.get running))

(defn- ticker-loop [st running world-atom queue deliver! opts]
  (let [tick! #(timed-tick! st %1 %2 world-atom queue deliver! opts)]
    (loop [next-ns (System/nanoTime) i 0 perf nil empty 0]
      (when (running? running)
        (let [n (empty-ticks empty @world-atom)]
          (if (idle? opts n queue)
            (do (Thread/sleep 50) (recur (System/nanoTime) 0 nil n))
            (let [p (tick! i perf)
                  at (pace next-ns nominal-tick-ns)]
              (recur at (inc i) p n))))))))

(defn- ticker-thread
  ^Thread [st running world-atom queue deliver! opts]
  (let [run #(ticker-loop st running world-atom queue deliver! opts)]
    (doto (Thread. ^Runnable run "collider-ticker")
      (.setDaemon true)
      (.start))))

(defn start-ticker!
  "Starts a daemon thread that ticks world-atom.
  It ticks on the events from queue and gives the deltas of each
  tick to deliver!. Returns a handle for the stop."
  ([world-atom queue deliver!]
   (start-ticker! world-atom queue deliver! nil))
  ([world-atom ^ConcurrentLinkedQueue queue deliver! opts]
   (let [st (ticker-state opts)
         running (AtomicBoolean. true)
         thread (ticker-thread
                  st running world-atom queue deliver! opts)]
     {:thread  thread
      :running running
      :stats   #(percentiles (:window st) (:counter st))})))

(defn stop-ticker!
  "Stops the ticker and waits up to a second for its thread."
  [{:keys [^Thread thread ^AtomicBoolean running]}]
  (.set running false)
  (.join thread 1000)
  nil)
