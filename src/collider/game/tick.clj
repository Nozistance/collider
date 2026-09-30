(ns collider.game.tick
  "The tick and its ticker."
  (:require [clojure.data.int-map :as i]
            [collider.game.state :as state]
            [collider.game.schema :as schema]
            [collider.game.cost :as cost]
            [collider.game.deltas :as deltas]
            [collider.game.deltas.record :as types]
            [collider.game.detector :as detector]
            [collider.log :as log]
            [collider.game.systems.block.updates :as block-updates]
            [collider.game.systems.blocks :as blocks]
            [collider.game.systems.brewing :as brewing]
            [collider.game.systems.camera :as camera]
            [collider.game.systems.campfires :as campfires]
            [collider.game.systems.chat :as chat]
            [collider.game.systems.chunks :as chunks]
            [collider.game.systems.consume :as consume]
            [collider.game.systems.containers :as containers]
            [collider.game.systems.daynight :as daynight]
            [collider.game.systems.dripleaf :as dripleaf]
            [collider.game.systems.effects :as effects]
            [collider.game.systems.experience :as experience]
            [collider.game.systems.explosions :as explosions]
            [collider.game.systems.falling :as falling]
            [collider.game.systems.furnaces :as furnaces]
            [collider.game.systems.geysers :as geysers]
            [collider.game.systems.inventory :as inventory]
            [collider.game.systems.items :as items]
            [collider.game.systems.jukebox :as jukebox]
            [collider.game.systems.keepalive :as keepalive]
            [collider.game.systems.mobs :as mobs]
            [collider.game.systems.orbs :as orbs]
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
  (:import (collider.game.deltas.record Deltas)
           (java.util Arrays)
           (java.util.concurrent ConcurrentLinkedQueue)
           (java.util.concurrent.atomic AtomicBoolean AtomicLong)))

(set! *warn-on-reflection* true)

(def packet-systems
  "The systems a player event drives.
  They all run before the level tick."
  [#'chunks/chunk-streaming
   #'players/player-list
   #'players/players
   #'camera/camera
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
   #'orbs/orbs
   #'falling/falling-blocks
   #'damage/countdown
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
             [#'effects/effects]
             [#'consume/consume]
             [#'pose/pose]
             [#'daynight/daynight]
             [#'block-updates/block-updates
              #'dripleaf/dripleaf-tilt
              #'containers/rechecks]
             [#'block-updates/fluid-updates]
             [#'chunks/unloading
              #'random-tick/random-ticks
              #'weather-system/weather]
             [#'block-updates/block-flush
              #'players/late-tracking]
             entity-systems
             [#'explosions/blasts]
             [#'items/pickups #'orbs/pickups #'containers/broadcast]
             [#'furnaces/furnace-cooking
              #'campfires/campfire-cooking
              #'brewing/brewing
              #'geysers/geysers
              #'jukebox/jukebox-songs
              #'signs/sign-editors]
             [#'blocks/acks #'experience/experience]
             [#'detector/observe]])

(def server-systems
  "The systems that run once for the whole server, not per level.
  They see the players of every level."
  #{#'players/player-list #'daynight/daynight})

(defn- server-phase? [phase]
  (boolean (some server-systems phase)))

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

(defn- entered [ds [w views] dim]
  (let [[w' lv] (state/entered w dim (ds dim))]
    [w' (if lv {dim lv} views)]))

(defn- awaited? [world dim]
  (let [spawning (:spawning world)]
    (and (not (deltas/vacant? spawning))
         (some #(= dim (:dim (val %))) spawning))))

(defn- asleep? [world dim]
  (and (not (identical? home dim))
       (state/idle? (get (:levels world) dim))
       (not (awaited? world dim))))

(defn- awake-views [world views]
  (reduce (fn [vs dim]
            (cond (asleep? world dim) (dissoc vs dim)
                  (contains? vs dim) vs
                  :else (assoc vs dim (state/level world dim))))
          (or views {}) dims))

(defn- begin [world events]
  (let [by (group-by #(event-dim world %) events)
        ds (into {} (map (fn [dim] [dim (input-of dim (by dim))]))
                 dims)
        [w views] (reduce #(entered ds %1 %2) [world nil] dims)
        heeded (fn [[dim d]] [dim (state/heeded w dim d)])]
    [w (into {} (map heeded) ds) (awake-views w views)]))

(defn- skipped! [world s dim ^Throwable t]
  (let [unit (log/name-of s)
        msg (str "system " unit " failed in " (name (or dim :server))
                 ", its deltas this tick are dropped")]
    (log/failure! unit msg t)
    (some-> (:failures world) (swap! conj [unit t]))))

(defn- guarded [world s dim f]
  (fn []
    (try (deltas/run [f])
         (catch Throwable t
           (skipped! world s dim t)
           deltas/empty-deltas))))

(defn- server-thunk [world s [view d]]
  (let [f (guarded world s nil #(s view d))]
    #(deltas/with-dim (f) nil)))

(defn- thunk [world lv d server dim s]
  (if (server-systems s)
    (server-thunk world s @server)
    (guarded world s dim #(s lv d))))

(defn- runs-in? [dim s]
  (or (identical? home dim) (not (server-systems s))))

(defn- held? [v]
  (if (coll? v) (not (empty? v)) (boolean v)))

(defn- tagged? [tags ds]
  (loop [i (dec (count ds))]
    (cond (neg? i) false
          (contains? tags (nth (nth ds i) 0)) true
          :else (recur (dec i)))))

(defn- kept? [lv ks]
  (loop [i (dec (count ks))]
    (cond (neg? i) false
          (let [k (nth ks i)]
            (held? (if (vector? k) (get-in lv k) (get lv k))))
          true
          :else (recur (dec i)))))

(defn- due? [lv n]
  (zero? (rem (long (:tick lv)) (long n))))

(defn- woken? [w lv d]
  (let [types (:types w) events (:events w) ds (:deltas w)
        ks (:keys w) every (:every w)]
    (boolean
      (or (and every (due? lv every))
          (and types (state/holds-types? lv types))
          (and events (tagged? events (deltas/input-of d)))
          (and ds (tagged? ds (deltas/world-of d)))
          (and ks (kept? lv ks))))))

(defn awake?
  "Returns true when system s may have work in level lv on deltas d.
  Its :wake names the events, the deltas, the entity types, the keys
  of the level and the period in ticks that are its work. A system
  with no :wake, or the :wake :always, never sleeps."
  [s lv d]
  (let [w (:wake (meta s))]
    (or (not (map? w)) (woken? w lv d))))

(defn- awake-in [lv d dim phase]
  (filterv #(and (runs-in? dim %) (awake? % lv d)) phase))

(defn- level-deltas [world lv ds server phase dim]
  (let [d (get ds dim)
        ks (awake-in lv d dim phase)]
    (when (pos? (count ks))
      (let [lv (assoc lv :server world)
            fs (mapv #(thunk world lv d server dim %) ks)
            timed (cost/timer (:tick world))]
        (deltas/with-dim
         (deltas/run-weighed cost/heavy? timed ks fs)
         dim)))))

(defn- view [world views dim]
  (or (get views dim) (state/level world dim)))

(defn- merged [ds] (reduce deltas/merge (map ds dims)))

(defn- away [world eid]
  (let [dim (state/dim-of world eid)]
    (when (and dim (not= home dim)) dim)))

(defn- stray-dim [world delta]
  (when (identical? :remove-entity (nth delta 0))
    (away world (nth delta 1))))

(defn- part [ws es]
  (types/->Deltas (vec ws) (into (i/int-map) es) [] []))

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

(defn- phase-deltas
  ([world ds phase]
   (phase-deltas world (awake-views world nil) ds phase))
  ([world views ds phase]
   (let [server (delay [(state/server-view world) (merged ds)])
         of (fn [pd dim]
              (let [lv (get views dim)
                    d (when lv
                        (level-deltas world lv ds server phase dim))]
                (if d (assoc pd dim d) pd)))
         pd (reduce of {} dims)]
     (if (and (contains? pd home) (server-phase? phase))
       (relocated world pd)
       pd))))

(def ^:private left-behind #{:chunks-sent :tracking})

(defn- left-behind? [d]
  (let [tag (nth d 0)]
    (or (contains? left-behind tag)
        (and (identical? :merge-entity tag)
             (contains? (nth d 2) :chunk-quota)))))

(defn- stay [ds]
  (filterv #(not (left-behind? %)) ds))

(defn- departed ^Deltas [^Deltas d changes]
  (let [gone (map #(nth % 1) changes)
        es (deltas/entities-of d)
        kept (fn [m eid]
               (if-let [v (get m eid)] (assoc m eid (stay v)) m))
        es' (reduce kept es gone)]
    (assoc d :entities es')))

(defn- arrivals [ds changes]
  (reduce (fn [ds c]
            (update ds (nth c 2) deltas/merge
                    (types/->Deltas [c] (i/int-map) [] [])))
          ds changes))

(defn- crossing [[world ds] dim ^Deltas d changes]
  (let [d (departed d changes)]
    [(state/cross (state/apply-in world dim d) dim changes)
     (arrivals (update ds dim deltas/merge d) changes)]))

(declare step)

(defn- handed [acc ^Deltas d]
  (reduce (fn [acc [dim sub]]
            (let [sd (deltas/add deltas/empty-deltas sub)]
              (step acc dim (deltas/with-dim sd dim))))
          acc (state/handoffs-of d)))

(defn- own-step [[world ds views] dim d]
  (let [changes (state/changes-of d)
        ds' (update ds dim deltas/merge d)]
    (cond
      (seq changes) (crossing [world ds] dim d changes)
      (deltas/inert? d) [world ds' views]
      :else (let [lv (view world views dim)
                  [w lv'] (state/applied-in world dim lv d)]
              [w ds' {dim lv'}]))))

(defn- step [acc dim d]
  (if (or (nil? d) (identical? deltas/empty-deltas d))
    acc
    (handed (own-step acc dim d) d)))

(defn- fresh [[w ds vs :as acc] world views]
  (if (and (identical? w world) (identical? vs views))
    acc
    [w ds (awake-views w vs)]))

(defn- run-phase [[world ds views :as acc] phase]
  (let [pd (phase-deltas world views ds phase)]
    (if (zero? (count pd))
      acc
      (fresh (reduce (fn [acc dim] (step acc dim (get pd dim)))
                     acc dims)
             world views))))

(defn tick
  "Returns the world and the deltas after one tick of the events.
  A system that fails gives no deltas this tick, the others go on."
  ([world events] (tick world events phases))
  ([world events phases]
   (deltas/in-pool
     #(let [acc (begin world events)
            [world' ds] (reduce run-phase acc phases)]
        [world' (merged ds)]))))

(def ^:private ^:const nominal-tick-ns 50000000)

(def ^:private ^:const window-size 4096)

(def ^:private ^:const tps-window 100)

(def ^:private ^:const mspt-window 100)

(defn- poll! [^ConcurrentLinkedQueue q] (.poll q))

(defn- queue-empty? [^ConcurrentLinkedQueue q] (.isEmpty q))

(defn- count-up! ^long [^AtomicLong c] (.getAndIncrement c))

(defn- count-of ^long [^AtomicLong c] (.get c))

(defn- running? [^AtomicBoolean running] (.get running))

(defn- stop! [^AtomicBoolean running] (.set running false))

(defn- drain! [q]
  (loop [acc (transient [])]
    (if-some [e (poll! q)]
      (recur (conj! acc e))
      (persistent! acc))))

(defn- record! [^longs window counter ^long elapsed]
  (let [i (int (rem (count-up! counter) window-size))]
    (aset window i elapsed)))

(defn- empty-ticks ^long [^long n world]
  (if (and (empty? (:players world)) (empty? (:spawning world)))
    (inc n)
    0))

(defn- pause-seconds ^long [opts]
  (let [src (if-let [s (:settings opts)] @s opts)]
    (long (or (:pause-when-empty-seconds src) 0))))

(defn- paused? [opts ^long n]
  (let [s (pause-seconds opts)]
    (when (and (pos? s) (= n (* 20 s)))
      (log/info "no players for" s "s, world paused"
                "until someone joins")
      (when-let [f (:on-pause opts)] (f)))
    (and (pos? s) (>= n (* 20 s)))))

(defn- idle? [opts n queue]
  (and (paused? opts n) (queue-empty? queue)))

(defn- mean-ms ^double [^longs window ^long ticks]
  (let [n (min ticks mspt-window)]
    (loop [k 0 sum 0]
      (if (< k n)
        (let [i (int (rem (- ticks 1 k) window-size))]
          (recur (inc k) (+ sum (aget window i))))
        (/ (double sum) n 1e6)))))

(defn- percentiles [^longs window counter]
  (let [n (int (min (count-of counter) window-size))]
    (when (pos? n)
      (let [arr (Arrays/copyOf window n)]
        (Arrays/sort arr)
        {:ticks  (count-of counter)
         :mspt   (mean-ms window (count-of counter))
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

(def ^:private ^:const crash-streak 20)

(defn- safe-tick [world events phases]
  (try (tick world events phases)
       (catch Throwable t
         (log/failure! ::tick "tick failed, its events retry once" t)
         (swap! (:failures world) conj ["the tick" t])
         nil)))

(defn- send-out! [deliver! world ^Deltas deltas]
  (when (or (seq (deltas/out-of deltas))
            (not (deltas/vacant? (deltas/entities-of deltas))))
    (try (deliver! world deltas)
         (catch Throwable t (log/error-with "deliver error:" t)))))

(defn- ticked-world [world-atom perf {:keys [io-input]}]
  (-> (tick-input world-atom perf (when io-input (io-input)))
      (assoc :failures (atom []))))

(defn- run-tick! [{:keys [carried]} world-atom queue deliver! perf
                  opts]
  (let [fresh (drain! queue)
        world (ticked-world world-atom perf opts)
        events (into @carried fresh)
        done (safe-tick world events (:phases opts phases))]
    (if-let [[world' deltas] done]
      (do (reset! carried [])
          (reset! world-atom (dissoc world' :failures))
          (send-out! deliver! world' deltas))
      (reset! carried fresh))
    @(:failures world)))

(defn- streaks [old failures]
  (let [n #(inc (long (first (old % [0]))))]
    (into {} (map (fn [[unit t]] [unit [(n unit) t]])) failures)))

(defn- crashing [streaks]
  (some (fn [[unit [n t]]]
          (when (<= crash-streak (long n)) [unit n t]))
        streaks))

(defn- crash! [{:keys [running]} opts [unit n t]]
  (log/error "**** THE SERVER CRASHED!")
  (log/error unit "failed" n "ticks in a row")
  (log/error-with "its last failure" t)
  (log/error "saving the world and stopping")
  (stop! running)
  (when-let [f (:on-crash opts)] (f {:unit unit :ticks n :cause t})))

(defn- supervised! [st opts failures]
  (let [s (swap! (:streaks st) streaks failures)]
    (when-let [c (crashing s)] (crash! st opts c))))

(defn- ticker-state [_cfg]
  {:window  (long-array window-size)
   :counter (AtomicLong. 0)
   :stamps  (long-array tps-window)
   :carried (atom [])
   :streaks (atom {})
   :running (AtomicBoolean. true)})

(defn- perf-of [{:keys [window counter ^longs stamps]} i t0 tps]
  (when (zero? (rem (long i) 20))
    (let [base (or (percentiles window counter) {})]
      (assoc base :tps (tps-of stamps i t0 tps)))))

(defn- one-tick! [{:keys [window counter] :as st} world-atom queue
                  deliver! perf t0 opts]
  (let [failures (run-tick! st world-atom queue deliver! perf opts)]
    (record! window counter (- (System/nanoTime) t0))
    (supervised! st opts failures)))

(defn- timed-tick! [st i perf world-atom queue deliver! opts]
  (let [t0 (System/nanoTime)
        p (or (perf-of st i t0 20) perf)]
    (aset ^longs (:stamps st) (int (rem (long i) tps-window)) t0)
    (one-tick! st world-atom queue deliver! p t0 opts)
    p))

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

(defn- daemon! ^Thread [^Runnable f ^String name]
  (doto (Thread. f name)
    (.setDaemon true)
    (.start)))

(defn- joined! [^Thread t ^long ms] (.join t ms))

(defn- ticker-thread
  ^Thread [st running world-atom queue deliver! opts]
  (let [run #(ticker-loop st running world-atom queue deliver! opts)]
    (daemon! run "collider-ticker")))

(defn start-ticker!
  "Starts ticking world-atom on the events of queue.
  Each tick hands its deltas to deliver!. Returns a handle to stop
  it with. A unit that fails twenty ticks in a row stops the ticker
  and calls the crash callback of opts."
  ([world-atom queue deliver!]
   (start-ticker! world-atom queue deliver! nil))
  ([world-atom queue deliver! opts]
   (let [st (ticker-state opts)
         running (:running st)
         thread (ticker-thread
                  st running world-atom queue deliver! opts)]
     {:thread  thread
      :running running
      :stats   #(percentiles (:window st) (:counter st))})))

(defn stop-ticker!
  "Stops the ticker and waits up to a second for it to end."
  [{:keys [thread running]}]
  (stop! running)
  (joined! thread 1000)
  nil)
