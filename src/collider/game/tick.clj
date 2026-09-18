(ns collider.game.tick
  "The tick and its ticker."
  (:require [collider.game.state :as state]
            [collider.game.deltas :as deltas]
            [collider.game.detector :as detector]
            [collider.log :as log]
            [collider.game.systems.block.updates :as block-updates]
            [collider.game.systems.blocks :as blocks]
            [collider.game.systems.brewing :as brewing]
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
            [collider.game.systems.players :as players]
            [collider.game.systems.pose :as pose]
            [collider.game.systems.projectiles :as projectiles]
            [collider.game.systems.random.tick :as random-tick]
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

(def systems [#'chunks/chunk-streaming
              #'players/players
              #'blocks/block-edits
              #'dripleaf/dripleaf-tilt
              #'block-updates/block-updates
              #'random-tick/random-ticks
              #'items/items
              #'jukebox/jukebox-songs
              #'falling/falling-blocks
              #'mobs/mobs-system
              #'tnt/tnt-system
              #'projectiles/projectiles
              #'damage/damage
              #'sleep/sleep
              #'inventory/inventory
              #'containers/containers
              #'chat/chat
              #'daynight/daynight
              #'keepalive/keepalive])

(def post-systems [#'players/late-tracking
                   #'block-updates/block-flush
                   #'blocks/acks
                   #'weather-system/weather
                   #'falling/first-step
                   #'tnt/first-step
                   #'projectiles/first-step
                   #'items/pickups
                   #'containers/broadcast])

(def phases [[#'spawning/placing]
             [#'pose/pose]
             [#'furnaces/furnace-cooking #'brewing/brewing]
             [#'consume/consume]
             systems
             [#'explosions/explosions]
             post-systems
             [#'chunks/unloading]
             [#'detector/observe]])

(defn tick [world events]
  (let [input (deltas/input events)]
    (reduce (fn [[w d] phase]
              (let [d' (deltas/of phase w d)]
                [(state/apply w d') (deltas/merge d d')]))
            [(state/apply world input) input] phases)))

(def ^:private ^:const nominal-tick-ns 50000000)
(def ^:private ^:const window-size 4096)
(def ^:private ^:const tps-window 100)
(defn- drain! [^ConcurrentLinkedQueue q]
  (loop [acc (transient [])]
    (if-some [e (.poll q)]
      (recur (conj! acc e))
      (persistent! acc))))

(defn- record! [^longs window ^AtomicLong counter ^long elapsed]
  (aset window (int (rem (.getAndIncrement counter) window-size)) elapsed))

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

(defn- tps-of ^double [^longs stamps ^long i ^long now ^long target-tps]
  (let [n (min (inc i) tps-window)]
    (if (< n 2)
      (double target-tps)
      (let [past (aget stamps (int (rem (- (inc i) n) tps-window)))]
        (if (<= now past)
          (double target-tps)
          (min (double target-tps) (/ (* 1.0E9 (dec n)) (- now past))))))))

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
         (log/warn "tick error:" t)
         [world deltas/empty-deltas])))

(defn- send-out! [deliver! world ^Deltas deltas]
  (when (or (seq (.out deltas)) (pos? (count (.entities deltas))))
    (try (deliver! world deltas)
         (catch Throwable t (log/warn "deliver error:" t)))))

(defn- run-tick! [world-atom ^ConcurrentLinkedQueue queue deliver! perf io-input]
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
    (assoc (or (percentiles window counter) {}) :tps (tps-of stamps i t0 tps))))

(defn- one-tick! [{:keys [window counter]} world-atom queue deliver! perf t0 io-input]
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

(defn- ticker-thread ^Thread [st running world-atom queue deliver! opts]
  (doto (Thread. ^Runnable #(ticker-loop st running world-atom queue deliver! opts)
                 "collider-ticker")
    (.setDaemon true)
    (.start)))

(defn start-ticker!
  "Starts a daemon thread that ticks world-atom on the events from queue and
   gives the deltas of each tick to deliver!. Returns a handle for the stop."
  ([world-atom queue deliver!] (start-ticker! world-atom queue deliver! nil))
  ([world-atom ^ConcurrentLinkedQueue queue deliver! opts]
   (let [st (ticker-state opts)
         running (AtomicBoolean. true)
         thread (ticker-thread st running world-atom queue deliver! opts)]
     {:thread  thread
      :running running
      :stats   #(percentiles (:window st) (:counter st))})))

(defn stop-ticker! [{:keys [^Thread thread ^AtomicBoolean running]}]
  (.set running false)
  (.join thread 1000)
  nil)
