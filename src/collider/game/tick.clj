(ns collider.game.tick
  "The tick and the ticker that runs it."
  (:require [collider.game.state :as state]
            [collider.game.deltas :as deltas]
            [collider.game.detector :as detector]
            [collider.log :as log]
            [collider.game.systems.block.updates :as block-updates]
            [collider.game.systems.blocks :as blocks]
            [collider.game.systems.chat :as chat]
            [collider.game.systems.chunks :as chunks]
            [collider.game.systems.containers :as containers]
            [collider.game.systems.daynight :as daynight]
            [collider.game.systems.dripleaf :as dripleaf]
            [collider.game.systems.explosions :as explosions]
            [collider.game.systems.falling :as falling]
            [collider.game.systems.inventory :as inventory]
            [collider.game.systems.items :as items]
            [collider.game.systems.jukebox :as jukebox]
            [collider.game.systems.keepalive :as keepalive]
            [collider.game.systems.mobs :as mobs]
            [collider.game.systems.players :as players]
            [collider.game.systems.pose :as pose]
            [collider.game.systems.random.tick :as random-tick]
            [collider.game.systems.sleep :as sleep]
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
                   #'tnt/first-step])

(def phases [[#'pose/pose]
             systems
             [#'explosions/explosions]
             post-systems
             [#'detector/observe]])

(defn tick
  "Returns [world' deltas] for one tick of world with the input events."
  [world events]
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

(defn- record!
  "Notes how long one tick took."
  [^longs window ^AtomicLong counter ^long elapsed]
  (aset window (int (rem (.getAndIncrement counter) window-size)) elapsed))

(defn- idle? [world ^ConcurrentLinkedQueue queue]
  (and (.isEmpty queue) (empty? (:players world))))

(defn- percentiles
  "Returns how long recent ticks took, in the middle and at the worst."
  [^longs window ^AtomicLong counter]
  (let [n (int (min (.get counter) window-size))]
    (when (pos? n)
      (let [arr (Arrays/copyOf window n)]
        (Arrays/sort arr)
        {:ticks  (.get counter)
         :p50-ms (/ (aget arr (quot n 2)) 1e6)
         :p99-ms (/ (aget arr (min (dec n) (int (* n 0.99)))) 1e6)
         :max-ms (/ (aget arr (dec n)) 1e6)}))))

(defn- tps-of
  "Returns the rate of recent ticks per second, never above the target."
  ^double [^longs stamps ^long i ^long now ^long target-tps]
  (let [n (min (inc i) tps-window)]
    (if (< n 2)
      (double target-tps)
      (let [past (aget stamps (int (rem (- (inc i) n) tps-window)))]
        (if (<= now past)
          (double target-tps)
          (min (double target-tps) (/ (* 1.0E9 (dec n)) (- now past))))))))

(defn- pace
  "Holds until the next tick is due, and returns the time it was due."
  ^long [^long next-ns ^long step-ns]
  (let [target (+ next-ns step-ns)
        now (System/nanoTime)
        target (if (> (- now target) 1000000000) now target)
        sleep (quot (- target now) 1000000)]
    (when (pos? sleep) (^[long] Thread/sleep sleep))
    target))

(defn- tick-input
  "Returns the world the next tick starts from, with the clock and whatever
   the world outside the tick reports."
  [world-atom perf io]
  (-> @world-atom
      (assoc :time-ms (System/currentTimeMillis))
      (cond-> perf (assoc :perf perf)
              io (merge io))))

(defn- safe-tick
  "Returns one tick of world, or world unchanged when the tick throws."
  [world events]
  (try (tick world events)
       (catch Throwable t
         (log/warn "tick error:" t)
         [world deltas/empty-deltas])))

(defn- send-out! [deliver! world ^Deltas deltas]
  (when (or (seq (.out deltas)) (pos? (count (.entities deltas))))
    (try (deliver! world deltas)
         (catch Throwable t (log/warn "deliver error:" t)))))

(defn- run-tick!
  "Ticks the world once on the events that have come in, and sends out what
   changed."
  [world-atom ^ConcurrentLinkedQueue queue deliver! perf io-input]
  (let [events (drain! queue)
        world (tick-input world-atom perf (when io-input (io-input)))
        [world' deltas] (safe-tick world events)]
    (reset! world-atom world')
    (send-out! deliver! world' deltas)))

(defn- ticker-state [_cfg]
  {:window  (long-array window-size)
   :counter (AtomicLong. 0)
   :stamps  (long-array tps-window)})

(defn- perf-of
  "Returns the tick timings worth reporting, or nil when it is not time to
   report."
  [{:keys [window counter ^longs stamps]} i t0 tps]
  (when (zero? (rem (long i) 20))
    (assoc (or (percentiles window counter) {}) :tps (tps-of stamps i t0 tps))))

(defn- one-tick! [{:keys [window counter]} world-atom queue deliver! perf t0 io-input]
  (run-tick! world-atom queue deliver! perf io-input)
  (record! window counter (- (System/nanoTime) t0)))

(defn- ticker-loop [st ^AtomicBoolean running world-atom queue deliver! io-input]
  (let [^longs stamps (:stamps st)]
    (loop [next-ns (System/nanoTime), i 0, perf nil]
      (when (.get running)
        (if (idle? @world-atom queue)
          (do (Thread/sleep 50) (recur (System/nanoTime) 0 nil))
          (let [t0 (System/nanoTime)
                p (or (perf-of st i t0 20) perf)]
            (aset stamps (int (rem (long i) tps-window)) t0)
            (one-tick! st world-atom queue deliver! p t0 io-input)
            (recur (pace next-ns nominal-tick-ns) (inc (long i)) p)))))))

(defn- ticker-thread ^Thread [st running world-atom queue deliver! opts]
  (doto (Thread. ^Runnable #(ticker-loop st running world-atom queue deliver! (:io-input opts))
                 "collider-ticker")
    (.setDaemon true)
    (.start)))

(defn start-ticker!
  "Starts ticking world-atom: input is taken from queue and the deltas of
   each tick go to deliver!. Returns a handle for stop-ticker!."
  ([world-atom queue deliver!] (start-ticker! world-atom queue deliver! nil))
  ([world-atom ^ConcurrentLinkedQueue queue deliver! opts]
   (let [st (ticker-state opts)
         running (AtomicBoolean. true)
         thread (ticker-thread st running world-atom queue deliver! opts)]
     {:thread  thread
      :running running
      :stats   #(percentiles (:window st) (:counter st))})))

(defn stop-ticker!
  "Stops the ticker of a start-ticker! handle."
  [{:keys [^Thread thread ^AtomicBoolean running]}]
  (.set running false)
  (.join thread 1000)
  nil)
