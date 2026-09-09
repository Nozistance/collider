(ns collider.game.tick
  (:require [collider.game.sign :as sign]
            [collider.game.state :as state]
            [collider.game.deltas :as deltas]
            [collider.game.detector :as detector]
            [collider.game.out :as out]
            [collider.log :as log]
            [collider.game.systems.block.updates :as block-updates]
            [collider.game.systems.blocks :as blocks]
            [collider.game.systems.chat :as chat]
            [collider.game.systems.chunks :as chunks]
            [collider.game.systems.daynight :as daynight]
            [collider.game.systems.falling :as falling]
            [collider.game.systems.inventory :as inventory]
            [collider.game.systems.items :as items]
            [collider.game.systems.keepalive :as keepalive]
            [collider.game.systems.mobs :as mobs]
            [collider.game.systems.players :as players]
            [collider.game.systems.random.tick :as random-tick]
            [collider.game.systems.sleep :as sleep]
            [collider.game.systems.tnt :as tnt]
            [collider.game.systems.damage :as damage])
  (:import (collider.game.deltas Deltas)
           (java.util Arrays)
           (java.util.concurrent ConcurrentLinkedQueue)
           (java.util.concurrent.atomic AtomicBoolean AtomicLong)))

(set! *warn-on-reflection* true)

(def systems [#'chunks/chunk-streaming
   #'players/players
   #'blocks/block-edits
   #'block-updates/block-updates
   #'random-tick/random-ticks
   #'items/items
   #'falling/falling-blocks
   #'mobs/mobs-system
   #'tnt/tnt-system
   #'damage/damage
   #'sleep/sleep
   #'inventory/inventory
   #'chat/chat
   #'daynight/daynight
   #'keepalive/keepalive])

(defn- final-records [recs]
  (let [last (into {} recs)]
    (into [] (comp (map first) (distinct) (map (fn [pos] [pos (get last pos)]))) recs)))

(defn- block-flush-deltas [w]
  (when-let [events (:block-events w)]
    (concat [[:block-events-flushed]]
            (map (fn [[cp recs]] (out/all (out/blocks-changed cp (final-records recs)))) events)
            (for [[_ recs] events [pos _] recs :when (sign/at w pos)]
              (out/all (out/block-entity pos))))))

(defn- apply-events [world events]
  (loop [w world i 0 origins {}]
    (if-let [ev (nth events i nil)]
      (let [o (state/use-origin w ev)]
        (recur (state/apply-event w ev) (inc i) (if o (assoc origins i o) origins)))
      (assoc w :use-origins origins))))

(defn tick [world events]
  (let [world' (cond-> (update world :tick inc)
                 (get-in world [:rules :advance-time] true) (update :time-of-day (fnil inc 0)))
        world' (apply-events world' events)
        deltas (deltas/of systems world' events)
        [w1 d1] (state/apply-deltas world' deltas)
        post (concat (block-flush-deltas w1) (blocks/ack-deltas events) (detector/observe world events d1 w1))]
    (if (empty? post)
      [w1 d1]
      (let [[w2 d2] (state/apply-deltas w1 post)]
        [w2 (deltas/merge-deltas d1 d2)]))))

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

(defn- idle? [world ^ConcurrentLinkedQueue queue]
  (and (.isEmpty queue) (empty? (:players world))))

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
        now    (System/nanoTime)
        target (if (> (- now target) 1000000000) now target)
        sleep  (quot (- target now) 1000000)]
    (when (pos? sleep) (^[long] Thread/sleep sleep))
    target))

(defn- tick-input [world-atom perf io]
  (-> @world-atom
      (assoc :time-ms (System/currentTimeMillis))
      (cond-> perf (assoc :perf perf)
              io   (merge io))))

(defn- safe-tick [world events]
  (try (tick world events)
       (catch Throwable t
         (log/info "tick error:" t)
         [world deltas/empty-deltas])))

(defn- send-out! [deliver! world ^Deltas deltas]
  (when (or (seq (.out deltas)) (pos? (count (.entities deltas))))
    (try (deliver! world deltas)
         (catch Throwable t (log/info "deliver error:" t)))))

(defn- run-tick! [world-atom ^ConcurrentLinkedQueue queue deliver! perf io-input]
  (let [events (drain! queue)
        world  (tick-input world-atom perf (when io-input (io-input)))
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

(defn- ticker-loop [st ^AtomicBoolean running world-atom queue deliver! io-input]
  (let [^longs stamps (:stamps st)]
    (loop [next-ns (System/nanoTime), i 0, perf nil]
      (when (.get running)
        (if (idle? @world-atom queue)
          (do (Thread/sleep 50) (recur (System/nanoTime) 0 nil))
          (let [t0 (System/nanoTime)
                p  (or (perf-of st i t0 20) perf)]
            (aset stamps (int (rem (long i) tps-window)) t0)
            (one-tick! st world-atom queue deliver! p t0 io-input)
            (recur (pace next-ns nominal-tick-ns) (inc (long i)) p)))))))

(defn start-ticker!
  ([world-atom queue deliver!] (start-ticker! world-atom queue deliver! nil))
  ([world-atom ^ConcurrentLinkedQueue queue deliver! opts]
   (let [st      (ticker-state opts)
         running (AtomicBoolean. true)
         thread  (doto (Thread. ^Runnable #(ticker-loop st running world-atom queue deliver!
                                            (:io-input opts))
                                "collider-ticker")
                   (.setDaemon true)
                   (.start))]
     {:thread thread
      :running running
      :stats #(percentiles (:window st) (:counter st))})))

(defn stop-ticker! [{:keys [^Thread thread ^AtomicBoolean running]}]
  (.set running false)
  (.join thread 1000)
  nil)
