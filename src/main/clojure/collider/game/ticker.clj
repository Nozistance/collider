(ns collider.game.ticker
  "The ticker: it runs the tick twenty times a second."
  (:require [collider.game.deltas :as deltas]
            [collider.game.tick :as tick]
            [collider.log :as log]
            [collider.par :as par])
  (:import (collider.game.deltas.record Deltas)
           (java.util Arrays)
           (java.util.concurrent ConcurrentLinkedQueue)
           (java.util.concurrent.atomic AtomicBoolean AtomicLong)))

(set! *warn-on-reflection* true)

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
  (try (tick/tick world events phases)
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

(defn- phases-of [opts]
  (let [p (:phases opts tick/phases)]
    (if (fn? p) (p) p)))

(defn- run-tick! [{:keys [carried]} world-atom queue deliver! perf
                  opts]
  (let [fresh (drain! queue)
        world (ticked-world world-atom perf opts)
        events (into @carried fresh)
        done (safe-tick world events (phases-of opts))]
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
    (daemon! #(par/in-pool run) "collider-ticker")))

(defn start-ticker!
  "Starts ticking world-atom on the events of queue.
  Each tick hands its deltas to deliver!. Returns a handle to stop
  it with. A unit that fails twenty ticks in a row stops the ticker
  and calls the crash callback of opts. The :phases of opts are the
  phases, or a function that returns them each tick."
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
