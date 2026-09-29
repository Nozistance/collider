(ns collider.game.clock
  "The world clocks and the timelines that read them.
  A clock counts whole ticks and a partial tick. It runs at its
  rate unless paused or the advance_time rule is off. The day of
  every level follows the overworld clock."
  (:require [collider.data :as data]))

(set! *warn-on-reflection* true)

(def fresh
  "The state of a clock nothing has moved yet."
  {:total-ticks 0 :partial-tick 0.0 :rate 1.0 :paused false})

(def timelines
  "The timelines of the vanilla pack.
  Each names the clock it reads, its period in ticks and its time
  markers as the tick and whether commands offer the marker."
  {:day {:clock :overworld :period 24000
         :markers {:day [1000 true] :noon [6000 true]
                   :night [13000 true] :midnight [18000 true]
                   :wake-up-from-sleep [0 false]
                   :roll-village-siege [18000 false]}}
   :early-game {:clock :overworld}
   :moon {:clock :overworld :period 192000}
   :villager-schedule {:clock :overworld :period 24000}})

(defn names
  "Returns the clocks of the world_clock registry."
  []
  (get (data/datapack) "world_clock"))

(defn state
  "Returns the state of clock in world."
  [world clock]
  (merge fresh (get-in world [:clocks clock])))

(defn ticks
  "Returns the whole ticks clock has counted in world."
  ^long [world clock]
  (long (:total-ticks (state world clock))))

(defn day-ticks
  "Returns the ticks of the overworld clock."
  ^long [world]
  (ticks world :overworld))

(defn default-of
  "Returns the clock that level dim follows, or nil."
  [dim]
  (:default-clock (data/dimension-type dim)))

(defn- as-float ^double [x] (unchecked-float (double x)))

(defn ticked
  "Returns clock state c one tick on."
  [c]
  (if (:paused c)
    c
    (let [r (double (:rate c))
          p (as-float (+ (double (:partial-tick c)) r))
          n (long (Math/floor p))]
      (assoc c :partial-tick (as-float (- p n))
             :total-ticks (+ (long (:total-ticks c)) n)))))

(defn advanced
  "Returns clocks, every clock of the registry one tick on."
  [clocks]
  (into {}
        (map (fn [k] [k (ticked (merge fresh (get clocks k)))]))
        (names)))

(defn advancing?
  "Returns true when the rules of world let clocks run."
  [world]
  (boolean (get-in world [:rules :advance-time] true)))

(defn network-state
  "Returns clock state c as the client runs it."
  [c advancing]
  {:total-ticks (long (:total-ticks c))
   :partial-tick (as-float (:partial-tick c))
   :rate (if (or (:paused c) (not advancing))
           0.0
           (as-float (:rate c)))})

(defn sync-of
  "Returns the network state of clocks in world, by clock."
  [world clocks]
  (let [on (advancing? world)]
    (into {} (map (fn [k] [k (network-state (state world k) on)]))
          clocks)))

(defn full-sync
  "Returns the network state of every clock in world."
  [world]
  (sync-of world (names)))

(defn set-ticks
  "Returns clock state c at total ticks n, no partial tick."
  [c n]
  (assoc c :total-ticks (long n) :partial-tick 0.0))

(defn added
  "Returns clock state c n ticks on, never below zero."
  [c n]
  (assoc c :total-ticks (max 0 (+ (long (:total-ticks c)) (long n)))))

(defn markers
  "Returns the time markers of clock, by id."
  [clock]
  (into {}
        (for [[_ {c :clock p :period ms :markers}] timelines
              :when (= c clock)
              [k [t show?]] ms]
          [(data/wire k) {:ticks t :period p :show? show?}])))

(defn- ticks-to ^long [{:keys [ticks period]} ^long total]
  (if period
    (let [d (- (long ticks) (rem total (long period)))]
      (+ total (if (pos? d) d (+ (long period) d))))
    (long ticks)))

(defn moved-to
  "Returns clock state c moved to marker id of clock, or nil when
  clock has no such marker."
  [c clock id]
  (when-let [m (get (markers clock) id)]
    (set-ticks c (ticks-to m (long (:total-ticks c))))))

(defn timeline-of
  "Returns the timeline of id, or nil."
  [id]
  (some (fn [[k t]] (when (= id (data/wire k)) t)) timelines))

(defn timeline-ticks
  "Returns where in its period timeline t stands at total ticks."
  ^long [t ^long total]
  (if-let [p (:period t)] (rem total (long p)) total))

(defn repetitions
  "Returns how many whole periods of timeline t total ticks hold."
  ^long [t ^long total]
  (if-let [p (:period t)]
    (long (unchecked-int (quot total (long p))))
    0))
