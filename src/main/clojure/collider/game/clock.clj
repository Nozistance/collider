(ns collider.game.clock
  "The world clocks and the timelines that read them.
  A clock counts whole ticks and a partial tick. It runs at its
  rate unless paused or the advance_time rule is off. The day of
  every level follows the overworld clock."
  (:require [collider.data :as data]
            [collider.num :as num]
            [collider.world.env.dimension :as dimension]))

(set! *warn-on-reflection* true)

(def fresh
  "The state of a clock nothing has moved yet."
  {:total-ticks 0 :partial-tick 0.0 :rate 1.0 :paused false})

(defn- marker [period v]
  (let [full? (map? v)]
    {:ticks (if full? (get v "ticks") v)
     :period period
     :show? (and full? (get v "show_in_commands" false))}))

(defn- timeline [{clock "clock" period "period_ticks"
                  markers "time_markers"}]
  (cond-> {:clock   (data/kebab clock)
           :markers (update-vals markers #(marker period %))}
    period (assoc :period period)))

(def ^:private ^:table timeline-table
  (delay (update-vals (data/pack "timeline") timeline)))

(defn timelines
  "Returns the timelines of the vanilla pack, by id.
  Each names the clock it reads, its period in ticks and its time
  markers."
  []
  @timeline-table)

(defn names
  "Returns the clocks of the world_clock registry."
  []
  (get (data/datapack) "world_clock"))

(defn- whole [c]
  (if (and (== 4 (count c)) (contains? c :total-ticks)
           (contains? c :partial-tick) (contains? c :rate)
           (contains? c :paused))
    c
    (merge fresh c)))

(defn state
  "Returns the state of clock in world."
  [world clock]
  (whole (get (:clocks world) clock)))

(defn ticks
  "Returns the whole ticks clock has counted in world."
  ^long [world clock]
  (long (get (get (:clocks world) clock) :total-ticks 0)))

(defn day-ticks
  "Returns the ticks of the overworld clock."
  ^long [world]
  (ticks world :overworld))

(defn default-of
  "Returns the clock that level dim follows, or nil."
  [dim]
  (:default-clock (dimension/type-of dim)))

(defn ticked
  "Returns clock state c one tick on."
  [c]
  (if (:paused c)
    c
    (let [r (double (:rate c))
          p (num/f32 (+ (double (:partial-tick c)) r))
          n (num/floor p)]
      (assoc c :partial-tick (num/f32 (- p n))
             :total-ticks (+ (long (:total-ticks c)) n)))))

(defn advanced
  "Returns clocks, every clock of the registry one tick on."
  [clocks]
  (into {}
        (map (fn [k] [k (ticked (whole (get clocks k)))]))
        (names)))

(defn advancing?
  "Returns true when the rules of world let clocks run."
  [world]
  (boolean (get-in world [:rules :advance-time] true)))

(defn network-state
  "Returns clock state c as the client runs it."
  [c advancing]
  {:total-ticks (long (:total-ticks c))
   :partial-tick (num/f32 (:partial-tick c))
   :rate (if (or (:paused c) (not advancing))
           0.0
           (num/f32 (:rate c)))})

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
        (for [[_ t] (timelines)
              :when (= clock (:clock t))
              m (:markers t)]
          m)))

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
  (get (timelines) id))

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
