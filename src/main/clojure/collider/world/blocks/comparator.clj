(ns collider.world.blocks.comparator
  "The comparator, a diode that passes on, compares or subtracts the
  signal behind it against the signals at its sides."
  (:require [collider.world.block :as block]
            [collider.world.blocks.diode :as diode]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.signal :as signal]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(def ^:const delay-ticks
  "The ticks a comparator waits before it follows its inputs."
  2)

(defn comparator?
  "Returns true when st is a comparator."
  [^long st]
  (= :comparator (block/type-of st)))

(defn- on? [^long st] (= :true (:powered (block/props-of st))))

(defn- subtract? [^long st] (= :subtract (:mode (block/props-of st))))

(defn- analog [ctx chunks p st side]
  (let [st (long st)
        v (if (#{:copper-bulb-block :weathering-copper-bulb}
               (block/type-of st))
            (if (= :true (:lit (block/props-of st))) 15 0)
            (block/analog-signal st))]
    (if (neg? (long v))
      ((:analog ctx) chunks p st side)
      v)))

(defn- framed [ctx r f]
  (let [fs (or (force (:frames ctx))
               (throw (ex-info "the frames of the level are not given"
                               {:pos r})))
        vs (keep (fn [[facing v]] (when (= f facing) v)) (get fs r))]
    (when (= 1 (count vs)) (first vs))))

(defn- behind [ctx chunks q f s]
  (let [r (dir/toward q f)
        st (chunk/at chunks r)
        v (framed ctx r f)
        a (when (block/analog-output? st)
            (long (analog ctx chunks r st (dir/opposite f))))]
    (if (and v a) (max (long v) a) (or v a s))))

(defn- input ^long [ctx chunks p ^long st]
  (let [s (diode/input chunks p st)
        f (block/facing-of st)
        q (dir/toward p f)
        there (chunk/at chunks q)]
    (cond
      (block/analog-output? there)
      (analog ctx chunks q there (dir/opposite f))
      (and (< s 15) (block/conductor? there))
      (behind ctx chunks q f s)
      :else s)))

(defn- side ^long [chunks p d]
  (let [q (dir/toward p d)
        st (chunk/at chunks q)]
    (cond
      (= :redstone-block (block/block-of st)) 15
      (= :redstone-wire (block/block-of st)) (block/prop-long st :power)
      (block/signal-source? st) (signal/direct-signal chunks q d)
      :else 0)))

(defn- sides ^long [chunks p ^long st]
  (let [f (block/facing-of st)]
    (max (side chunks p (dir/clockwise f))
         (side chunks p (dir/counter-clockwise f)))))

(defn- output ^long [ctx chunks p ^long st]
  (let [in (input ctx chunks p st)]
    (if (zero? in)
      0
      (let [s (sides chunks p st)]
        (cond (> s in) 0
              (subtract? st) (- in s)
              :else in)))))

(defn- turns-on? [ctx chunks p ^long st]
  (let [in (input ctx chunks p st)]
    (and (pos? in)
         (let [s (sides chunks p st)]
           (or (> in s) (and (= in s) (not (subtract? st))))))))

(defn- checked [chunks p {:keys [tick will-tick?] :as ctx}]
  (let [st (chunk/at chunks p)]
    (when (and (not (and will-tick? (will-tick? p :comparator)))
               (or (not= (output ctx chunks p st)
                         (chunk/signal-at chunks p))
                   (not= (on? st) (turns-on? ctx chunks p st))))
      [[:tick p (+ (long tick) delay-ticks)
        (if (diode/prioritized? chunks p st) -1 0) :comparator]])))

(defn refreshed
  "Returns the changes of comparator st at p as it takes the output
  its inputs give. ctx holds the frames and the blocks whose block
  entity decides their signal, as level/level-ctx gives them."
  [ctx chunks p ^long st]
  (let [out (output ctx chunks p st)
        old (chunk/signal-at chunks p)]
    (into [[:signal p out]]
          (when (or (not= old out) (not (subtract? st)))
            (let [on (turns-on? ctx chunks p st)]
              (cond-> []
                (not= on (on? st))
                (conj [p (block/with st :powered (block/flag on)) nil
                       update/clients])
                true (into (diode/front p st))))))))

(defn stepped
  "Returns comparator st with its mode switched."
  ^long [^long st]
  (block/with st :mode (if (subtract? st) :compare :subtract)))

(defn placed-tick
  "Returns the tick that block st a player set at p asks for, at once
  when it is a comparator its inputs turn on, else nil."
  [ctx chunks p st tick]
  (when (and (comparator? st) (turns-on? ctx chunks p st))
    (inc (long tick))))

(defn- changed [chunks p ctx]
  (if (support/supported? chunks p (chunk/at chunks p))
    (checked chunks p ctx)
    ((:reshape support/rule) chunks p ctx)))

(defn- shaped [chunks p {:keys [side] :as ctx}]
  (when (and (= :down side)
             (not (support/supported? chunks p (chunk/at chunks p))))
    ((:reshape support/rule) chunks p ctx)))

(def rule
  "The block rule of the comparator. It is held as attached blocks
  are."
  (assoc support/rule
         :name :comparator
         :match? (fn [_chunks st _p] (comparator? st))
         :pass :both
         :place? false
         :reach 2
         :wake (constantly :neighbor)
         :reshape (fn [chunks p ctx]
                    (case (:pass ctx)
                      :neighbor (changed chunks p ctx)
                      :shape (shaped chunks p ctx)))
         :due (fn [chunks p ctx]
                (refreshed ctx chunks p (chunk/at chunks p)))))
