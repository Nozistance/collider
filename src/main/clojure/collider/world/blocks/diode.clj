(ns collider.world.blocks.diode
  "The repeater, a diode that passes a full signal on after a delay
  and holds while a diode beside it powers it."
  (:require [collider.world.block :as block]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.signal :as signal]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(defn- repeater? [^long st] (= :repeater (block/type-of st)))

(defn diode?
  "Returns true when st is a repeater or a comparator."
  [^long st]
  (contains? #{:repeater :comparator} (block/type-of st)))

(defn- on? [^long st] (= :true (:powered (block/props-of st))))

(defn- delay-of ^long [^long st] (* 2 (block/prop-long st :delay)))

(defn input
  "Returns the signal diode st at p reads from the cell it faces, the
  power of a wire there when that is more."
  ^long [chunks p ^long st]
  (let [f (block/facing-of st)
        q (dir/toward p f)
        s (signal/signal chunks q f)
        there (chunk/at chunks q)]
    (if (or (>= s 15) (not= :redstone-wire (block/type-of there)))
      s
      (max s (block/prop-long there :power)))))

(defn front
  "Returns the ops of st at p that tell the cell it faces away from,
  and the cells around that cell, of a change."
  [p ^long st]
  (let [f (block/facing-of st)
        q (dir/toward p (dir/opposite f))]
    [[:changed q st f] [:notify q f st]]))

(defn- turns-on? [chunks p st] (pos? (input chunks p st)))

(defn- side-signal ^long [chunks p d]
  (let [st (chunk/at chunks (dir/toward p d))]
    (if (diode? st) (signal/direct-signal chunks (dir/toward p d) d) 0)))

(defn- held? [chunks p ^long st]
  (let [f (block/facing-of st)]
    (pos? (max (side-signal chunks p (dir/clockwise f))
               (side-signal chunks p (dir/counter-clockwise f))))))

(defn held
  "Returns repeater st at p locked when a diode beside it powers it."
  ^long [chunks p ^long st]
  (block/with st :locked (block/flag (held? chunks p st))))

(defn prioritized?
  "Returns true when diode st at p faces a diode that does not
  face it back."
  [chunks p ^long st]
  (let [d (dir/opposite (block/facing-of st))
        o (chunk/at chunks (dir/toward p d))]
    (and (diode? o) (not= d (block/facing-of o)))))

(defn- priority ^long [chunks p st]
  (cond (prioritized? chunks p st) -3
        (on? st) -2
        :else -1))

(defn- checked [chunks p {:keys [tick will-tick?]}]
  (let [st (chunk/at chunks p)]
    (when (and (not (held? chunks p st))
               (not= (on? st) (turns-on? chunks p st))
               (not (and will-tick? (will-tick? p (block/block-of st)))))
      [[:tick p (+ (long tick) (delay-of st)) (priority chunks p st)
        :repeater]])))

(defn- reshaped [chunks p {:keys [side] :as ctx}]
  (let [st (chunk/at chunks p)]
    (if (and (= :down side) (not (support/supported? chunks p st)))
      ((:reshape support/rule) chunks p ctx)
      (when (not= (dir/axis side) (dir/axis (block/facing-of st)))
        (let [st' (held chunks p st)]
          (when (not= st st') [[p st']]))))))

(defn- changed [chunks p ctx]
  (if (support/supported? chunks p (chunk/at chunks p))
    (checked chunks p ctx)
    ((:reshape support/rule) chunks p ctx)))

(defn- due [chunks p {:keys [tick]}]
  (let [st (chunk/at chunks p)
        on (turns-on? chunks p st)]
    (cond
      (held? chunks p st) nil
      (on? st) (when-not on
                 [[p (block/with st :powered :false) nil update/clients]])
      :else (cond-> [[p (block/with st :powered :true) nil update/clients]]
              (not on) (conj [:tick p (+ (long tick) (delay-of st)) -2 :repeater])))))

(defn placed-tick
  "Returns the tick that block st a player set at p asks for, at once
  when it is a repeater with its input on, else nil."
  [chunks p ^long st tick]
  (when (and (repeater? st) (turns-on? chunks p st))
    (inc (long tick))))

(defn stepped
  "Returns repeater st with its delay one step on, round from 4 to 1."
  ^long [^long st]
  (let [n (block/prop-long st :delay)]
    (block/with st :delay (keyword (str (inc (mod n 4)))))))

(def rule
  "The block rule of the repeater. It is held as attached blocks are."
  (assoc support/rule
         :name :diode
         :match? (fn [_chunks st _p] (repeater? st))
         :pass :both
         :place? false
         :reach 2
         :wake (constantly :neighbor)
         :reshape (fn [chunks p ctx]
                    (case (:pass ctx)
                      :neighbor (changed chunks p ctx)
                      :shape (reshaped chunks p ctx)))
         :due due))
