(ns collider.world.blocks.torch
  "The redstone torch, lit while the block it stands on is unpowered.
  It burns out when it turns off too often."
  (:require [collider.world.block :as block]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.signal :as signal]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(def ^:const toggle-delay
  "The ticks a torch waits before it follows its input."
  2)

(def ^:const recent-ticks
  "How many ticks a turn off counts toward burning out."
  60)

(def ^:const max-toggles
  "How many recent turns off burn a torch out."
  8)

(def ^:const restart-delay
  "The ticks a burnt out torch waits before it tries again."
  160)

(defn torch?
  "Returns true when st is a redstone torch, standing or on a wall."
  [^long st]
  (contains? #{:redstone-torch :redstone-wall-torch} (block/type-of st)))

(defn- lit? [^long st] (= :true (:lit (block/props-of st))))

(defn- behind [^long st]
  (if (= :redstone-wall-torch (block/type-of st))
    (dir/opposite (block/facing-of st))
    :down))

(defn- powered? [chunks p st]
  (let [d (behind st)]
    (pos? (signal/signal chunks (dir/toward p d) d))))

(defn told
  "Returns the ops of torch st at p that tell the neighbours of each
  cell around it of a change."
  [p ^long st]
  (mapv #(vector :notify (dir/toward p %) nil st) dir/six))

(defn- recent
  "Returns the turns off of toggles at p that still count at tick t."
  [toggles p ^long t]
  (filter #(<= (- t (long %)) recent-ticks) (get toggles p)))

(defn toggled
  "Returns toggles, the turns off of torches by position, with one at p
  on tick t. Turns off that no longer count go."
  [toggles p t]
  (let [t (long t)
        kept (into {} (keep (fn [[q _]]
                              (when-let [ts (seq (recent toggles q t))]
                                [q (vec ts)])))
                   toggles)]
    (update kept p (fnil conj []) t)))

(defn- burnt-out? [toggles p t extra]
  (>= (+ (count (recent toggles p t)) (long extra)) max-toggles))

(defn- checked [chunks p {:keys [tick will-tick?]}]
  (let [st (chunk/at chunks p)
        ty (block/block-of st)]
    (when (and (= (lit? st) (powered? chunks p st))
               (not (and will-tick? (will-tick? p ty))))
      [[:tick p (+ (long tick) toggle-delay) 0 ty]])))

(defn- out-again [p t]
  (fn [chunks]
    (let [st (chunk/at chunks p)]
      [[p st [[:event :redstone-torch-burnout]]]
       [:tick p (+ (long t) restart-delay) 0 (block/block-of st)]])))

(defn- due [chunks p {:keys [tick torch-toggles]}]
  (let [st (chunk/at chunks p)
        on (powered? chunks p st)]
    (cond
      (lit? st)
      (when on
        (cond-> [[p (block/with st :lit :false) [[:toggle tick]] update/all]]
          (burnt-out? torch-toggles p tick 1) (conj (out-again p tick))))
      (and (not on) (not (burnt-out? torch-toggles p tick 0)))
      [[p (block/with st :lit :true) nil update/all]])))

(defn- shaped [chunks p {:keys [dim tick old side] :as ctx}]
  (when (= :neighbor ((:wake support/rule) chunks dim tick p old side))
    ((:reshape support/rule) chunks p ctx)))

(def rule
  "The block rule of the redstone torch. It is held as attached blocks
  are."
  (assoc support/rule
         :name :torch
         :match? (fn [_chunks st _p] (torch? st))
         :pass :both
         :place? false
         :reach 2
         :wake (constantly :neighbor)
         :reshape (fn [chunks p ctx]
                    (case (:pass ctx)
                      :neighbor (checked chunks p ctx)
                      :shape (shaped chunks p ctx)))
         :due due))
