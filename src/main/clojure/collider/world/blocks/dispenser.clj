(ns collider.world.blocks.dispenser
  "A dispenser or a dropper arms when it or the block above it
  is powered, and fires four ticks later."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.signal :as signal]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(def ^:private ^:const delay-ticks 4)

(defn- launcher? [^long st]
  (let [t (block/type-of st)]
    (or (= :dispenser t) (= :dropper t))))

(defn- triggered? [^long st]
  (= :true (:triggered (block/props-of st))))

(defn- powered? [chunks p]
  (or (signal/has-neighbor-signal? chunks p)
      (signal/has-neighbor-signal? chunks (update p 1 inc))))

(defn- changes [chunks p {:keys [tick]}]
  (let [st (chunk/at chunks p)
        on (powered? chunks p)
        was (triggered? st)]
    (cond
      (and on (not was))
      [[p (block/with st :triggered :true) [] update/clients]
       [:tick p (+ (long tick) delay-ticks) 0 (block/block-of st)]]
      (and (not on) was)
      [[p (block/with st :triggered :false) [] update/clients]])))

(defn- fired [_chunks p _ctx]
  [[:dispense p]])

(def rule
  "The block rule of a dispenser or a dropper. A rising signal
  arms it, and it fires four ticks later."
  {:name    :dispenser
   :match?  (fn [_ st _] (launcher? st))
   :pass    :neighbor
   :reach   0
   :place?  false
   :wake    (constantly :neighbor)
   :reshape changes
   :due     fired})
