(ns collider.game.using
  "Items a player holds in use: how long a use lasts, and the
  instrument of a horn."
  (:require [collider.data :as data]
            [collider.data.pack :refer [kw]]
            [collider.game.stack :as stack]))

(set! *warn-on-reflection* true)

(def ^:private ^:const spyglass-ticks 1200)

(defn- sound-id [v]
  (kw (if (map? v) (get v "sound_id") v)))

(defn- instrument [v]
  {:sound (sound-id (get v "sound_event"))
   :duration (double (get v "use_duration"))
   :range (double (get v "range"))})

(def ^:private ^:table instruments
  (delay (into {}
               (map (fn [[id v]] [(kw id) (instrument v)]))
               (data/pack "instrument"))))

(defn- direct [{:keys [sound use-duration range]}]
  (when (keyword? sound)
    {:sound sound :duration (double use-duration)
     :range (double range)}))

(defn instrument-of
  "Returns the sound, duration and range of the instrument of stack.
  Returns nil when it has none."
  [stack]
  (let [v (stack/component stack :instrument)]
    (if (map? v) (direct (:direct v)) (get @instruments v))))

(defn instrument-ticks
  "Returns the ticks a use of instrument lasts."
  ^long [ins]
  (let [d (double (float (:duration ins)))]
    (long (Math/floor (double (float (* d 20.0)))))))

(defn- consume-ticks [c]
  (long (* 20.0 (double (:seconds c)))))

(defn ticks
  "Returns the ticks a use of stack lasts, or nil when a use of it
  does not last."
  [stack]
  (let [item (:item stack)]
    (if-let [c (get-in (data/items) [item :consumable])]
      (consume-ticks c)
      (case item
        :spyglass spyglass-ticks
        :goat-horn (some-> (instrument-of stack) instrument-ticks)
        nil))))
