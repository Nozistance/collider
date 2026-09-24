(ns collider.world.env.attribute
  "Gameplay attributes of a dimension, like fast lava."
  (:require [clojure.string :as str]
            [collider.data :as data]))

(set! *warn-on-reflection* true)

(defn value?
  "Tests whether the attribute attr of dimension dim is on.
  An attribute the dimension does not set is off."
  [dim attr]
  (true? (get-in (data/dimension-type dim) [:attributes attr])))

(defn fast-lava? [dim] (value? dim :gameplay/fast-lava))

(defn water-evaporates? [dim]
  (value? dim :gameplay/water-evaporates))

(def ^:private sleep-when-dark
  {:can-sleep "when_dark" :can-set-spawn "always"
   :error-message {:translate "block.minecraft.bed.no_sleep"}})

(defn- rule-key [s] (keyword (str/replace s "_" "-")))

(defn bed-rule [dim]
  (let [r (get-in (data/dimension-type dim)
                  [:attributes :gameplay/bed-rule] sleep-when-dark)]
    (-> (merge {:explodes false} r)
        (update :can-sleep rule-key)
        (update :can-set-spawn rule-key))))

(defn allows? [rule dark?]
  (case rule :always true :when-dark (boolean dark?) :never false))
