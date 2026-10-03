(ns collider.world.env.attribute
  "Gameplay attributes of a dimension, like fast lava."
  (:require [clojure.string :as str]
            [collider.world.env.dimension :as dimension]))

(set! *warn-on-reflection* true)

(defn value?
  "Returns true when the attribute attr of dimension dim is on.
  An attribute that the dimension does not set is off."
  [dim attr]
  (true? (get-in (dimension/type-of dim) [:attributes attr])))

(defn fast-lava?
  "Returns true when lava flows fast in dimension dim."
  [dim]
  (value? dim :gameplay/fast-lava))

(defn water-evaporates?
  "Returns true when water placed in dimension dim evaporates."
  [dim]
  (value? dim :gameplay/water-evaporates))

(def ^:private sleep-when-dark
  {:can-sleep "when_dark" :can-set-spawn "always"
   :error-message {:translate "block.minecraft.bed.no_sleep"}})

(defn- rule-key [s] (keyword (str/replace s "_" "-")))

(defn bed-rule
  "Returns when a bed lets a player sleep and set a spawn in
  dimension dim, and whether it explodes."
  [dim]
  (let [r (get-in (dimension/type-of dim)
                  [:attributes :gameplay/bed-rule] sleep-when-dark)]
    (-> (merge {:explodes false} r)
        (update :can-sleep rule-key)
        (update :can-set-spawn rule-key))))

(defn allows?
  "Returns true when bed rule rule allows the action, given dark?."
  [rule dark?]
  (case rule :always true :when-dark (boolean dark?) :never false))
