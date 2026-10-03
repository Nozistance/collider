(ns collider.game.block.stonecutter
  "Stonecutter recipes and its menu."
  (:require [collider.data :as data]
            [collider.game.block.menu :as menu]))

(set! *warn-on-reflection* true)

(def ^:private ^:table cut-recipes
  (delay (:stonecutting (data/recipes))))

(defn cuts
  [stack]
  (if (nil? stack)
    []
    (filterv (fn [r] (some #(= % (:item stack)) (:in r)))
             @cut-recipes)))

(defn- cuts-input? [stack]
  (boolean (seq (cuts stack))))

(defn cut-result
  "Returns the stack recipe number selected cuts from stack."
  [stack ^long selected]
  (let [rs (cuts stack)]
    (when (and (not (neg? selected)) (< selected (count rs)))
      (let [out (:out (nth rs selected))]
        {:item (:item out) :count (long (:count out 1))}))))

(defn changed
  [m inv]
  (let [item (:item (get inv 0))]
    (if (= item (:input-item m))
      m
      (assoc m :input-item item :selected -1))))

(defn- quick [v inv slot]
  (let [i (menu/index-of v slot)]
    (cond
      (= 1 i) (menu/span v 2 38 true)
      (= 0 i) (menu/span v 2 38 false)
      (cuts-input? (get inv slot)) (menu/span v 0 1 false)
      (< 1 i 29) (menu/span v 29 38 false)
      :else (menu/span v 2 29 false))))

(defn- derive-result [inv ^long selected]
  (if-let [r (cut-result (get inv 0) selected)]
    (assoc inv 1 r)
    (dissoc inv 1)))

(defn layout
  [m]
  (let [place? (fn [slot _] (not= 1 (long slot)))
        base (menu/slots-layout 2 place?)
        v (:visible base)
        selected (long (:selected m))]
    (assoc base
      :result 1
      :no-gather #{1}
      :stat {:slot 1 :by :taken}
      :quick (fn [inv slot] (quick v inv slot))
      :on-take (fn [inv] (menu/shrink inv 0))
      :derive (fn [inv] (derive-result inv selected)))))
