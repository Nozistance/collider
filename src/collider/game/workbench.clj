(ns collider.game.workbench
  "Stonecutter and loom: menus that hold no storage. Their slots are a few
   inputs plus a result assembled from a recipe or a pattern the player picks
   with a button (StonecutterMenu, LoomMenu)."
  (:require [collider.data :as data])
  (:import (java.util List)))

(set! *warn-on-reflection* true)

(def ^:private cut-recipes (delay (:stonecutting @data/recipes)))

(defn- of [item] (data/tag-values "item" item))

(defn cuts
  "SelectableRecipe.SingleInputSet.selectByInput: every stonecutting recipe whose
   ingredient accepts the stack, in datapack order."
  [stack]
  (if (nil? stack)
    []
    (filterv (fn [r] (some #(= % (:item stack)) (:in r))) @cut-recipes)))

(defn cuts-input?
  "SingleInputSet.acceptsInput, as StonecutterMenu.quickMoveStack asks it."
  [stack]
  (boolean (seq (cuts stack))))

(defn cut-result
  "StonecutterMenu.setupResultSlot: assemble() ignores the input and creates the
   recipe result."
  [stack ^long selected]
  (let [rs (cuts stack)]
    (when (and (not (neg? selected)) (< selected (count rs)))
      (let [out (:out (nth rs selected))]
        {:item (:item out) :count (long (:count out 1))}))))

(defn cut-changed
  "StonecutterMenu.slotsChanged: only a change of the input item resets the list
   and clears the selection."
  [m inv]
  (let [item (:item (get inv 0))]
    (if (= item (:input-item m))
      m
      (assoc m :input-item item :selected -1))))

(def ^:private no-item-required
  (delay (vec (data/tag-values "banner_pattern" "no_item_required"))))

(def ^:private banner-items (delay (set (of "banners"))))
(def ^:private loom-dyes (delay (set (of "loom_dyes"))))
(def ^:private loom-patterns (delay (set (of "loom_patterns"))))

(defn banner? [stack] (contains? @banner-items (:item stack)))

(defn dye?
  "LoomMenu.isDyeItem: in #loom_dyes and carrying the dye component."
  [stack]
  (and (contains? @loom-dyes (:item stack))
       (some? (data/dye-color (:item stack)))))

(defn pattern-item?
  "LoomMenu.isPatternItem: in #loom_patterns and carrying provides_banner_patterns."
  [stack]
  (and (contains? @loom-patterns (:item stack))
       (some? (data/pattern-tag (:item stack)))))

(defn selectable-patterns
  "LoomMenu.getSelectablePatterns: without a pattern item the whole
   #no_item_required tag, with one the tag its component provides."
  [pattern]
  (if (nil? pattern)
    @no-item-required
    (if-let [tag (data/pattern-tag (:item pattern))]
      (vec (data/tag-values "banner_pattern" tag))
      [])))

(def ^:private max-layers 6)

(defn- layers [stack] (vec (get-in stack [:components :banner-patterns])))

(defn loom-result
  "LoomMenu.setupResultSlot: one banner with the chosen pattern in the dye's
   colour appended to its banner_patterns."
  [banner dye pattern]
  (when (and banner dye pattern)
    (when-let [color (data/dye-color (:item dye))]
      (-> banner
          (assoc :count 1)
          (assoc-in [:components :banner-patterns]
                    (conj (layers banner) {:pattern pattern :color color}))))))

(defn- kept-pattern [prev ^long selected patterns]
  (cond
    (= 1 (count patterns)) 0
    (or (neg? selected) (>= selected (count prev))) -1
    :else (.indexOf ^List patterns (nth prev selected))))

(defn loom-changed
  "LoomMenu.slotsChanged: the pattern list follows the pattern slot, a single
   choice selects itself, and a banner that already wears six layers takes none."
  [m inv]
  (let [banner (get inv 0) dye (get inv 1)]
    (if-not (and banner dye)
      (assoc m :patterns [] :selected -1)
      (let [patterns (selectable-patterns (get inv 2))
            selected (long (kept-pattern (vec (:patterns m)) (long (:selected m)) patterns))]
        (assoc m :patterns patterns
               :selected (if (>= (count (layers banner)) max-layers) -1 selected))))))
