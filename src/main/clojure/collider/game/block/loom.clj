(ns collider.game.block.loom
  "Loom patterns, results and its menu."
  (:require [collider.data :as data]
            [collider.game.block.menu :as menu]))

(set! *warn-on-reflection* true)

(defn- item-tag [tag] (data/tag-values "item" tag))

(def ^:private ^:table no-item-required
  (delay (vec (data/tag-values "banner_pattern" "no_item_required"))))

(def ^:private ^:table banner-items
  (delay (set (item-tag "banners"))))

(def ^:private ^:table loom-dyes (delay (set (item-tag "loom_dyes"))))

(def ^:private ^:table loom-patterns
  (delay (set (item-tag "loom_patterns"))))

(defn- banner? [stack] (contains? @banner-items (:item stack)))

(defn- dye? [stack]
  (and (contains? @loom-dyes (:item stack))
       (some? (data/dye-color (:item stack)))))

(defn- pattern-item? [stack]
  (and (contains? @loom-patterns (:item stack))
       (some? (data/pattern-tag (:item stack)))))

(defn selectable-patterns
  "Returns the patterns the loom offers for pattern item pattern."
  [pattern]
  (if (nil? pattern)
    @no-item-required
    (if-let [tag (data/pattern-tag (:item pattern))]
      (vec (data/tag-values "banner_pattern" tag))
      [])))

(def ^:private max-layers 6)

(defn- layers [stack]
  (vec (get-in stack [:components :banner-patterns])))

(defn- layered [banner pattern color]
  (let [layer {:pattern pattern :color color}]
    (-> banner
        (assoc :count 1)
        (assoc-in [:components :banner-patterns]
                  (conj (layers banner) layer)))))

(defn- result [banner dye pattern]
  (when (and banner dye pattern)
    (when-let [color (data/dye-color (:item dye))]
      (layered banner pattern color))))

(defn- kept-pattern [prev ^long selected patterns]
  (cond
    (= 1 (count patterns)) 0
    (or (neg? selected) (>= selected (count prev))) -1
    :else (menu/index-of patterns (nth prev selected))))

(defn- selection [m banner patterns]
  (let [prev (vec (:patterns m))
        kept (kept-pattern prev (long (:selected m)) patterns)]
    (if (>= (count (layers banner)) max-layers) -1 (long kept))))

(defn changed
  "Returns loom menu m after its slots hold inv."
  [m inv]
  (let [banner (get inv 0) dye (get inv 1)]
    (if-not (and banner dye)
      (assoc m :patterns [] :selected -1)
      (let [patterns (selectable-patterns (get inv 2))]
        (assoc m :patterns patterns
                 :selected (selection m banner patterns))))))

(defn- may-place? [slot stack]
  (case (long slot)
    0 (banner? stack)
    1 (dye? stack)
    2 (pattern-item? stack)
    3 false))

(defn- quick [v inv slot]
  (let [i (menu/index-of v slot)
        stack (get inv slot)]
    (cond
      (= 3 i) (menu/span v 4 40 true)
      (< i 3) (menu/span v 4 40 false)
      (banner? stack) (menu/span v 0 1 false)
      (dye? stack) (menu/span v 1 2 false)
      (pattern-item? stack) (menu/span v 2 3 false)
      (< 3 i 31) (menu/span v 31 40 false)
      :else (menu/span v 4 31 false))))

(defn- derive-result [inv pattern]
  (if-let [r (result (get inv 0) (get inv 1) pattern)]
    (assoc inv 3 r)
    (dissoc inv 3)))

(defn layout
  "Returns the slot layout of loom menu m."
  [m]
  (let [base (menu/slots-layout 4 may-place?)
        v (:visible base)
        patterns (vec (:patterns m))
        selected (long (:selected m))
        pattern (when (< -1 selected (count patterns))
                  (nth patterns selected))]
    (assoc base
      :result 3
      :quick (fn [inv slot] (quick v inv slot))
      :on-take (fn [inv] (-> inv (menu/shrink 0) (menu/shrink 1)))
      :derive (fn [inv] (derive-result inv pattern)))))
