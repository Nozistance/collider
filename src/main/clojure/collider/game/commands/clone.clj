(ns collider.game.commands.clone
  "The cells /clone copies and the order it sets them in."
  (:require [collider.cell :as cell]
            [collider.game.block.blockentity :as be]
            [collider.game.schedule :as schedule]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(defn- state-at ^long [lv [_ y _ :as p]]
  (if (chunk/in-level? lv (long y))
    (chunk/at (:chunks lv) p)
    0))

(defn- shifted [p off] (mapv + p off))

(defn- kind [e ^long st]
  (cond
    e :entities
    (not (or (block/solid-render? st) (block/full-cube? st))) :other
    :else :solid))

(defn- listed
  "Returns a step that sorts one source cell for the copy and for the
  clear."
  [from to off test]
  (fn [acc p]
    (let [st (state-at from p)
          e (be/at from p)]
      (if (and test (not (test st e)))
        acc
        (let [q (shifted p off)
              k (kind e st)
              info [q st e (state-at to q)]]
          (-> (update acc k conj info)
              (update (if (= :other k) :front :back) conj p)))))))

(defn- cells [[[x1 x2] [y1 y2] [z1 z2]]]
  (for [z (range z1 (inc (long z2)))
        y (range y1 (inc (long y2)))
        x (range x1 (inc (long x2)))]
    [x y z]))

(def ^:private lists
  {:solid [] :entities [] :other [] :front () :back []})

(defn- barrier [] (block/state :barrier))

(defn- cleared
  "Returns the ops that a move runs on the source."
  [clear strict?]
  (let [b (barrier)
        flags (if strict? update/strict update/all)]
    (-> []
        (into (map (fn [p] [:set [p b] update/strict])) clear)
        (into (map (fn [p] [:set [p 0] flags])) clear))))

(defn- placed
  "Returns the ops that a clone runs on the destination."
  [all es strict?]
  (let [b (barrier)
        flags (if strict? update/strict update/clients)
        back (rseq all)
        walled (fn [[q]] [:set [q b] update/strict])]
    (cond-> (-> []
                (into (map walled) back)
                (into (map (fn [[q st]] [:put [q st] flags])) all)
                (into (map (fn [[q st]] [:set [q st] flags])) es))
      (not strict?)
      (into (map (fn [[q _ _ old]] [:tell q old])) back))))

(defn- ticks-copied [from box off]
  (let [[[x1 x2] [y1 y2] [z1 z2]] box
        in? (fn [id]
              (let [[x y z] (cell/unpack id)]
                (and (<= x1 x x2) (<= y1 y y2) (<= z1 z z2))))
        moved #(cell/pack (shifted (cell/unpack %) off))]
    (schedule/copied (:block-ticks from) in? moved)))

(defn plan
  "Returns the ops, block entities and block ticks that /clone copies
  between two levels.
  box holds the sorted source bounds and off the shift to the
  destination. test, when given, takes the state and block entity of
  a source cell."
  [from to box off test mode strict?]
  (let [r (reduce (listed from to off test) lists (cells box))
        all (-> (:solid r) (into (:entities r)) (into (:other r)))
        clear (into (vec (:front r)) (:back r))]
    {:clear (when (= "move" mode) (cleared clear strict?))
     :place (placed all (:entities r) strict?)
     :entities (mapv (fn [[q _ e]] [q e]) (:entities r))
     :ticks (ticks-copied from box off)}))
