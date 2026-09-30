(ns collider.game.apply
  "The application of deltas to the world and to its levels."
  (:require [clojure.core.reducers :as r]
            [clojure.data.int-map :as i]
            [collider.game.areas :as areas]
            [collider.game.delta :as delta]
            [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.level :as level]
            [collider.game.player :as player]
            [collider.log :as log])
  (:import (collider.game.deltas.record Deltas)))

(set! *warn-on-reflection* true)

(def ^:private ^:const fold-leaf 64)

(defn- unchanged [w _]
  w)

(defn event
  "Returns the world after one input delta."
  [world delta]
  ((get delta/input-apply (nth delta 0) unchanged) world delta))

(defn dropped!
  "Logs that event ev failed in unit f and is dropped."
  [f ev ^Throwable t]
  (let [unit (log/name-of f)
        msg (str "event " (pr-str ev) " failed in " unit
                 ", the event is dropped")]
    (log/failure! unit msg t)))

(defn- heard-event [w acc d]
  (try (when (player/heeded? w d)
         (let [w' (event w d)] [w' (player/heard acc w w' d)]))
       (catch Throwable t (dropped! #'event d t))))

(defn- applied-input [world input]
  (loop [w world acc player/unheard xs (seq input)]
    (if-let [d (first xs)]
      (if-let [[w' acc'] (heard-event w acc d)]
        (recur w' acc' (next xs))
        (recur w acc (next xs)))
      (assoc w :input acc))))

(defn heeded
  "Returns the input deltas d of level dim that the world heeds."
  [world dim d]
  (if-let [h (get-in world [:levels dim :input :heeded])]
    (assoc d :input h)
    d))

(defn- apply-entity-delta [tick e delta]
  ((get delta/entity-apply (nth delta 0)) tick e delta))

(defn entities
  "Returns the world with only the entity deltas of ds folded in."
  [world ds]
  (let [one (fn [e d] (apply-entity-delta (:tick world) e d))
        step (fn [w d]
               (if (contains? delta/entity-apply (nth d 0))
                 (level/update-entity w (nth d 1) one d)
                 w))]
    (reduce step world ds)))

(defn- apply-world-delta [w delta]
  (let [tag (nth delta 0)]
    (if-let [f (get delta/world-apply tag)]
      (f w delta)
      (if-let [g (get delta/entity-apply tag)]
        (level/update-entity w (nth delta 1) #(g (:tick w) % delta))
        w))))

(defn- merged-in [e m] (if m (entity/merged e m) e))

(defn- kept-mdata? [e m]
  (or (some? (:kept-mdata e)) (contains? m :kept-mdata)))

(defn- merge-of
  "Returns delta d as a map to merge into entity e, or nil.
  It is nil when d does more than set keys under the pending merge m."
  [e m d]
  (case (nth d 0)
    :merge-entity (nth d 2)
    :track (when-not (kept-mdata? e m) {:track (nth d 2)})
    nil))

(defn- entity-folded [t e ds]
  (loop [e e m nil ds (seq ds)]
    (if ds
      (let [d (first ds)]
        (if-let [dm (merge-of e m d)]
          (recur e (if m (merge m dm) dm) (next ds))
          (recur (apply-entity-delta t (merged-in e m) d) nil
                 (next ds))))
      (merged-in e m))))

(defn- stepped [entities t]
  (fn [m [eid ds]]
    (if-let [e (get entities eid)]
      (assoc! m eid (entity-folded t e ds))
      m)))

(defn- eid-at ^long [v ^long k] (long (key (nth v k))))

(defn- leaf-of [step entities v ^long j]
  (let [n (count v)
        a (* j fold-leaf)
        b (min n (+ a fold-leaf))
        lo (if (zero? j) Long/MIN_VALUE (eid-at v a))
        hi (if (= b n) Long/MAX_VALUE (dec (eid-at v b)))]
    (persistent!
      (reduce step (transient (i/range entities lo hi))
              (subvec v a b)))))

(defn- leaves [^long n]
  (vec (range (quot (+ n (dec fold-leaf)) fold-leaf))))

(defn- folded-entities [w entities by-eid]
  (let [step (stepped entities (:tick w))
        n (count by-eid)]
    (if (< n fold-leaf)
      (persistent! (reduce step (transient entities) by-eid))
      (let [v (vec by-eid)
            leaf #(i/merge %1 (leaf-of step entities v %2))]
        (r/fold 1 (r/monoid i/merge i/int-map) leaf (leaves n))))))

(defn- deltas-of [ds]
  (if (instance? Deltas ds) ds (deltas/of-vec ds)))

(defn- untyped [types t eid]
  (let [s (disj (get types t) eid)]
    (if (seq s) (assoc types t s) (dissoc types t))))

(defn- retyped [types e0 e1 eid]
  (let [a (get e0 eid) b (get e1 eid)]
    (if (and a b (identical? (:type a) (:type b)))
      types
      (cond-> types
        a (untyped (:type a) eid)
        b (update (:type b) (fnil conj (i/int-set)) eid)))))

(defn- retyped-index [types w w' eids]
  (let [e0 (:entities w) e1 (:entities w')]
    (if (or (nil? types) (identical? e0 e1))
      types
      (when eids (reduce #(retyped %1 e0 e1 %2) types eids)))))

(defn- touched [w w' delta]
  (let [tag (nth delta 0)]
    (if-let [f (:eids (get delta/registry tag))]
      (f w w' delta)
      (when (contains? delta/entity-apply tag) []))))

(defn- world-step [[w removes types] delta]
  (if (identical? :remove-entity (nth delta 0))
    [w (conj removes (nth delta 1)) types]
    (let [w' (apply-world-delta w delta)]
      [w' removes
       (retyped-index types w w' (touched w w' delta))])))

(defn- input-eids [inp]
  (into [] (keep #(let [x (nth % 1 nil)] (when (integer? x) x))) inp))

(defn- checked [es types]
  (when (and types (not= types (level/by-type es)))
    (throw (ex-info "the type index strayed from the entities"
                    {:index types :entities (level/by-type es)})))
  types)

(defn- typed [w types]
  (let [es (:entities w)
        types (if delta/validate? (checked es types) types)]
    (if (and types (level/types-by w))
      w
      (level/with-types w (or types (level/by-type es))))))

(defn- folded-in [w by-eid]
  (if (deltas/vacant? by-eid)
    w
    (assoc w :entities (folded-entities w (:entities w) by-eid))))

(defn- apply-level [lv ds]
  (let [^Deltas d (deltas-of ds)
        ws (deltas/world-of d)
        init [lv [] (level/types-by lv)]
        [lv1 removes types] (reduce world-step init ws)
        inp (deltas/input-of d)
        applied (if (seq inp) (applied-input lv1 inp) lv1)
        types (retyped-index types lv1 applied (input-eids inp))
        folded (folded-in applied (deltas/entities-of d))
        quit (reduce player/quit folded removes)
        types (retyped-index types folded quit removes)]
    (areas/cache-active-chunks (typed quit types))))

(defn applied-in
  "Returns world with ds in level dim, and that level.
  lv is the level dim of world."
  [world dim lv ds]
  (let [lv' (apply-level lv ds)]
    [(level/with-level world dim lv') lv']))

(defn in
  "Returns world with ds folded into its level dim."
  [world dim ds]
  (let [lv (apply-level (level/level world dim) ds)]
    (level/with-level world dim lv)))

(defn- crossed [world from [_ eid dim pos yaw pitch]]
  (if-let [e (get-in world [:levels from :entities eid])]
    (-> world
        (update-in [:levels from :entities] dissoc eid)
        (assoc-in [:levels dim :entities eid]
                  (player/arrived e (:tick world) pos yaw pitch)))
    world))

(defn changes-of
  "Returns the dimension changes among the world deltas of d."
  [^Deltas d]
  (filterv #(identical? :change-dimension (nth % 0))
           (deltas/world-of d)))

(defn handoffs-of
  "Returns the deltas d hands to other levels, as [dim deltas]."
  [^Deltas d]
  (into [] (keep #(when (identical? :level-deltas (nth % 0))
                    [(nth % 1) (nth % 2)]))
        (deltas/world-of d)))

(defn cross
  "Returns world with the players that change dimension in d moved.
  Each keeps its eid in the new level and knows no chunk and no entity
  there yet."
  [world from changes]
  (reduce #(crossed %1 from %2) world changes))

(defn entered
  "Returns world with the input deltas of level dim, and that level.
  Returns nil when it stays as it was. What the last input of the
  level left behind is dropped first."
  [world dim ds]
  (if (and (deltas/inert? ds)
           (nil? (get-in world [:levels dim :input])))
    [world nil]
    (let [lv (dissoc (level/level world dim) :input)
          lv' (apply-level lv ds)]
      [(level/with-level world dim lv') lv'])))

(defn deltas
  "Returns the world after ds and ds as applied.
  A whole world takes ds in its overworld."
  [world ds]
  (let [d (deltas-of ds)]
    [(if (contains? world :levels)
       (in world :overworld d)
       (apply-level world d))
     d]))

(defn- event-deltas [f w x ev]
  (try (vec (f w x))
       (catch Throwable t (dropped! f ev t) [])))

(defn fold-events
  "Returns the deltas f gives for each event in order.
  Each event sees the world after the events and slot events
  before it. An event that f fails on gives no deltas."
  ([world events f] (fold-events world events f identity))
  ([world events f event-of]
   (loop [w world evs (seq events) acc []]
     (if-not evs
       acc
       (let [x (first evs) more (next evs) ev (event-of x)
             w (entities w (player/slot-part w ev))
             ds (event-deltas f w x ev)
             step? (and more (seq ds))]
         (recur (if step? (first (deltas w ds)) w)
                more (into acc ds)))))))

