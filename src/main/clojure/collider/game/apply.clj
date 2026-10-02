(ns collider.game.apply
  "The application of deltas to the world and to its levels."
  (:require [clojure.core.reducers :as r]
            [collider.data.long-map :as lm]
            [collider.game.areas :as areas]
            [collider.game.delta :as delta]
            [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.input :as input]
            [collider.game.level :as level]
            [collider.game.player :as player]
            [collider.game.schema :as schema]
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
  (try (when (input/heeded? w d)
         (let [w' (event w d)] [w' (input/heard acc w w' d)]))
       (catch Throwable t (dropped! #'event d t))))

(defn- applied-input [world input]
  (loop [w world acc input/unheard xs (seq input)]
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

(defn- plugin-delta [w delta]
  (if-let [f (get (:deltas (:hooks w)) (nth delta 0))]
    (f w delta)
    w))

(defn- apply-world-delta [w delta]
  (let [tag (nth delta 0)]
    (if-let [f (get delta/world-apply tag)]
      (f w delta)
      (if-let [g (get delta/entity-apply tag)]
        (level/update-entity w (nth delta 1) #(g (:tick w) % delta))
        (plugin-delta w delta)))))

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

(defn entity
  "Returns entity e after its deltas ds of tick t, in order."
  [t e ds]
  (entity-folded t e ds))

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
      (reduce step (transient (lm/range entities lo hi))
              (subvec v a b)))))

(defn- leaves [^long n]
  (vec (range (quot (+ n (dec fold-leaf)) fold-leaf))))

(defn- folded-entities [w entities by-eid]
  (let [step (stepped entities (:tick w))
        n (count by-eid)]
    (if (< n fold-leaf)
      (persistent! (reduce step (transient entities) by-eid))
      (let [v (vec by-eid)
            leaf #(lm/merge %1 (leaf-of step entities v %2))]
        (r/fold 1 (r/monoid lm/merge lm/long-map) leaf (leaves n))))))

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
        b (update (:type b) (fnil conj (lm/long-set)) eid)))))

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

(defn- inhabited? [world dim]
  (not (deltas/vacant? (:entities (get (:levels world) dim)))))

(defn- homes [world dim]
  (when (some #(and (not (identical? % dim)) (inhabited? world %))
              schema/dims)
    (let [es (:entities (get (:levels world) dim))]
      (fn [eid]
        (when-not (contains? es eid) (level/dim-of world eid))))))

(defn- stray-dim [home delta]
  (when (:by-eid (get delta/registry (nth delta 0)))
    (home (nth delta 1))))

(defn- strays? [home ^Deltas d]
  (or (some #(home (key %)) (deltas/entities-of d))
      (some #(stray-dim home %) (deltas/world-of d))))

(defn- part [ws es]
  (assoc deltas/empty-deltas
    :world (vec ws) :entities (into (lm/long-map) es)))

(defn- rehomed [home ^Deltas d]
  (let [ws (group-by #(stray-dim home %) (deltas/world-of d))
        es (group-by #(home (key %)) (deltas/entities-of d))
        away (disj (into (set (keys ws)) (keys es)) nil)]
    [(assoc (part (ws nil) (es nil))
       :out (deltas/out-of d) :input (deltas/input-of d))
     (into [] (keep #(when (away %) [% (part (ws %) (es %))]))
           schema/dims)]))

(defn- routed [world dim ^Deltas d]
  (let [home (homes world dim)]
    (if (and home (strays? home d)) (rehomed home d) [d nil])))

(defn- handoffs-of [^Deltas d]
  (into [] (keep #(when (identical? :level-deltas (nth % 0))
                    [(nth % 1) (nth % 2)]))
        (deltas/world-of d)))

(defn- crossed [world from [_ eid dim pos yaw pitch]]
  (if-let [e (get-in world [:levels from :entities eid])]
    (-> world
        (update-in [:levels from :entities] dissoc eid)
        (assoc-in [:levels dim :entities eid]
                  (player/arrived e (:tick world) pos yaw pitch)))
    world))

(defn- noted [ds dim d]
  (update ds dim (fnil deltas/merge deltas/empty-deltas) d))

(defn- own [[world ds] dim ^Deltas d]
  [(if (deltas/inert? d)
     world
     (let [lv (apply-level (get (:levels world) dim) d)]
       (level/with-level world dim lv)))
   (noted ds dim d)])

(defn- crossing [acc from ^Deltas d]
  (reduce (fn [[world ds :as acc] x]
            (if (identical? :change-dimension (nth x 0))
              [(crossed world from x)
               (noted ds (nth x 2) (deltas/of-vec [x]))]
              acc))
          acc (deltas/world-of d)))

(declare taken)

(defn- handed [acc [dim sub]]
  (taken acc dim (deltas/with-dim (deltas/of-vec sub) dim)))

(defn- taken [acc dim d]
  (if (or (nil? d) (identical? deltas/empty-deltas d))
    acc
    (let [[d strays] (routed (nth acc 0) dim d)
          acc (reduce #(taken %1 (nth %2 0) (nth %2 1))
                      (own acc dim d) strays)
          acc (reduce handed acc (handoffs-of d))]
      (crossing acc dim d))))

(defn in
  "Returns world with deltas d applied to its level dim.
  The shared keys the level changed go to the top of world and to its
  other levels. The deltas of an entity in another level, those d
  hands to another level and the players that change dimension go
  where they belong, in that order. Given ds, the deltas each level
  took so far by dimension, returns [world ds] with d noted where it
  applied."
  ([world dim d]
   (nth (in (level/synced world) dim (deltas-of d) {}) 0))
  ([world dim d ds]
   (let [acc (taken [world ds] dim d)]
     (when delta/validate? (level/shared-kept! (nth acc 0)))
     acc)))

(defn entered
  "Returns world with the input deltas d of level dim.
  What the last input of the level left behind is dropped first."
  [world dim d]
  (if (and (deltas/inert? d)
           (nil? (get-in world [:levels dim :input])))
    world
    (let [lv (dissoc (get (:levels world) dim) :input)]
      (level/with-level world dim (apply-level lv d)))))

(defn deltas
  "Returns the world after ds and ds as applied.
  A whole world takes ds in its overworld."
  [world ds]
  (let [d (deltas-of ds)]
    [(if (contains? world :levels)
       (in world :overworld d)
       (apply-level world d))
     d]))

(defn- authored [ds ev]
  (let [eid (nth ev 1 nil)]
    (if (integer? eid) (delta/authored ds eid :player) ds)))

(defn- event-deltas [f w x ev]
  (try (vec (authored (f w x) ev))
       (catch Throwable t (dropped! f ev t) [])))

(defn fold-events
  "Returns the deltas f gives for each event in order.
  Each event sees the world after the events and slot events
  before it. An event that f fails on gives no deltas. The player
  of an event is the author of the block changes it makes."
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

(defn then
  "Returns [world' d'] after deltas ds: world with ds applied and
  the deltas d with ds added."
  [[world d] ds]
  (let [x (deltas/of-vec (vec ds))]
    [(if (deltas/inert? x) world (first (deltas world x)))
     (deltas/merge d x)]))
