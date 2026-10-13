(ns collider.game.apply
  "The application of deltas to the world and to its levels."
  (:require [clojure.core.reducers :as r]
            [collider.data.long-map :as lm]
            [collider.game.areas :as areas]
            [collider.game.delta :as delta]
            [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.entity.sections :as sections]
            [collider.game.input :as input]
            [collider.game.level :as level]
            [collider.game.player :as player]
            [collider.game.schema :as schema]
            [collider.log :as log]
            [collider.parallel :as par])
  (:import (collider.game.deltas.record Deltas)))

(set! *warn-on-reflection* true)

(defn- unchanged [w _]
  w)

(defn event
  "Returns the world after one input delta."
  [world delta]
  ((get delta/input-apply (nth delta 0) unchanged) world delta))

(defn dropped!
  "Logs that event ev failed in unit f and is dropped."
  [f ev ^Throwable t]
  (let [msg #(str "event " (pr-str ev) " failed in " %
                  ", the event is dropped")]
    (log/unit-failed! nil f t msg)))

(defn- heard-event [w acc d]
  (try (when (input/heeded? w d)
         (let [w' (event w d)] [w' (input/heard acc w d)]))
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

(defn entity
  "Returns entity e after its deltas ds of tick t, in order."
  [t e ds]
  (loop [e e m nil ds (seq ds)]
    (if ds
      (let [d (first ds)]
        (if-let [dm (merge-of e m d)]
          (recur e (if m (merge m dm) dm) (next ds))
          (recur (apply-entity-delta t (merged-in e m) d) nil
                 (next ds))))
      (merged-in e m))))

(defn- stepped [entities t]
  (fn [[m ys :as acc] [eid ds]]
    (if-let [e (get entities eid)]
      (let [e' (entity t e ds)
            y (sections/crossing eid e e')]
        [(assoc! m eid e') (if y (conj! ys y) ys)])
      acc)))

(defn- eid-at ^long [v ^long k] (long (key (nth v k))))

(defn- stepped-all [step es entries]
  (let [init [(transient es) (transient [])]
        [m ys] (reduce step init entries)]
    [(persistent! m) (persistent! ys)]))

(defn- leaf-of [step entities v ^long j]
  (let [n (count v)
        a (* j par/fold-leaf)
        b (min n (+ a par/fold-leaf))
        lo (if (zero? j) Long/MIN_VALUE (eid-at v a))
        hi (if (= b n) Long/MAX_VALUE (dec (eid-at v b)))]
    (stepped-all step (lm/range entities lo hi) (subvec v a b))))

(defn- leaves [^long n]
  (vec (range (quot (+ n (dec par/fold-leaf)) par/fold-leaf))))

(defn- joined
  ([] [(lm/long-map) []])
  ([[a ya] [b yb]]
   [(lm/merge a b) (par/joined ya yb)]))

(defn- folded-entities [w entities by-eid]
  (let [step (stepped entities (:tick w))
        n (count by-eid)]
    (if (< n par/fold-leaf)
      (stepped-all step entities by-eid)
      (let [v (vec by-eid)
            leaf #(joined %1 (leaf-of step entities v %2))]
        (r/fold 1 joined leaf (leaves n))))))

(defn- deltas-of [ds]
  (if (instance? Deltas ds) ds (deltas/of-vec ds)))

(defn- world-step [[w removes] delta]
  (if (identical? :remove-entity (nth delta 0))
    [w (conj removes [(nth delta 1) (nth delta 2 nil)])]
    [(apply-world-delta w delta) removes]))

(defn- checked! [w]
  (let [types (level/types-by w) built (level/by-type (:entities w))]
    (when (not= types built)
      (throw (ex-info "the type index strayed from the entities"
                      {:index types :entities built})))))

(defn- typed [w]
  (let [w (level/typed w)]
    (when delta/validate? (checked! w))
    w))

(defn- strayed! [what idx built]
  (when (not= idx built)
    (throw (ex-info (str "the " what " index strayed")
                    {:index idx :built built}))))

(defn- indexed [lv]
  (let [lv (level/with-players (sections/indexed lv))]
    (when delta/validate?
      (strayed! "section" (sections/index lv)
                (sections/of (seq (:entities lv)))))
    lv))

(defn- folded-in [w by-eid]
  (if (lm/empty? by-eid)
    w
    (let [w (sections/indexed w)
          [es ys] (folded-entities w (:entities w) by-eid)]
      (-> (sections/moved w es ys)
          (assoc :entities es)))))

(defn- arrived [lv lv']
  (let [es (:entities lv')
        es' (sections/arrivals (:entities lv) es (:tick lv))]
    (if (identical? es es') lv' (assoc lv' :entities es'))))

(defn- apply-level [lv ds]
  (let [^Deltas d (deltas-of ds)
        ws (deltas/world-of d)
        [lv1 removes] (reduce world-step [lv []] ws)
        inp (deltas/input-of d)
        applied (if (seq inp) (applied-input lv1 inp) lv1)
        folded (folded-in applied (deltas/entities-of d))
        gone (fn [w [eid why]] (player/quit w eid why))
        quit (arrived lv (reduce gone folded removes))]
    (indexed (areas/with-areas (typed quit)))))

(defn- inhabited? [world dim]
  (not (lm/empty? (:entities (get (:levels world) dim)))))

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
    (let [t (:tick world)
          e' (assoc (player/arrived e t pos yaw pitch)
                    :arrived (sections/appeared t))]
      (-> world
          (update-in [:levels from :entities] dissoc eid)
          (assoc-in [:levels dim :entities eid] e')))
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
  The shared keys the level changed go to the world and to its other
  levels. Deltas of other levels and changes of dimension go where
  they belong. Given ds, the deltas each level took so far, returns
  the world and ds with d noted where it applied."
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

(defn applied
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

(defn- event-step [f event-of w x more]
  (let [ev (event-of x)
        w (entities w (player/slot-part w ev))
        ds (event-deltas f w x ev)]
    [(if (and more (seq ds)) (first (applied w ds)) w) ds]))

(defn- folded [world events f event-of]
  (loop [w world evs (seq events) acc []]
    (if-not evs
      acc
      (let [more (next evs)
            [w ds] (event-step f event-of w (first evs) more)]
        (recur w more (into acc ds))))))

(defn fold-events
  "Returns the deltas f gives for each event in order.
  Each event sees the world after the events and slot events
  before it. An event that f fails on gives no deltas. The player
  of an event is the author of the block changes it makes."
  ([world events f] (fold-events world events f identity))
  ([world events f event-of] (folded world events f event-of)))

(defn- level-of [world ev]
  (let [eid (nth ev 1 nil)]
    (or (when (integer? eid) (level/dim-of world eid)) :overworld)))

(defn- played [dim ds]
  (let [v (deltas/as-vec (deltas/with-dim (deltas-of ds) dim))
        fx? #(identical? :fx (nth % 0))
        own (filterv (complement fx?) v)]
    (into (if (seq own) [[:level-deltas dim own]] []) (filter fx?) v)))

(def ^:private crossing-tags #{:level-deltas :change-dimension})

(defn- crosses? [ds]
  (some #(contains? crossing-tags (nth % 0)) ds))

(defn- kept [w dim lv]
  (if lv (level/with-level w dim (dissoc lv :server)) w))

(defn- level-at [w dim lv at]
  (if (and lv (identical? at dim))
    [w lv]
    (let [w (kept w dim lv)]
      [w (assoc (get (:levels w) at) :server w)])))

(defn- event-ds [f lv ev]
  (let [sp (player/slot-part lv ev)]
    (into (vec sp) (event-deltas f (entities lv sp) ev ev))))

(defn- went [w at lv ds]
  (cond (empty? ds) [w at lv]
        (crosses? ds) [(nth (in (kept w at lv) at (deltas-of ds) {}) 0)]
        :else [w at (apply-level lv (deltas-of ds))]))

(defn fold-world
  "Returns the deltas of the events of the tick in the order they
  came, each in the level its player stands in when it comes and
  seeing the world after the events before it. The deltas of an
  event, its slot deltas first, go to its level, and its effects
  stay in order. f takes the level and the event."
  [world events f]
  (loop [[w dim lv] [world] evs (seq events) acc []]
    (if-let [ev (first evs)]
      (let [at (level-of w ev)
            [w lv] (level-at w dim lv at)
            ds (event-ds f lv ev)]
        (recur (went w at lv ds) (next evs) (into acc (played at ds))))
      acc)))

(defn then
  "Returns world with deltas ds applied, and deltas d with ds added."
  [[world d] ds]
  (let [x (deltas/of-vec (vec ds))]
    [(if (deltas/inert? x) world (first (applied world x)))
     (deltas/merge d x)]))
