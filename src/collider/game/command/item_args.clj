(ns collider.game.command.item-args
  "Item stack and item predicate arguments of commands."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.command.args :as args]
            [collider.game.command.components :as cs]
            [collider.game.command.dfu :as dfu]
            [collider.game.command.reader :as r]
            [collider.game.command.snbt :as snbt]
            [collider.game.stack :as stack]))

(set! *warn-on-reflection* true)

(defn- component-id [k]
  (str "minecraft:" (str/replace (subs (str k) 1) \- \_)))

(defn- index [registry]
  (delay (into {} (map (fn [k] [(component-id k) k]))
               (keys (get (data/registries) registry)))))

(def ^:private ^:table item-ids (index "item"))

(def ^:private ^:table component-ids (index "data_component_type"))

(def ^:private ^:table predicate-ids
  (index "data_component_predicate_type"))

(def ^:private ^:table item-tags
  (delay (into {}
               (map (fn [[t is]] [(str "minecraft:" t) (set is)]))
               (get (data/tags) "item"))))

(def ^:private unsaved
  "Components with no saved form, so commands cannot name them: they
  have no persistent codec."
  #{:creative-slot-lock :additional-trade-cost :map-post-processing})

(defn- at? [[s n :as rd] c]
  (and (r/can-read? rd) (= c (nth s n))))

(defn- skip [[s n]] [s (inc n)])

(defn- read-item [rd]
  (let [res (args/read-id rd)
        k (when-not (r/error? res) (@item-ids (first res)))]
    (cond (r/error? res) res
          k (assoc res 0 k)
          :else
          (r/error-at rd "argument.item.id.invalid" (first res)))))

(def ^:private no-component "arguments.item.component.expected")

(def ^:private unknown-component "arguments.item.component.unknown")

(defn- read-type [rd]
  (let [res (when (r/can-read? rd) (args/read-id rd))
        k (when (vector? res) (@component-ids (first res)))]
    (cond (nil? res) (r/error-at rd no-component)
          (r/error? res) res
          (and k (not (unsaved k))) (assoc res 0 k)
          :else (r/error-at rd unknown-component (first res)))))

(defn- once [seen res]
  (cond (r/error? res) res
        (seen (first res))
        (r/error "arguments.item.component.repeated"
                 (component-id (first res)))
        :else res))

(def ^:private bad-component "arguments.item.component.malformed")

(def ^:private bad-predicate "arguments.item.predicate.malformed")

(defn- read-value [k rd]
  (let [res (snbt/read-tag rd)
        [op v] (when-not (r/error? res) (cs/decode k (first res)))]
    (case op
      nil res
      :malformed (r/error-at rd bad-component (component-id k) v)
      [[k op v] (r/skip-whitespace (second res))])))

(defn- read-set [seen rd]
  (let [res (once seen (read-type rd))
        at (when-not (r/error? res)
             (r/expect (r/skip-whitespace (second res)) \=))]
    (cond (r/error? res) res
          (r/error? at) at
          :else (read-value (first res) (r/skip-whitespace at)))))

(defn- read-removal [seen rd]
  (let [res (once seen (read-type (skip rd)))]
    (if (r/error? res)
      res
      [[(first res) :remove] (r/skip-whitespace (second res))])))

(defn- next-entry [rd]
  (let [at (r/skip-whitespace (skip rd))]
    (if (r/can-read? at)
      at
      (r/error-at at no-component))))

(defn- patch-step
  "Reads one entry of patch at rd. Returns [patch rd] to go on,
  or [patch-or-nil end] when the patch ends."
  [patch rd]
  (let [rd (r/skip-whitespace rd)
        read (if (at? rd \!) read-removal read-set)
        res (read (set (map first patch)) rd)
        [e end] (when-not (r/error? res) res)
        patch (conj patch e)
        nx (when (and end (at? end \,)) (next-entry end))]
    (cond (r/error? res) [nil res :done]
          (nil? nx) [patch (r/expect end \]) :done]
          (r/error? nx) [nil nx :done]
          :else [patch nx])))

(defn- read-patch [start]
  (loop [patch [] rd (skip start)]
    (if (or (not (r/can-read? rd)) (at? rd \]))
      [patch (r/expect rd \])]
      (let [[patch rd done] (patch-step patch rd)]
        (if done [patch rd] (recur patch rd))))))

(defn- parse-stack [rd]
  (let [res (read-item rd)
        [k end] (when-not (r/error? res) res)
        [patch after] (when (and k (at? end \[)) (read-patch end))]
    (cond (r/error? res) res
          (r/error? after) after
          :else [{:item k :patch (or patch [])} (or after end)])))

(defn item-stack-arg
  "Returns the item_stack argument: an item and the changes to its
  components, in input order."
  []
  {:id "minecraft:item_stack" :parse parse-stack})

(defn- patched [s [k op v]]
  (case op
    :ok (stack/put s k v)
    :remove (stack/put s k nil)
    :raw s))

(defn item-stack
  "Returns the stack of n items that input names, or the error.
  Components kept raw are not applied."
  [{:keys [item patch]} n]
  (let [s (reduce patched {:item item :count n} patch)
        most (long (or (stack/component s :max-stack-size) 1))]
    (cond (> (long n) most)
          (r/error "arguments.item.overstacked" (data/wire item) most)
          (and (stack/has? s :max-damage) (> most 1))
          (r/error "arguments.item.malformed"
                   "Item cannot be both damageable and stackable")
          :else s)))

(defn- store!
  "Keeps error e at c when no error reached further."
  [{:keys [err]} c e]
  (vswap! err (fn [{:keys [at] :as x}]
                (cond (> (long c) (long at)) {:at c :e e}
                      (and (= c at) (nil? (:e x))) (assoc x :e e)
                      :else x)))
  nil)

(defn- ws [st p] (second (r/skip-whitespace [(:s st) p])))

(defn- lit [st p c]
  (let [q (ws st p) s (:s st)]
    (if (and (< q (count s)) (= c (nth s q)))
      (inc q)
      (->> (r/error-at [s q] "argument.literal.incorrect" (str c))
           (store! st q)))))

(defn- lookup [st p check]
  (let [q (ws st p) rd [(:s st) q] res (args/read-id rd)
        [id [_ e]] (when-not (r/error? res) res)]
    (if (or (nil? id) (= e q))
      (store! st q (r/error-at rd "argument.id.invalid"))
      (let [v (check id [(:s st) e])]
        (if (r/error? v) (store! st q v) [v e])))))

(defn- element [id rd]
  (if-let [k (@item-ids id)]
    [:item k]
    (r/error-at rd "argument.item.id.invalid" id)))

(defn- tag-check [id rd]
  (if (@item-tags id)
    [:tag id]
    (r/error-at rd "arguments.item.tag.unknown" id)))

(def ^:private count-id "minecraft:count")

(defn- component-check [id rd]
  (let [k (@component-ids id)]
    (cond (= count-id id) :count
          (and k (not (unsaved k))) k
          :else (r/error-at rd unknown-component id))))

(defn- predicate-check [id rd]
  (cond (= count-id id) :count
        (@predicate-ids id) [:predicate (@predicate-ids id)]
        (@component-ids id) [:exists (@component-ids id)]
        :else (r/error-at rd "arguments.item.predicate.unknown" id)))

(defn- item-type [st p]
  (or (lookup st p element)
      (if-let [q (lit st p \#)]
        (lookup st q tag-check)
        (when-let [q (lit st p \*)] [nil q]))))

(defn- tag-at [st p]
  (let [q (ws st p) res (snbt/read-tag [(:s st) q])]
    (if (r/error? res)
      (store! st q res)
      [(first res) (second (second res))])))

(defn- bound [x] (when (number? x) (unchecked-int x)))

(defn- not-range [tag]
  [:malformed (str "Failed to parse either."
                   " First: Not a map: " (dfu/printed tag)
                   "; Second: Not a number")])

(defn- numeric-bounds? [tag lo hi]
  (and (= (some? lo) (contains? tag :min))
       (= (some? hi) (contains? tag :max))))

(defn- swapped [lo hi]
  [:malformed (str "Swapped bounds in range: Optional[" lo
                   "] is higher than Optional[" hi "]")])

(defn- bounds
  "Returns the count test of tag: MinMaxBounds.Ints.CODEC.
  A map with fields not numbers is kept raw."
  [tag]
  (let [lo (bound (:min tag)) hi (bound (:max tag))]
    (cond (number? tag) [:ok [:count (bound tag) (bound tag)]]
          (not (map? tag)) (not-range tag)
          (not (numeric-bounds? tag lo hi)) [:ok [:raw :count tag]]
          (and lo hi (> (long lo) (long hi))) (swapped lo hi)
          :else [:ok [:count lo hi]])))

(defn- component-test [k tag]
  (if (= :count k)
    (bounds tag)
    (let [[op v] (cs/decode k tag)]
      (case op
        :ok [:ok [:equals k v]]
        :raw [:ok [:raw k v]]
        [op v]))))

(def ^:private map-predicates
  "Predicate types whose codec is a record."
  #{:damage})

(defn- record-test? [c]
  (and (vector? c)
       (or (= :exists (first c)) (map-predicates (peek c)))))

(defn- predicate-test [c tag]
  (let [[op v] (when (and (record-test? c) (not (map? tag)))
                 (dfu/not-map tag))]
    (cond (= :count c) (bounds tag)
          (= :malformed op) [op v]
          (= :predicate (first c)) [:ok [:raw (peek c) tag]]
          (map? tag) [:ok [:has (peek c)]]
          :else [:ok [:raw (peek c) tag]])))

(defn- test-id [c]
  (cond (= :count c) count-id
        (keyword? c) (component-id c)
        :else (component-id (peek c))))

(defn- valued
  "Returns [test end] of c followed by ch and a value, :cut when
  the value fails, or nil when ch is not there."
  [st [c q] ch decide k]
  (when-let [q (lit st q ch)]
    (let [[tag end] (tag-at st q)
          [op v] (when end (decide c tag))]
      (case op
        nil :cut
        :ok [{:test v} end]
        (do (store! st end (r/error-at [(:s st) end] k (test-id c) v))
            :cut)))))

(defn- presence [[c q]]
  [{:test (if (= :count c) [:count nil nil] [:has c])} q])

(defn- predicate-or-presence [st p c]
  (let [pr (lookup st p predicate-check)
        b (when pr (valued st pr \~ predicate-test bad-predicate))]
    (cond (vector? b) b
          (= :cut b) nil
          c (presence c))))

(defn- one-test [st p]
  (let [c (lookup st p component-check)
        a (when c (valued st c \= component-test bad-component))]
    (cond (vector? a) a
          (= :cut a) nil
          :else (predicate-or-presence st p c))))

(defn- term [st p]
  (or (one-test st p)
      (when-let [q (lit st p \!)]
        (some-> (one-test st q) (update 0 assoc :not? true)))))

(defn- chain [st p one sep]
  (when-let [[x q] (one st p)]
    (loop [acc [x] q q]
      (if-let [[y q2] (some->> (lit st q sep) (one st))]
        (recur (conj acc y) q2)
        [acc q]))))

(defn- any-of [st p] (chain st p term \|))

(defn- top [st p]
  (when-let [[t q] (item-type st p)]
    (if-let [q2 (lit st q \[)]
      (let [[cs q3] (or (chain st q2 any-of \,) [[] q2])]
        (when-let [q4 (lit st q3 \])] [{:type t :tests cs} q4]))
      [{:type t :tests []} q])))

(defn- parse-predicate [[s n]]
  (let [st {:s s :err (volatile! {:at -1})}]
    (if-let [[v q] (top st n)]
      [v [s q]]
      (:e @(:err st)))))

(defn item-predicate-arg
  "Returns the item_predicate argument: an item, tag or any, and
  the tests on its components."
  []
  {:id "minecraft:item_predicate" :parse parse-predicate})

(defn- in-bounds? [lo hi n]
  (and (or (nil? lo) (<= (long lo) (long n)))
       (or (nil? hi) (<= (long n) (long hi)))))

(defn- passes?
  "Tells whether stack s passes test t. A raw test never passes
  until its codec is modelled."
  [s [kind k v :as t]]
  (case kind
    :has (stack/has? s k)
    :equals (= v (stack/component s k))
    :count (in-bounds? k v (stack/size s))
    :raw false
    (throw (ex-info "unknown item test" {:test t}))))

(defn- term? [s {:keys [test not?]}]
  (not= (boolean not?) (boolean (passes? s test))))

(defn- type? [[kind v] s]
  (case kind
    nil true
    :item (= v (:item s))
    :tag (contains? (@item-tags v) (:item s))))

(defn matches?
  "Tells whether stack s passes item predicate pred."
  [{:keys [type tests]} s]
  (and (type? type s)
       (every? (fn [alts] (some #(term? s %) alts)) tests)))
