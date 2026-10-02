(ns collider.game.command.dispatcher
  "Parsing and completion of commands over a node graph."
  (:require [clojure.string :as str]
            [collider.game.command.reader :as r]))

(set! *warn-on-reflection* true)

(defn context
  "Returns an empty parse context at cursor n of node root."
  [root n]
  {:root root :range [n n] :nodes []})

(defn- word-at [[^String s n]]
  (let [e (or (str/index-of s " " n) (count s))]
    (subs s n e)))

(defn- relevant
  "Returns the literal child that the next word names, else the
  argument children."
  [node rd]
  (let [{lits :literal args :argument}
        (group-by :type (:children node))
        w (word-at rd)]
    (cond (empty? lits) (:children node)
          :else (or (some #(when (= w (:name %)) [%]) lits)
                    args))))

(defn- with-node [ctx node s e v]
  (-> ctx
      (update :nodes conj {:node node :range [s e] :value v})
      (update :range (fn [[a b]] [(min a s) (max b e)]))))

(defn- read-node [node [s n :as rd] ctx cx]
  (if (= :literal (:type node))
    (let [e (+ n (count (:name node)))]
      [(with-node ctx node n e nil) [s e]])
    (let [res ((:parse node) rd cx ctx)]
      (if (r/error? res)
        res
        (let [[v [_ e :as end]] res]
          [(with-node ctx node n e v) end])))))

(defn- parsed-child [node rd ctx cx]
  (let [res (read-node node rd ctx cx) end (second res)]
    (cond (r/error? res) res
          (and (r/can-read? end) (not (r/at? end \space)))
          (r/error-at end "command.expected.separator")
          :else res)))

(defn- rank [{:keys [rd errors]}]
  [(if (r/can-read? rd) 1 0) (if (seq errors) 1 0)])

(defn- usable? [node cx]
  (if-let [f (:usable? node)] (f cx) true))

(defn- best [pots none]
  (if (seq pots) (first (sort-by rank pots)) none))

(declare parse-nodes)

(defn- redirected [child rd ctx cx graph]
  (let [to (graph (:redirect child)) rd (r/skip rd)
        p (parse-nodes to rd (context to (second rd)) cx graph)]
    (assoc p :ctx (assoc ctx :child (:ctx p)))))

(defn- potential [child rd ctx cx graph]
  (if (r/can-read? rd 2)
    (parse-nodes child (r/skip rd) ctx cx graph)
    {:ctx ctx :rd rd :errors []}))

(defn parse-nodes
  "Returns the best parse of rd from node, redirects through graph."
  [node rd ctx cx graph]
  (loop [cs (filter #(usable? % cx) (relevant node rd))
         errors [] pots []]
    (if-let [c (first cs)]
      (let [res (parsed-child c rd ctx cx)]
        (if (r/error? res)
          (recur (rest cs) (conj errors [c res]) pots)
          (let [[ctx rd] res ctx (assoc ctx :command (:command c))]
            (if (and (:redirect c) (r/can-read? rd))
              (redirected c rd ctx cx graph)
              (recur (rest cs) errors
                     (conj pots (potential c rd ctx cx graph)))))))
      (best pots {:ctx ctx :rd rd :errors errors}))))

(defn parse
  "Returns the parse of input from cursor n."
  [graph input n cx]
  (let [root (graph :root)]
    (parse-nodes root [input n] (context root n) cx graph)))

(defn last-context [ctx]
  (if-let [c (:child ctx)] (recur c) ctx))

(defn failure
  "Returns the error of a parse result, or nil when it can run."
  [{:keys [ctx rd errors]}]
  (let [[a b] (:range ctx)]
    (cond (not (r/can-read? rd))
          (when-not (:command (last-context ctx))
            (r/error-at rd "command.unknown.command"))
          (= 1 (count errors)) (second (first errors))
          (= a b) (r/error-at rd "command.unknown.command")
          :else (r/error-at rd "command.unknown.argument"))))

(defn chain
  "Returns the contexts from the first one to the one that runs."
  [ctx]
  (take-while some? (iterate :child ctx)))

(defn- inside [ctx cursor]
  (loop [prev (:root ctx) ns (:nodes ctx)]
    (if-let [{[a b] :range node :node} (first ns)]
      (if (<= a cursor b) [prev a ctx] (recur node (rest ns)))
      [prev (first (:range ctx)) ctx])))

(defn- suggestion-context
  "Returns the node whose children complete the cursor, and the
  start of the completions."
  [ctx cursor]
  (let [[a b] (:range ctx) l (peek (:nodes ctx))]
    (cond (>= b cursor) (inside ctx cursor)
          (:child ctx) (recur (:child ctx) cursor)
          l [(:node l) (inc (long (second (:range l)))) ctx]
          :else [(:root ctx) a ctx])))

(defn- expanded [^String text lo {:keys [start texts]}]
  (map #(str (subs text lo start) %) texts))

(defn merged
  "Returns suggestions ss as one range, each text widened to it."
  [text ss start]
  (let [ss (filter (comp seq :texts) ss)
        lo (if (seq ss) (reduce min (map :start ss)) start)]
    {:start lo
     :texts (->> ss (mapcat #(expanded text lo %)) distinct
                 (sort String/CASE_INSENSITIVE_ORDER) vec)}))

(defn- literal-suggestions [node ^String text start]
  (let [rem (str/lower-case (subs text start))
        nm (:name node)]
    {:start start
     :texts (if (and (str/starts-with? (str/lower-case nm) rem)
                     (not= nm (subs text start)))
              [nm]
              [])}))

(defn- node-suggestions [node text start ctx cx]
  (if (= :literal (:type node))
    (literal-suggestions node text start)
    (when-let [f (:suggest node)] (f text start ctx cx))))

(defn suggestions
  "Returns the completions at the end of text."
  [{:keys [ctx]} ^String text cx]
  (let [cursor (count text)
        [parent start sctx] (suggestion-context ctx cursor)
        start (min (long start) cursor)
        kids (filter #(usable? % cx) (:children parent))]
    (merged text (keep #(node-suggestions % text start sctx cx) kids)
            start)))
