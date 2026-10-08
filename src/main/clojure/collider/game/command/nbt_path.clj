(ns collider.game.command.nbt-path
  "Paths into tags, as /data reads them."
  (:require [collider.game.command.reader :as r]
            [collider.game.command.snbt :as snbt]))

(set! *warn-on-reflection* true)

(def ^:private invalid "arguments.nbtpath.node.invalid")

(defn- name-char? [c] (not (#{\space \" \' \[ \] \. \{ \}} c)))

(defn- read-name [[s n :as rd]]
  (let [e (loop [i n]
            (if (and (< i (count s)) (name-char? (nth s i)))
              (recur (inc i))
              i))]
    (if (= e n) (r/error-at rd invalid) [(subs s n e) [s e]])))

(defn- object-node [res]
  (let [[nm rd] (when-not (r/error? res) res)]
    (cond (r/error? res) res
          (= "" nm) (r/error-at rd invalid)
          (r/at? rd \{)
          (let [c (snbt/read-compound rd)]
            (if (r/error? c)
              c
              [[:match-object (keyword nm) (first c)] (second c)]))
          :else [[:child (keyword nm)] rd])))

(defn- closed [res node]
  (if (r/error? res)
    res
    (let [end (r/expect (second res) \])]
      (if (r/error? end) end [(node (first res)) end]))))

(defn- element-node [rd]
  (cond (r/at? rd \{)
        (closed (snbt/read-compound rd) #(vector :match-element %))
        (r/at? rd \]) [[:all] (r/skip rd)]
        :else (closed (r/read-int rd) #(vector :index %))))

(defn- read-node [[s n :as rd] first?]
  (case (when (r/can-read? rd) (nth s n))
    (\" \') (object-node (r/read-string rd))
    \[ (element-node (r/skip rd))
    \{ (if first?
         (let [c (snbt/read-compound rd)]
           (if (r/error? c) c [[:match-root (first c)] (second c)]))
         (r/error-at rd invalid))
    (object-node (read-name rd))))

(defn- dot [[s n :as rd]]
  (if (and (r/can-read? rd) (not (#{\space \[ \{} (nth s n))))
    (r/expect rd \.)
    rd))

(defn read-path
  "Reads a path into its text and its nodes, each with the cursor
  where it ends."
  [[s start :as rd]]
  (loop [rd rd nodes []]
    (if (and (r/can-read? rd) (not (r/at? rd \space)))
      (let [res (read-node rd (empty? nodes))]
        (if (r/error? res)
          res
          (let [[node [_ e :as end]] res
                nxt (dot end)]
            (if (r/error? nxt)
              nxt
              (recur nxt (conj nodes [node (- e start)]))))))
      [{:text (subs s start (second rd)) :nodes nodes} rd])))

(defn- collection? [x]
  (or (vector? x) (bytes? x) (instance? (Class/forName "[I") x)
      (instance? (Class/forName "[J") x)))

(defn- matches? [pattern x] (snbt/compare-nbt pattern x true))

(defn- step [[kind a b] x]
  (case kind
    :child (when (map? x) (some-> (get x a) vector))
    :match-object (when (map? x)
                    (let [v (get x a)] (when (matches? b v) [v])))
    :match-root (when (and (map? x) (matches? a x)) [x])
    :all (when (collection? x) (seq x))
    :match-element (when (vector? x) (filter #(matches? a %) x))
    :index (when (collection? x)
             (let [n (count x) i (if (neg? a) (+ n a) a)]
               (when (< -1 i n) [(nth x i)])))))

(defn- end-of
  "Returns where node i ends in the text. Every [] node is one
  vanilla node, so each of them ends where the last one does."
  [nodes i]
  (let [[node e] (nodes i)]
    (if (= [:all] node)
      (second (last (filter #(= [:all] (first %)) nodes)))
      e)))

(defn tags
  "Returns the tags path finds in tag, or the nothing found error."
  [{:keys [text nodes]} tag]
  (reduce (fn [found i]
            (let [found (mapcat #(step (first (nodes i)) %) found)]
              (if (seq found)
                found
                (reduced (r/error "arguments.nbtpath.nothing_found"
                                  (subs text 0 (end-of nodes i)))))))
          [tag] (range (count nodes))))
