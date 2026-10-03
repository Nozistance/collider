(ns collider.game.command.help
  "The usage lines that /help shows."
  (:require [clojure.string :as str]
            [collider.game.command.dispatcher :as d]
            [collider.game.command.nodes :as node-tree]
            [collider.game.command.tree :as tree]))

(set! *warn-on-reflection* true)

(defn- usage-text [n]
  (if (= :argument (:type n)) (str "<" (:name n) ">") (:name n)))

(defn- redirect-usage [nodes n]
  (let [to (:redirect n)]
    (if (= to (dec (count nodes)))
      "..."
      (str "-> " (usage-text (nth nodes to))))))

(declare smart-usage)

(defn- alternatives [nodes kids opt?]
  (let [us (distinct (map #(smart-usage nodes % opt? true) kids))
        [open close] (if opt? ["[" "]"] ["(" ")"])]
    (if (= 1 (count us))
      (if opt? (str "[" (first us) "]") (first us))
      (str open (str/join "|" (map #(usage-text (nth nodes %)) kids))
           close))))

(defn- smart-usage
  [nodes i optional? deep?]
  (let [n (nth nodes i)
        self (cond->> (usage-text n) optional? (format "[%s]"))
        opt? (boolean (:executable? n))
        kids (:children n)]
    (cond
      deep? self
      (:redirect n) (str self " " (redirect-usage nodes n))
      (= 1 (count kids))
      (str self " " (smart-usage nodes (first kids) opt? opt?))
      (seq kids) (str self " " (alternatives nodes kids opt?))
      :else self)))

(defn- help-node [nodes text level extra]
  (let [res (d/parse (tree/graph extra) text 0 {:level level})
        l (peek (:nodes (:ctx res)))]
    (when l
      (node-tree/index-of nodes #(= (:path %) (:path (:node l)))))))

(defn- help-of [nodes n text]
  (let [head (if text (str "/" text " ") "/")
        opt? (boolean (:executable? n))]
    (mapv #(str head (smart-usage nodes % opt? false))
          (:children n))))

(defn help-lines
  "Returns the lines /help shows a player of permission level, for
  command text or for every command when text is nil, with the
  command forms extra. Returns nil when text names no command."
  ([level text] (help-lines level text nil))
  ([level text extra]
   (let [all (-> (node-tree/root-node level extra)
                 (node-tree/with-paths [])
                 node-tree/flattened)
         i (if text
             (help-node all text level extra)
             (dec (count all)))]
     (when i (help-of all (nth all i) text)))))
