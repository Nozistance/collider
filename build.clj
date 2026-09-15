(ns build
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.build.api :as b])
  (:import (java.time Instant)))

(def class-dir "target/classes")
(def prim-dir "target/classes")
(def jar-file "target/collider.jar")
(def basis (b/create-basis {:project "deps.edn"}))
(defn clean [_]
  (b/delete {:path class-dir})
  (b/delete {:path jar-file}))

(defn javac [_]
  (b/javac {:src-dirs   ["src"]
            :class-dir  prim-dir
            :basis      basis
            :javac-opts ["-proc:none" "--release" "21"]}))

(defn- log-bytes [n] ((requiring-resolve 'collider.log/human-bytes) n))
(defn- info [& args] (apply (requiring-resolve 'collider.log/info) args))
(defn- step [doing done f] ((requiring-resolve 'collider.log/step) doing done f))

(defn fetch [{:keys [version jar]}]
  (let [version (or version @(requiring-resolve 'collider.tables/version))]
    (info "vanilla jar at" (str ((requiring-resolve 'collider.tables/fetch) version jar)))))

(defn data [opts]
  ((requiring-resolve 'collider.tables/generate!) opts))

(defn- compile-clj []
  (b/compile-clj {:basis      basis
                  :src-dirs   ["src"]
                  :class-dir  class-dir
                  :ns-compile '[collider.launch collider.core]
                  :java-opts  ["-Dclojure.compiler.direct-linking=true"]}))

(defn- uber []
  (b/uber {:class-dir class-dir
           :uber-file jar-file
           :basis     basis
           :main      'collider.launch
           :exclude   [".*\\.java$" ".*\\.cljs$"]}))

(defn release [_]
  (clean nil)
  (step "Compiling java" "Compiled java" #(javac nil))
  (step "Compiling clojure" "Compiled clojure" #(compile-clj))
  (step "Packing the jar" "Packed the jar" #(uber))
  (info jar-file (log-bytes (.length (io/file jar-file)))))

(defn- pascal [k]
  (apply str (map str/capitalize (str/split (name k) #"-"))))

(defn- hook-status [mark]
  (let [m (str/trim (or mark ""))]
    (cond (str/starts-with? m "x") "done"
          (str/starts-with? m "pending") "pending"
          :else "open")))

(defn- parse-hooks [text]
  (let [sections {"Entities" "entities" "Blocks" "blocks"}]
    (loop [lines (str/split-lines text) cat nil cls nil acc {}]
      (if-let [l (first lines)]
        (cond
          (str/starts-with? l "## ") (recur (rest lines) (get sections (subs l 3)) nil acc)
          (and cat (str/starts-with? l "### "))
          (let [[_ c] (re-matches #"### (\S+) - `([^`]+)`" l)]
            (recur (rest lines) cat c (assoc-in acc [cat c] {:hooks []})))
          (and cat cls (str/starts-with? l "- "))
          (let [[_ h owner mark] (re-matches #"- (\S+) \(([^)]*)\) \|(.*)" l)]
            (recur (rest lines) cat cls
                   (cond-> acc h (update-in [cat cls :hooks] conj {:name h :owner owner :status (hook-status mark)}))))
          :else (recur (rest lines) cat cls acc))
        acc))))

(defn- excluded-counts [text]
  (let [section (fn [head]
                  (->> (str/split-lines text)
                       (drop-while #(not= % (str "## Excluded as " head)))
                       rest
                       (take-while #(not (str/starts-with? % "## ")))
                       (filter #(re-matches #"\| [^|]+ \| [^|]+ \|" %))
                       count
                       (max 0)))]
    {:client-only      (- (section "client-only") 1)
     :developer-compat (- (section "developer compatibility") 1)}))

(def ^:private type-classes
  {:jack-o-lantern "CarvedPumpkinBlock" :enchantment-table "EnchantingTableBlock"})

(defn- letters [s] (str/lower-case (str/replace (str s) #"[^A-Za-z]" "")))
(defn- block-entries [classes]
  (let [blocks ((requiring-resolve 'collider.data/blocks))
        by-letters (into {} (map (fn [c] [(letters c) c])) classes)]
    (reduce (fn [m [b info]]
              (let [t (:type info)
                    c (or (type-classes t)
                          (get by-letters (letters (str (name t) "block")))
                          (get by-letters (letters t))
                          (str (pascal t) "Block"))]
                (update m c (fnil conj []) (name b))))
            {} (sort-by key blocks))))

(defn- with-entries [blocks]
  (reduce (fn [m [c es]]
            (update m c #(assoc (or % {:hooks []}) :entries (vec (sort es)))))
          blocks
          (block-entries (set (keys blocks)))))

(defn tracker [{:keys [hooks out] :or {hooks "../exclude/hooks.md" out "parity/implementation.json"}}]
  (let [parsed (parse-hooks (slurp hooks))
        data {:meta     {:generated (str (Instant/now))
                         :commit    (str/trim (b/git-process {:git-args "rev-parse HEAD"}))
                         :target    "26.2"}
              :blocks   (with-entries (get parsed "blocks" {}))
              :entities (get parsed "entities" {})}
        excluded (excluded-counts (slurp hooks))
        total (fn [cat] (let [hs (mapcat :hooks (vals (get data cat)))]
                          [(count (filter #(= "done" (:status %)) hs))
                           (count (filter #(= "pending" (:status %)) hs))
                           (count hs)]))]
    (io/make-parents out)
    (spit out (json/write-str data))
    (let [tally (fn [cat] (let [hs (mapcat :hooks (vals (get data cat)))
                                done (count (filter #(= "done" (:status %)) hs))
                                pending (count (filter #(= "pending" (:status %)) hs))]
                            {:classes (count (get data cat)) :hooks (count hs) :done done
                             :pending pending
                             :pct     (if (seq hs) (Math/round (* 100.0 (/ done (count hs)))) 0)}))
          b (tally :blocks) e (tally :entities)
          sum (fn [k] (+ (long (k b)) (long (k e))))
          all {:classes (sum :classes) :hooks (sum :hooks) :done (sum :done)
               :pending (sum :pending)
               :pct     (if (pos? (sum :hooks)) (Math/round (* 100.0 (/ (sum :done) (sum :hooks)))) 0)}]
      (spit (str (.getParent (io/file out)) "/summary.json")
            (json/write-str (assoc (:meta data) :all all :blocks b :entities e :excluded excluded)))
      (info (format "%s/summary.json: %d%% overall, %d%% in blocks"
                    (.getParent (io/file out)) (:pct all) (:pct b))))
    (info (format "%s: blocks %d classes, hooks done/pending/all %s; entities %d classes, %s"
                  out (count (:blocks data)) (total :blocks) (count (:entities data)) (total :entities)))))
