(ns collider.plugin
  "Plugins: what each declares, the order they load in and what they
  add to the server."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.game.command.tree :as tree]
            [collider.log :as log]
            [malli.core :as m]
            [malli.error :as me])
  (:import (clojure.lang Compiler DynamicClassLoader RT)
           (java.io File)
           (java.util.zip ZipFile)))

(set! *warn-on-reflection* true)

(def api-version
  "The major version of the plugin API this server has."
  1)

(def builtins
  "The commands of the collider command line itself."
  #{"run" "help" "version" "gen" "import" "export"})

(def ^:private Manifest
  [:map
   [:id :keyword]
   [:version :string]
   [:api-version :int]
   [:ns :symbol]
   [:depends {:optional true} [:vector :keyword]]
   [:cli {:optional true} [:map-of :string :qualified-symbol]]])

(def ^:private Contribution
  [:map
   [:systems {:optional true} [:vector [:tuple :keyword ifn?]]]
   [:commands {:optional true} [:vector vector?]]
   [:event-filters {:optional true} [:vector ifn?]]
   [:delta-filters {:optional true} [:vector ifn?]]
   [:identity {:optional true}
    [:map [:identify ifn?] [:encrypt? {:optional true} boolean?]]]])

(defn- refused [what & why]
  (ex-info what {:what what :why (vec why)}))

(defn- named [id] (str "plugin " (name id)))

(defn- cause [^Throwable e]
  (str (.getName (class e)) ": " (ex-message e)))

(defn- jar? [^File f] (str/ends-with? (.getName f) ".jar"))

(defn- manifest-text [^File f]
  (if (jar? f)
    (with-open [z (ZipFile. f)]
      (when-let [e (.getEntry z "plugin.edn")]
        (slurp (.getInputStream z e))))
    (let [m (io/file f "plugin.edn")]
      (when (.exists m) (slurp m)))))

(defn- complaints [schema v]
  (map (fn [[k msgs]] (str k " " (str/join ", " msgs)))
       (me/humanize (m/explain schema v))))

(defn- read-manifest [where text]
  (try (edn/read-string text)
       (catch Exception e
         (throw (refused (str where " is not edn") (ex-message e))))))

(defn- api-refusal [{:keys [id] :as m}]
  (refused (str (named id) " wants API " (:api-version m))
           (str "This Collider has plugin API " api-version ".")))

(defn- manifest [^File f text]
  (let [where (str (.getPath f) (when-not (jar? f) "/plugin.edn"))
        m (read-manifest where text)]
    (when-let [why (seq (complaints Manifest m))]
      (throw (apply refused (str where " has bad fields") why)))
    (when (not= api-version (:api-version m))
      (throw (api-refusal m)))
    {:file f :manifest m}))

(defn- found-in [^File f]
  (if-let [text (manifest-text f)]
    (manifest f text)
    (when (jar? f)
      (throw (refused (str (.getPath f) " has no plugin.edn"))))))

(defn- unique! [k what found]
  (doseq [[v n] (frequencies (mapcat k found))
          :when (< 1 (long n))]
    (throw (refused (str "two plugins " what " " v)))))

(defn- builtin! [c]
  (when (builtins c)
    (throw (refused (str "a plugin adds the command " c)
                    "Collider has a command of that name."))))

(defn scan
  "Returns the plugins in directory dir in name order, each as its
  :file and :manifest. A directory without plugin.edn is not a
  plugin. Throws when one is broken or two clash."
  [dir]
  (let [files (sort-by #(.getName ^File %) (.listFiles (io/file dir)))
        found (into [] (keep found-in) files)
        cli (comp keys :cli :manifest)]
    (unique! (comp vector name :id :manifest) "are called" found)
    (unique! cli "add the command" found)
    (run! builtin! (mapcat cli found))
    found))

(defn- id-of [p] (:id (:manifest p)))

(defn- missing [p d]
  (refused (str (named (id-of p)) " needs " (named d))
           (str "Put " (name d) " in plugins/ too, or take "
                (name (id-of p)) " out.")))

(defn- missing! [ids p]
  (doseq [d (:depends (:manifest p)) :when (not (ids d))]
    (throw (missing p d))))

(defn- circle [left]
  (refused "plugins depend on each other in a circle"
           (str "Their :depends never end: "
                (str/join ", " (map (comp name id-of) left)) ".")))

(defn- ready [done left]
  (first (filter #(every? done (:depends (:manifest %))) left)))

(defn ordered
  "Returns found in an order where each plugin comes after those it
  depends on, otherwise in the order given. Throws when a dependency
  is missing or the dependencies make a circle."
  [found]
  (let [ids (set (map id-of found))]
    (run! #(missing! ids %) found))
  (loop [out [] done #{} left found]
    (if (empty? left)
      out
      (let [p (or (ready done left) (throw (circle left)))]
        (recur (conj out p) (conj done (id-of p))
               (remove #(identical? p %) left))))))

(defn- class-loader [found]
  (let [cl (DynamicClassLoader. (RT/baseLoader))]
    (doseq [{:keys [^File file]} found]
      (.addURL cl (.toURL (.toURI file))))
    cl))

(defn- logger [id]
  (let [tag (str "[" (name id) "]")
        at (fn [f] (fn [& args] (apply f tag args)))]
    {:info (at log/info) :warn (at log/warn) :error (at log/error)}))

(defn- ctx [{:keys [id]} {:keys [dir mode settings store]}]
  {:id id :mode mode :config (get-in settings [:plugins id])
   :dir (io/file dir (name id)) :log (logger id) :store store})

(defn- in-words [what ^Throwable e]
  (if (:what (ex-data e)) e (refused what (cause e))))

(defn- checked [id v]
  (when-let [why (seq (complaints Contribution v))]
    (throw (apply refused (str (named id) " returned a bad map")
                  why)))
  v)

(defn- entry [ns nm] (symbol (name ns) nm))

(defn- with-loader [cl f]
  (with-bindings {Compiler/LOADER cl} (f)))

(defn- init-fn [cl {:keys [id ns]}]
  (or (with-loader cl #(requiring-resolve (entry ns "init!")))
      (throw (refused (str (named id) " has no init!")
                      (str "Its :ns " ns " lacks (init! ctx).")))))

(defn- init [cl env {:keys [manifest] :as p}]
  (let [c (ctx manifest env)
        id (:id manifest)]
    (try
      (let [v (with-loader cl #((init-fn cl manifest) c))]
        (assoc p :ctx c :loader cl :plugin (checked id v)))
      (catch Exception e
        (throw (in-words (str (named id) " failed to start") e))))))

(defn- stop! [{:keys [manifest ctx]}]
  (try (when-let [f (resolve (entry (:ns manifest) "stop!"))] (f ctx))
       (catch Exception e
         (log/warn (named (:id manifest)) "failed to stop:"
                   (str e)))))

(defn stop-all!
  "Stops the plugins in the reverse of their load order."
  [plugins]
  (run! stop! (rseq (vec plugins))))

(defn- started [cl env]
  (fn [acc p]
    (try (conj acc (init cl env p))
         (catch Exception e (stop-all! acc) (throw e)))))

(defn load-all
  "Loads and starts the plugins in env's :dir for :mode, :server or
  :cli. Each init! gets a ctx with its :config section of :settings,
  its :dir, :mode, :log and :store. Returns the plugins in load
  order, none when there is no such directory. Throws with words when
  one cannot load; those started before it stop."
  [{:keys [dir] :as env}]
  (if-not (.isDirectory (io/file dir))
    []
    (let [found (ordered (scan dir))]
      (reduce (started (class-loader found) env) [] found))))

(defn- system-name [v] (keyword (name (symbol v))))

(defn- no-phase [k]
  (refused (str "a plugin system joins the phase of " (name k))
           "No system of that name runs in the tick."))

(defn- phase-of [phases]
  (let [at (into {} (for [[i ph] (map-indexed vector phases), s ph]
                      [(system-name s) i]))]
    (fn [k] (or (at k) (throw (no-phase k))))))

(defn phases
  "Returns phases with each plugin system [anchor s] in the phase of
  the system named anchor."
  [phases systems]
  (let [at (phase-of phases)]
    (reduce (fn [ps [k s]] (update ps (at k) conj s))
            (mapv vec phases) systems)))

(defn- identity-of [plugins]
  (let [ps (filter (comp :identity :plugin) plugins)]
    (when (next ps)
      (throw (refused "two plugins say who players are"
                      (str/join ", " (map (comp name id-of) ps)))))
    (:identity (:plugin (first ps)))))

(defn- taken! [forms]
  (let [names (into #{} (map first) tree/commands)]
    (doseq [[k] forms :when (names k)]
      (throw (refused (str "a plugin adds the command /" (name k))
                      "Collider has a command of that name.")))))

(defn- cli-of [plugins]
  (into {} (for [p plugins, [c sym] (:cli (:manifest p))]
             [c [p sym]])))

(defn contributions
  "Returns what the plugins add, in load order: the :systems as
  [anchor system], the :commands as forms of the command tree, the
  :event-filters and :delta-filters, the :identity function and the
  :cli entries by name."
  [plugins]
  (let [joined (fn [k] (into [] (mapcat (comp k :plugin)) plugins))
        forms (joined :commands)]
    (taken! forms)
    {:systems (joined :systems) :commands forms
     :event-filters (joined :event-filters)
     :delta-filters (joined :delta-filters)
     :identity (identity-of plugins) :cli (cli-of plugins)}))

(defn- cli-fn [loader c sym]
  (or (with-loader loader #(requiring-resolve sym))
      (throw (refused (str "plugin command " c " has no function")
                      (str sym " does not resolve.")))))

(defn run-cli!
  "Runs command line command c of the plugins with args. Returns the
  exit code the entry returns, 0 when it returns none."
  [plugins c args]
  (let [[{:keys [loader ctx]} sym] (get (cli-of plugins) c)
        f (cli-fn loader c sym)
        what (str "command " c " failed")
        code (try (with-loader loader #(f ctx args))
                  (catch Exception e (throw (in-words what e))))]
    (if (int? code) code 0)))
