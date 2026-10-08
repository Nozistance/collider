(ns collider.plugin
  "Plugin loading, order and contributions."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.config :as config]
            [collider.game.command.forms :as forms]
            [collider.game.systems.hooks :as hooks]
            [collider.log :as log]
            [malli.core :as m])
  (:import (clojure.lang Compiler DynamicClassLoader RT)
           (java.io File)
           (java.util.zip ZipFile)))

(set! *warn-on-reflection* true)

(def api-version
  "The major version of the plugin API this server has."
  1)

(def default-dir
  "The directory plugins load from when no other is given."
  "plugins")

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
   [:on-event {:optional true} [:map-of :keyword ifn?]]
   [:deltas {:optional true}
    [:map-of :qualified-keyword
     [:map [:schema vector?] [:apply ifn?]]]]
   [:event-filters {:optional true} [:vector ifn?]]
   [:delta-filters {:optional true} [:vector ifn?]]
   [:packets-in {:optional true} [:vector ifn?]]
   [:packets-out {:optional true} [:vector ifn?]]
   [:identity {:optional true}
    [:map [:identify ifn?] [:encrypt? {:optional true} boolean?]]]])

(defn- refused
  ([what why command] (refused what why command nil))
  ([what why command note]
   (let [why (if (string? why) [why] why)]
     (ex-info what (cond-> {:what what :why why :command command}
                     note (assoc :note note))))))

(defn- named [id] (str "plugin " (name id)))

(defn- cause [^Throwable e]
  (str (.getName (class e)) ": " (ex-message e)))

(def ^:private fix-or-remove
  "Fix it, or take it out of plugins/ and start again.")

(defn- jar? [f] (str/ends-with? (str f) ".jar"))

(defn- manifest-text [^File f]
  (if (jar? f)
    (with-open [z (ZipFile. f)]
      (when-let [e (.getEntry z "plugin.edn")]
        (slurp (.getInputStream z e))))
    (let [m (io/file f "plugin.edn")]
      (when (.exists m) (slurp m)))))

(defn- not-edn [where e]
  (refused (str where " is not edn")
           "Collider cannot read the manifest."
           fix-or-remove (cause e)))

(defn- read-manifest [where text]
  (try (edn/read-string text)
       (catch Exception e (throw (not-edn where e)))))

(defn- api-refusal [{:keys [id] :as m}]
  (refused (str (named id) " wants API " (:api-version m))
           (str "This Collider has plugin API " api-version ".")
           (str "Get a build of " (name id) " for API "
                api-version ", or take it out of plugins/.")))

(defn- manifest [f text]
  (let [where (str f (when-not (jar? f) "/plugin.edn"))
        m (read-manifest where text)]
    (when-let [why (seq (config/complaints Manifest m))]
      (throw (refused (str where " has bad fields") (vec why)
                      fix-or-remove)))
    (when (not= api-version (:api-version m))
      (throw (api-refusal m)))
    {:file f :manifest m}))

(defn- found-in [f]
  (if-let [text (manifest-text f)]
    (manifest f text)
    (when (jar? f)
      (throw (refused (str f " has no plugin.edn")
                      "A plugin jar holds plugin.edn at its root."
                      "Take the jar out of plugins/.")))))

(defn- id-of [p] (:id (:manifest p)))

(defn- unique! [names what]
  (doseq [[v n] (frequencies names)
          :when (< 1 (long n))]
    (throw (refused (str "two plugins " what " " v)
                    "Each name may belong to one plugin only."
                    "Take one of them out of plugins/."))))

(def ^:private rename-or-remove
  "Rename it in the plugin, or take the plugin out of plugins/.")

(defn- builtin! [c]
  (when (builtins c)
    (throw (refused (str "a plugin adds the command " c)
                    "Collider has a command of that name."
                    rename-or-remove))))

(defn scan
  "Returns the plugins in directory dir in name order, each as its
  :file and :manifest. A directory without plugin.edn is not a
  plugin. Throws when one is broken or two clash."
  [dir]
  (let [files (sort-by str (.listFiles (io/file dir)))
        found (into [] (keep found-in) files)
        commands (mapcat (comp keys :cli :manifest) found)]
    (unique! (map (comp name id-of) found) "are called")
    (unique! commands "add the command")
    (run! builtin! commands)
    found))

(defn- missing [p d]
  (refused (str (named (id-of p)) " needs " (named d))
           (str (name d) " is not in plugins/.")
           (str "Put " (name d) " in plugins/ too, or take "
                (name (id-of p)) " out.")))

(defn- missing! [ids p]
  (doseq [d (:depends (:manifest p)) :when (not (ids d))]
    (throw (missing p d))))

(defn- circle [left]
  (refused "plugins depend on each other in a circle"
           (str "Their :depends never end: "
                (str/join ", " (map (comp name id-of) left)) ".")
           "Take one of them out of plugins/."))

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
    (doseq [{:keys [file]} found]
      (.addURL cl (io/as-url file)))
    cl))

(defn- logger [id]
  (let [tag (str "[" (name id) "]")
        at (fn [f] (fn [& args] (apply f tag args)))]
    {:info (at log/info) :warn (at log/warn) :error (at log/error)}))

(defn- ctx [{:keys [id]} {:keys [dir mode settings store]}]
  {:id id :mode mode :config (get-in settings [:plugins id])
   :dir (io/file dir (name id)) :log (logger id) :store store})

(defn- in-words [what why ^Throwable e]
  (if (:what (ex-data e))
    e
    (refused what why fix-or-remove (cause e))))

(defn- foreign-tags [id v]
  (for [tag (keys (:deltas v)) :when (not= (name id) (namespace tag))]
    (str tag " is not under :" (name id) "/")))

(defn- checked [id v]
  (let [why (concat (config/complaints Contribution v)
                   (foreign-tags id v))]
    (when (seq why)
      (let [what (str (named id) " returned a bad map from init!")]
        (throw (refused what (vec why) fix-or-remove)))))
  v)

(defn- entry [ns nm] (symbol (name ns) nm))

(defn- with-loader [cl f]
  (with-bindings {Compiler/LOADER cl} (f)))

(defn- init-fn [{:keys [id ns]}]
  (or (requiring-resolve (entry ns "init!"))
      (throw (refused (str (named id) " has no init!")
                      (str "Its :ns " ns " lacks (init! ctx).")
                      fix-or-remove))))

(defn- init [cl env {:keys [manifest] :as p}]
  (let [c (ctx manifest env)
        id (:id manifest)]
    (try
      (let [v (with-loader cl #((init-fn manifest) c))]
        (assoc p :ctx c :loader cl :plugin (checked id v)))
      (catch Exception e
        (let [what (str (named id) " failed to start")]
          (throw (in-words what "Its init! threw." e)))))))

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
  "Loads and starts the plugins in the :dir of env.
  Returns them in load order, none when there is no such directory.
  Throws when one cannot load and stops those started before it."
  [{:keys [dir] :as env}]
  (if-not (.isDirectory (io/file dir))
    []
    (let [found (ordered (scan dir))]
      (reduce (started (class-loader found) env) [] found))))

(defn- system-name [v] (keyword (name (symbol v))))

(defn- no-phase [k]
  (refused (str "a plugin system joins the phase of " (name k))
           "No system of that name runs in the tick."
           fix-or-remove))

(defn- phase-of [phases]
  (let [at (into {} (for [[i ph] (map-indexed vector phases), s ph]
                      [(system-name s) i]))]
    (fn [k] (or (at k) (throw (no-phase k))))))

(defn phases
  "Returns the phases base with each plugin system [anchor s] in the
  phase of the system named anchor."
  [base systems]
  (let [at (phase-of base)]
    (reduce (fn [ps [k s]] (update ps (at k) conj s))
            (mapv vec base) systems)))

(def ^:private spliced (memoize phases))

(defn live-phases
  "Returns a function that gives the phases base-var holds at the call
  with the plugin systems in them."
  [base-var systems]
  #(spliced @base-var systems))

(defn- identity-of [plugins]
  (let [ps (filter (comp :identity :plugin) plugins)]
    (when (next ps)
      (let [names (str/join ", " (map (comp name id-of) ps))]
        (throw (refused "two plugins say who players are"
                        (str "Both give :identity: " names ".")
                        "Take one of them out of plugins/."))))
    (:identity (:plugin (first ps)))))

(defn- taken! [forms]
  (let [names (into #{} (map first) forms/commands)]
    (doseq [[k] forms :when (names k)]
      (throw (refused (str "a plugin adds the command /" (name k))
                      "Collider has a command of that name."
                      rename-or-remove)))))

(defn- cli-of [plugins]
  (into {} (for [p plugins, [c sym] (:cli (:manifest p))]
             [c [p sym]])))

(defn- listener [{:keys [plugin]}]
  (when-let [hs (not-empty (:on-event plugin))]
    [[:chat (hooks/on-event hs)]]))

(defn- invalid! [tag d]
  (let [e (ex-info (str "invalid delta " tag) {:delta d})
        msg (str "plugin delta " tag " broke its schema, dropped")]
    (log/failure! tag msg e)))

(defn- delta-apply [id tag {:keys [schema apply]}]
  (let [valid? (m/validator (into [:cat [:= tag]] (rest schema)))]
    (fn [w d]
      (if (valid? d)
        (update-in w [:plugins id] apply d)
        (do (invalid! tag d) w)))))

(defn- deltas-of [p]
  (let [id (id-of p)]
    (for [[tag spec] (:deltas (:plugin p))]
      [tag (delta-apply id tag spec)])))

(defn- versions [plugins]
  (into (sorted-set)
        (map (fn [{m :manifest}] [(:id m) (:version m)]))
        plugins))

(defn contributions
  "Returns what the plugins add to the server, in load order.
  Throws when a plugin adds a command the server has."
  [plugins]
  (let [joined (fn [k] (into [] (mapcat (comp k :plugin)) plugins))
        forms (joined :commands)]
    (taken! forms)
    {:systems (into (joined :systems) (mapcat listener) plugins)
     :commands forms
     :deltas (into {} (mapcat deltas-of) plugins)
     :event-filters (joined :event-filters)
     :delta-filters (joined :delta-filters)
     :packets-in (joined :packets-in) :packets-out (joined :packets-out)
     :identity (identity-of plugins) :cli (cli-of plugins)
     :plugins (versions plugins)}))

(defn- cli-fn [loader c sym]
  (or (with-loader loader #(requiring-resolve sym))
      (throw (refused (str "plugin command " c " has no function")
                      (str sym " does not resolve.")
                      fix-or-remove))))

(defn run-cli!
  "Runs command line command c of the plugins with args. Returns the
  exit code the entry returns, 0 when it returns none."
  [plugins c args]
  (let [[{:keys [loader ctx]} sym] (get (cli-of plugins) c)
        f (cli-fn loader c sym)
        what (str "command " c " failed")
        code (try (with-loader loader #(f ctx args))
                  (catch Exception e
                    (throw (in-words what "The command threw." e))))]
    (if (int? code) code 0)))

(def ^:private adds-at
  {:plugins [:config :plugins]
   :commands [:config :plugin-commands]
   :deltas [:hooks :deltas]
   :event-filters [:hooks :event-filters]
   :delta-filters [:hooks :delta-filters]
   :packets-in [:hooks :packets-in]
   :packets-out [:hooks :packets-out]})

(defn with-adds
  "Returns world with the contributions adds of the plugins in it."
  [world adds]
  (reduce-kv (fn [w k path]
               (if-let [v (not-empty (get adds k))]
                 (assoc-in w path v)
                 w))
             world adds-at))
