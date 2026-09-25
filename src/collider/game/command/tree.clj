(ns collider.game.command.tree
  "Player commands, their arguments and their meaning."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.gamerules :as rules]
            [collider.game.mob.mobs :as mobs]
            [collider.game.schema :as schema]))

(set! *warn-on-reflection* true)

(defn- block-name [kw] (data/snake kw))

(defn- block-kw [s] (data/kebab (str s)))

(def ^:private time-names {"day" 1000 "night" 13000})

(def ^:private int-max 2147483647)

(defn- weather-form [nm doc op]
  [nm doc
   [[:duration [:duration {:min 1 :max 1000000 :default 0}]]]
   [:world op]])

(defn- pos-args
  ([] (pos-args :x :y :z {:node "pos"}))
  ([x y z opts]
   [[x [:coord (assoc opts :axis 0)]]
    [y [:coord (assoc opts :axis 1)]]
    [z [:coord (assoc opts :axis 2)]]]))

(defn- vec-args [opts]
  [[:x [:dcoord (assoc opts :axis 0)]]
   [:y [:dcoord (assoc opts :axis 1)]]
   [:z [:dcoord (assoc opts :axis 2)]]])

(def commands
  [[:time "change or query the time of day"
    [:set "set the time"
     [[:value [:named-int {:min 0 :max int-max :names time-names}]]]
     [:world :time-set]]
    [:add "advance the time"
     [[:value [:int {:min 0 :max int-max}]]]
     [:world :time-add]]
    [:query "read the clock"
     [[:clock [:enum {:values #{"daytime" "gametime"}}]]]
     [:world :time-query]]]
   [:gamerule "read or set a game rule"
    [[:rule [:rule {}]]
     [:value [:text {:default nil}]]]
    [:world :gamerule]]
   [:teleport "teleport to a position (~ = where you are) or entity"
    [[:destination [:targets {:single? true}]]]
    [:world :tp-to]
    (vec-args {:node "location"})
    [:world :tp]
    [[:targets [:targets {}]]
     [:destination [:targets {:single? true}]]]
    [:world :tp-targets-to]
    (into [[:targets [:targets {}]]] (vec-args {:node "location"}))
    [:world :tp-targets]]
   [:give "give items to players"
    [[:targets [:targets {:players? true}]]
     [:item [:item {}]]
     [:count [:int {:min 1 :max 6400 :default 1}]]]
    [:world :give]]
   [:kill "kill entities (yourself without a target)"
    [[:targets [:targets {:default {:self true}}]]]
    [:world :kill]]
   [:summon "summon a mob (~ = where you are)"
    (into [[:entity [:entity-type {}]]]
          (vec-args {:default nil :node "pos"}))
    [:world :summon]]
   [:setblock "set one block (~ = your position)"
    (conj (pos-args) [:block [:block {}]])
    [:world :setblock]]
   [:setworldspawn
    "set the world spawn point (default: where you are)"
    (pos-args :x :y :z {:default nil :node "pos"})
    [:world :setworldspawn]]
   [:spawnpoint "set your respawn point (default: where you are)"
    (pos-args :x :y :z {:default nil :node "pos"})
    [:world :spawnpoint]]
   [:weather "set the weather"
    (weather-form :clear "clear the sky" :weather-clear)
    (weather-form :rain "let it rain" :weather-rain)
    (weather-form :thunder "let it storm" :weather-thunder)]
   [:fill "fill a box with a block (~ = your position)"
    (-> (pos-args :x1 :y1 :z1 {:node "from"})
        (into (pos-args :x2 :y2 :z2 {:node "to"}))
        (conj [:block [:block {:default :stone}]]))
    [:world :fill]]])

(defn- subcommands? [form] (keyword? (first (nth form 2))))

(defn- cmd-name [form] (name (first form)))

(defn- find-form [forms nm]
  (first (filter #(= nm (cmd-name %)) forms)))

(defn- label [[nm [kind {:keys [min max]}]]]
  (case kind
    :duration (str "<" (name nm) ">")
    :int (str "<" (name nm) " " min "-" max ">")
    (str "<" (name nm) ">")))

(defn- parse-long* [^String s]
  (try (Long/parseLong s) (catch NumberFormatException _ nil)))

(defn- parse-int* [^String s]
  (try (long (Integer/parseInt s))
       (catch NumberFormatException _ nil)))

(defn- parse-double* [^String s]
  (try (Double/parseDouble s) (catch NumberFormatException _ nil)))

(defn- in-range [nm n {:keys [min max]}]
  (if (<= (long min) (long n) (long max))
    [:ok n]
    [:err (str (name nm) ": give a value from " min " to " max
               ", not " n)]))

(defn- as-int [nm s opts _origin]
  (if-let [n (parse-long* s)]
    (in-range nm n opts)
    [:err (str (name nm) ": give a whole number, not \"" s "\"")]))

(defn- name-hint [names]
  (str (str/join "/" (take 6 (sort (keys names))))
       (when (> (count names) 6) "/...")))

(defn- as-named-int [nm s {:keys [names] :as opts} _origin]
  (let [k (str/lower-case (str/replace (str s) #"^minecraft:" ""))]
    (if-let [n (or (get names k) (parse-long* s))]
      (in-range nm n opts)
      [:err (str (name nm) ": give a whole number or "
                 (name-hint names) ", not \"" s "\"")])))

(defn- offset-of [^String s]
  (if (= "~" s) 0.0 (parse-double* (subs s 1))))

(defn- block-coord
  "Returns the block coordinate text s names, or nil.
  WorldCoordinate.parseInt: a ~ offset may have a fraction."
  [^String s axis origin]
  (if (str/starts-with? s "~")
    (when origin
      (when-let [off (offset-of s)]
        (let [at (+ (double (nth origin axis)) (double off))]
          (long (Math/floor at)))))
    (parse-int* s)))

(defn- as-coord [nm s {:keys [axis]} origin]
  (if-let [n (block-coord s axis origin)]
    [:ok n]
    [:err (str (name nm) ": give a whole number or ~, not \"" s
               "\"")]))

(defn- centered ^double [^double n ^String s ^long axis]
  (if (and (not= 1 axis) (not (str/includes? s "."))) (+ n 0.5) n))

(defn- exact-coord [^String s axis origin]
  (if (str/starts-with? s "~")
    (when origin
      (when-let [off (offset-of s)]
        (+ (double (nth origin axis)) (double off))))
    (when-let [v (parse-double* s)]
      (centered (double v) s (long axis)))))

(defn- as-dcoord [nm s {:keys [axis]} origin]
  (if-let [n (exact-coord s axis origin)]
    [:ok (double n)]
    [:err (str (name nm) ": give a number or ~, not \"" s "\"")]))

(defn- as-item [nm s _opts _origin]
  (let [k (block-kw s)]
    (if (contains? (get (data/registries) "item") k)
      [:ok k]
      [:err (str (name nm) ": unknown item \"" s "\"")])))

(defn- as-entity-type [nm s _opts _origin]
  (let [k (block-kw s)
        known (str/join ", " (sort (map name (keys mobs/types))))]
    (if (mobs/mob-type? k)
      [:ok k]
      [:err (str (name nm) ": cannot summon \"" s "\", only "
                 known)])))

(def ^:private selectors
  {"s" {:self true} "a" {:all true}
   "p" {:nearest true} "e" {:entities true}})

(defn- as-targets [nm s {:keys [players? single?]} _origin]
  (let [[_ sel args] (re-matches #"@([saep])(?:\[(.*)\])?" s)
        type-re #"type=(!?)([a-z_:]+)"
        [_ negated type] (when args (re-find type-re args))
        type (some-> type block-kw)]
    (cond
      (nil? sel) [:ok {:name s}]
      (and players? (= "e" sel))
      [:err (str (name nm) ": @e is not a player")]
      (and single? (#{"a" "e"} sel))
      [:err (str (name nm) ": give one entity, not @" sel)]
      :else [:ok (cond-> (selectors sel)
                   type (assoc :type type)
                   (= "!" negated) (assoc :not-type? true))])))

(defn- as-block [nm s _opts _origin]
  (let [k (block-kw s)]
    (if (contains? (data/blocks) k)
      [:ok k]
      [:err (str (name nm) ": unknown block \"" s "\"")])))

(defn- as-enum [nm s {:keys [values]} _origin]
  (if (contains? values s)
    [:ok s]
    [:err (str (name nm) ": give one of "
               (str/join ", " (sort values)) ", not \"" s "\"")]))

(defn- as-rule [nm s _opts _origin]
  (if-let [r (rules/rule-of s)]
    [:ok r]
    [:err (str (name nm) ": unknown game rule \"" s "\"")]))

(def ^:private time-units {"" 1 "t" 1 "s" 20 "d" 24000})

(defn- as-duration [nm s opts _origin]
  (let [re #"(-?[0-9]*\.?[0-9]+)([a-z]*)"
        [_ value unit] (re-matches re (str s))
        factor (get time-units (or unit ""))
        scale (double (or factor 1))
        ticks #(Math/round (* (Double/parseDouble value) scale))]
    (cond
      (nil? factor)
      [:err (str (name nm) ": give a duration in ticks, or with"
                 " d, s or t, not \"" s "\"")]
      (nil? value)
      [:err (str (name nm) ": give a duration, not \"" s "\"")]
      :else (in-range nm (ticks) opts))))

(defn- as-text [_nm s _opts _origin] [:ok s])

(def ^:private coercers
  {:int as-int :named-int as-named-int :coord as-coord
   :dcoord as-dcoord :enum as-enum :block as-block :item as-item
   :entity-type as-entity-type :targets as-targets :rule as-rule
   :text as-text :duration as-duration})

(defn- coerce [[nm [kind opts] :as arg] s origin]
  (cond
    (some? s) ((coercers kind as-enum) nm s opts origin)
    (contains? opts :default) [:ok (:default opts)]
    :else [:err (str "give the argument " (label arg))]))

(defn- int-values [{:keys [min max default]}]
  (->> [default min (quot (+ (long min) (long max)) 2) max]
       (remove nil?) (map str) distinct vec))

(defn- named-int-values [{:keys [default names]}]
  (let [dn (some (fn [[k v]] (when (= v default) k)) names)]
    (into (if dn [dn] []) (sort (remove #{dn} (keys names))))))

(defn- block-values [{:keys [default]}]
  (into [(block-name default)]
        (remove #{(block-name default)})
        (sort (map block-name (keys (data/blocks))))))

(defn- coord-values [target axis]
  (if target [(str (nth target axis))] []))

(defn- rule-names []
  (mapv (comp #(subs % 10) rules/wire-name) (keys rules/table)))

(defn- item-names []
  (vec (sort (map block-name (keys (get (data/registries) "item"))))))

(defn- arg-values [[_ [kind {:keys [values axis] :as opts}]] target]
  (case kind
    :duration ["1d" "1s" "100"]
    :int (int-values opts)
    (:coord :dcoord) (coord-values target axis)
    :named-int (named-int-values opts)
    :enum (vec (sort values))
    :rule (rule-names)
    :text []
    :item (item-names)
    :entity-type (vec (sort (map name (keys mobs/types))))
    :targets ["@s" "@a" "@p" "@e"]
    :block (block-values opts)))

(defn usage
  "Returns the usage line of the command at path."
  [path]
  (let [form (loop [forms commands, [nm & more] path]
               (let [f (find-form forms nm)]
                 (if (seq more) (recur (drop 2 f) more) f)))
        [_ doc args] form]
    (str "**/" (str/join " " path) "**"
         (when (seq args) (str " " (str/join " " (map label args))))
         " - " doc)))

(defn- usage-lines [form path]
  (if (subcommands? form)
    (mapcat #(usage-lines % (conj path (cmd-name %))) (drop 2 form))
    [(usage path)]))

(defn- leftover [ts path]
  {:error (str "unexpected \"" (first ts) "\"\n" (usage path))})

(defn- parse-args [args tokens path origin]
  (loop [as args, ts tokens, acc []]
    (if-let [a (first as)]
      (let [[st v] (coerce a (first ts) origin)]
        (if (= :err st)
          {:error (str v "\n" (usage path))}
          (recur (next as) (next ts) (conj acc v))))
      (if (seq ts) (leftover ts path) {:args acc}))))

(defn- ways
  "Returns the [args action] pairs of a form without subcommands."
  [form]
  (partition 2 (drop 2 form)))

(defn- way-delta [[args action] tokens path origin]
  (let [r (parse-args args tokens path origin)]
    (if (:error r) r {:delta (into action (:args r))})))

(defn- delta-of [form tokens path origin]
  (let [rs (map #(way-delta % tokens path origin) (ways form))]
    (or (first (filter :delta rs)) (first rs))))

(defn- no-subcommand [form nm sub]
  {:error (str (if sub
                 (str "unknown subcommand \"" sub "\"")
                 "give a subcommand")
               "\n" (str/join "\n" (usage-lines form [nm])))})

(defn- parse-subcommand [form nm [sub & arg-tokens] origin]
  (if-let [sform (find-form (drop 2 form) sub)]
    (delta-of sform arg-tokens [nm sub] origin)
    (no-subcommand form nm sub)))

(def ^:private aliases {"tp" "teleport"})

(defn- dimension-of [s]
  (let [k (data/kebab (str/replace (str s) #"^minecraft:" ""))]
    (some #{k} schema/dims)))

(defn- scale ^double [from to]
  (/ (double (:coordinate-scale (data/dimension-type from)))
     (double (:coordinate-scale (data/dimension-type to)))))

(defn- scaled
  "Returns origin moved from level from to level to.
  Only x and z scale, by the teleportation scale of the two."
  [origin from to]
  (if (or (nil? origin) (= from to))
    origin
    (let [k (scale from to)]
      [(* k (double (nth origin 0))) (nth origin 1)
       (* k (double (nth origin 2)))])))

(declare parse-words parse-execute)

(defn- parse-in [[d & more] origin dim]
  (if-let [to (dimension-of d)]
    (parse-execute more (scaled origin dim to) to)
    {:error (if d
              (str "dimension: unknown dimension \"" d "\"")
              "give the argument <dimension>")}))

(defn- parse-execute [[w & more] origin dim]
  (case w
    "in" (parse-in more origin dim)
    "run" (if (seq more)
            (merge {:dim dim :origin origin}
                   (parse-words more origin dim))
            {:error "give a command to run"})
    {:error "give in <dimension> or run <command>"}))

(defn- parse-words [[nm & more] origin dim]
  (let [nm (get aliases nm nm)
        form (find-form commands nm)]
    (cond
      (= "execute" nm) (parse-execute more origin dim)
      (nil? form)
      {:error (str "unknown command" (when nm (str " \"/" nm "\"")))}
      (subcommands? form) (parse-subcommand form nm more origin)
      :else (delta-of form more [nm] origin))))

(defn parse
  "Returns the delta the typed command means.
  Returns the reason it cannot run instead. Relative coordinates
  count from origin, in level dim. A command run in another level
  also returns that level as :dim and origin there as :origin."
  ([text] (parse text nil))
  ([text origin] (parse text origin :overworld))
  ([text origin dim]
   (let [words (str/split (subs text 1) #"\s+")]
     (parse-words (remove str/blank? words) origin dim))))

(defn- starting-with [prefix xs]
  (let [p (str/lower-case prefix)]
    (vec (filter #(str/starts-with? (str/lower-case %) p) xs))))

(defn- suggest-player [world prefix]
  (starting-with prefix (sort (keys (:players world)))))

(defn- command-names []
  (into ["execute"] (concat (keys aliases) (map cmd-name commands))))

(defn- suggest-command [prefix]
  (mapv #(str "/" %)
        (starting-with prefix (sort (command-names)))))

(defn- suggest-subcommand [form prefix]
  (starting-with prefix (sort (map cmd-name (drop 2 form)))))

(defn- suggest-arg [form tokens i target]
  (if-let [a (nth (nth form 2 []) i nil)]
    (starting-with (last tokens) (arg-values a target))
    []))

(defn- suggest-after-command [form tokens target]
  (cond
    (not (subcommands? form))
    (suggest-arg form tokens (dec (count tokens)) target)
    (= 1 (count tokens)) (suggest-subcommand form (first tokens))
    :else (suggest-arg (find-form (drop 2 form) (first tokens))
                       tokens
                       (- (count tokens) 2)
                       target)))

(defn suggest
  "Returns the completions for half-typed text.
  A player standing at target sees them."
  ([world text] (suggest world text nil))
  ([world text target]
   (let [text (or text "")]
     (if-not (str/starts-with? text "/")
       (suggest-player world (last (str/split text #" " -1)))
       (let [[nm & more] (str/split (subs text 1) #" " -1)
             form (find-form commands (get aliases nm nm))]
         (cond
           (empty? more) (suggest-command nm)
           (nil? form) []
           :else (suggest-after-command form more target)))))))

(def ^:private brigadier-integer (keyword "brigadier:integer"))

(def ^:private brigadier-bool (keyword "brigadier:bool"))

(def ^:private brigadier-string (keyword "brigadier:string"))

(def ^:private plain-arguments
  {:duration [:time {:min 1}]
   :named-int [:time {:min 0}]
   :coord [:block-pos nil]
   :dcoord [:vec3 nil]
   :block [:block-state nil]
   :item [:item-stack nil]
   :entity-type [:resource {:registry "minecraft:entity_type"}]
   :text [brigadier-string {:kind 0}]})

(defn- entity-props [single? players?]
  {:single? (boolean single?) :players? (boolean players?)})

(defn- argument-nodes
  [[nm [kind {:keys [min max values single? players?]}]]]
  (let [n (name nm)]
    (if-let [[parser props] (plain-arguments kind)]
      [[n parser props]]
      (case kind
        :int [[n brigadier-integer {:min min :max max}]]
        :enum {:literals (sort values)}
        :targets [[n :entity (entity-props single? players?)]]
        :rule {:rules true}))))

(defn- coords-merged [args]
  (loop [as args acc []]
    (if-let [[nm [kind opts] :as a] (first as)]
      (if (and (#{:coord :dcoord} kind) (= 0 (long (:axis opts 0))))
        (recur (drop 3 as)
               (conj acc [(keyword (:node opts nm)) [kind opts]]))
        (recur (rest as) (conj acc a)))
      acc)))

(defn- optional? [[_ [_ opts]]] (contains? opts :default))

(defn- optional-from [args]
  (or (first (keep-indexed
               (fn [i _] (when (every? optional? (drop i args)) i))
               args))
      (count args)))

(defn- rule-value [{:keys [type min max]}]
  {:type :argument :name "value" :executable? true
   :parser (if (= :bool type) brigadier-bool brigadier-integer)
   :props (when (= :int type) {:min min :max max})})

(defn- rule-nodes []
  (vec (for [[rule spec] rules/table]
         {:type :literal :executable? true
          :name (subs (rules/wire-name rule) 10)
          :children [(rule-value spec)]})))

(defn- spec-nodes [spec exec? children]
  (cond
    (:rules spec) (rule-nodes)
    (:literals spec)
    (mapv (fn [l] {:type :literal :name l :executable? exec?
                   :children children})
          (:literals spec))
    :else
    (let [[n parser props] (first spec)]
      [{:type :argument :name n :parser parser :props props
        :executable? exec? :children children}])))

(defn- chain
  "Returns [nodes executable?] of the args from i on.
  The flag tells whether the node before them can run."
  [args ^long i]
  (if (>= i (count args))
    [[] true]
    (let [optional (optional-from args)
          spec (argument-nodes (nth args i))
          tail (if (:rules spec) [[] true] (chain args (inc i)))
          [kids tail-exec] tail
          exec? (or tail-exec (>= (inc i) optional))]
      [(spec-nodes spec exec? kids) (>= i optional)])))

(defn- same-node? [a b]
  (let [k #(dissoc % :children :executable?)]
    (= (k a) (k b))))

(defn- index-of [xs pred]
  (first (keep-indexed (fn [i x] (when (pred x) i)) xs)))

(declare merged-nodes)

(defn- merged-node [a b]
  (-> a
      (update :executable? #(boolean (or % (:executable? b))))
      (update :children
              #(merged-nodes (into (vec %) (:children b))))))

(defn- merged-nodes
  "Returns sibling nodes with the same name folded into one.
  Their children fold the same way."
  [nodes]
  (reduce (fn [acc n]
            (if-let [i (index-of acc #(same-node? % n))]
              (update acc i merged-node n)
              (conj acc n)))
          [] nodes))

(defn- form-node [form]
  (if (subcommands? form)
    {:type :literal :name (cmd-name form)
     :children (mapv form-node (drop 2 form))}
    (let [chains (mapv #(chain (coords-merged (first %)) 0)
                       (ways form))]
      {:type :literal :name (cmd-name form)
       :executable? (boolean (some second chains))
       :children (merged-nodes (into [] (mapcat first) chains))})))

(def ^:private dimension-node
  {:type :argument :name "dimension" :parser :dimension :props nil
   :children [] :redirect "execute"})

(def ^:private execute-node
  {:type :literal :name "execute"
   :children [{:type :literal :name "in" :children [dimension-node]}
              {:type :literal :name "run" :children []
               :redirect :root}]})

(defn- alias-node [[alias target]]
  {:type :literal :name alias :children [] :redirect target})

(defn- flat
  "Returns nodes with node and what it holds added after them.
  A node comes after its children; its index is returned too."
  [nodes node]
  (let [step (fn [[ns ks] c]
               (let [[ns i] (flat ns c)] [ns (conj ks i)]))
        [nodes kids] (reduce step [nodes []] (:children node))]
    [(conj nodes (assoc node :children kids)) (count nodes)]))

(defn- redirected [nodes]
  (let [root (dec (count nodes))
        top (into {} (map (fn [i] [(:name (nth nodes i)) i]))
                  (:children (peek nodes)))
        at #(if (= :root %) root (top %))]
    (mapv #(if (:redirect %) (update % :redirect at) %) nodes)))

(defn tree
  "Returns the command nodes a client gets, the root last."
  []
  (let [top (-> (mapv form-node commands)
                (conj execute-node)
                (into (map alias-node) aliases))]
    (redirected (first (flat [] {:type :root :children top})))))
