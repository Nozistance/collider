(ns collider.game.commands
  (:require [collider.game.mobs :as mobs]
            [collider.game.rules :as rules]
            [clojure.string :as str]
            [collider.data :as data]))

(defn- block-name [kw] (str/replace (name kw) "-" "_"))
(defn- block-kw [s] (keyword (str/replace (str/replace (str/lower-case (str s)) #"^minecraft:" "") "_" "-")))
(set! *warn-on-reflection* true)

(def commands [
   [:time "change or query the time of day"
    [:set "set the time" [[:value [:named-int {:min 0 :max 2147483647
                                               :names {"day" 1000 "night" 13000}}]]]
     [:world :time-set]]
    [:add "advance the time" [[:value [:int {:min 0 :max 2147483647}]]]
     [:world :time-add]]
    [:query "read the clock" [[:clock [:enum {:values #{"daytime" "gametime"}}]]]
     [:world :time-query]]]
   [:gamerule "read or set a game rule"
    [[:rule [:rule {}]]
     [:value [:text {:default nil}]]]
    [:world :gamerule]]
   [:tp "teleport to a position (~ = where you are)"
    [[:x [:dcoord {:axis 0}]]
     [:y [:dcoord {:axis 1}]]
     [:z [:dcoord {:axis 2}]]]
    [:world :tp]]
   [:give "give items to players"
    [[:targets [:targets {:players? true}]]
     [:item [:item {}]]
     [:count [:int {:min 1 :max 6400 :default 1}]]]
    [:world :give]]
   [:kill "kill entities (yourself without a target)"
    [[:targets [:targets {:default {:self true}}]]]
    [:world :kill]]
   [:summon "summon a mob (~ = where you are)"
    [[:entity [:entity-type {}]]
     [:x [:dcoord {:axis 0 :default nil}]]
     [:y [:dcoord {:axis 1 :default nil}]]
     [:z [:dcoord {:axis 2 :default nil}]]]
    [:world :summon]]
   [:setblock "set one block (~ = your position)"
    [[:x [:coord {:min -10000 :max 10000 :axis 0}]]
     [:y [:coord {:min -64 :max 319 :axis 1}]]
     [:z [:coord {:min -10000 :max 10000 :axis 2}]]
     [:block [:block {}]]]
    [:world :setblock]]
   [:setworldspawn "set the world spawn point (default: where you are)"
    [[:x [:coord {:min -10000 :max 10000 :axis 0 :default nil}]]
     [:y [:coord {:min -64 :max 319 :axis 1 :default nil}]]
     [:z [:coord {:min -10000 :max 10000 :axis 2 :default nil}]]]
    [:world :setworldspawn]]
   [:weather "set the weather"
    [:clear "clear the sky" [[:duration [:duration {:min 1 :max 1000000 :default 0}]]]
     [:world :weather-clear]]
    [:rain "let it rain" [[:duration [:duration {:min 1 :max 1000000 :default 0}]]]
     [:world :weather-rain]]
    [:thunder "let it storm" [[:duration [:duration {:min 1 :max 1000000 :default 0}]]]
     [:world :weather-thunder]]]
   [:fill "fill a box with a block (~ = your position)"
    [[:x1 [:coord {:min -10000 :max 10000 :axis 0}]]
     [:y1 [:coord {:min -64 :max 319 :axis 1}]]
     [:z1 [:coord {:min -10000 :max 10000 :axis 2}]]
     [:x2 [:coord {:min -10000 :max 10000 :axis 0}]]
     [:y2 [:coord {:min -64 :max 319 :axis 1}]]
     [:z2 [:coord {:min -10000 :max 10000 :axis 2}]]
     [:block [:block {:default :stone}]]]
    [:world :fill]]])

(defn- subcommands? [form] (keyword? (first (nth form 2))))
(defn- cmd-name [form] (name (first form)))
(defn- find-form [forms nm] (first (filter #(= nm (cmd-name %)) forms)))
(defn- label [[nm [kind {:keys [min max]}]]]
  (case kind
    :duration (str "<" (name nm) ">")
    :int (str "<" (name nm) " " min "-" max ">")
    (str "<" (name nm) ">")))

(defn- parse-long* [^String s]
  (try (Long/parseLong s) (catch NumberFormatException _ nil)))

(defn- in-range [nm n {:keys [min max]}]
  (if (<= (long min) (long n) (long max))
    [:ok n]
    [:err (str (name nm) ": give a value from " min " to " max ", not " n)]))

(defn- as-int [nm s opts _origin]
  (if-let [n (parse-long* s)]
    (in-range nm n opts)
    [:err (str (name nm) ": give a whole number, not \"" s "\"")]))

(defn- as-named-int [nm s {:keys [names] :as opts} _origin]
  (if-let [n (let [k (str/lower-case (str/replace (str s) #"^minecraft:" ""))]
               (or (get names k) (parse-long* s)))]
    (in-range nm n opts)
    [:err (str (name nm) ": give a whole number or "
               (str/join "/" (take 6 (sort (keys names))))
               (when (> (count names) 6) "/...")
               ", not \"" s "\"")]))

(defn- as-coord [nm s {:keys [axis] :as opts} origin]
  (let [rel? (str/starts-with? s "~")
        n    (if rel?
               (when origin
                 (when-let [off (if (= "~" s) 0 (parse-long* (subs s 1)))]
                   (+ (long (Math/floor (double (nth origin axis)))) (long off))))
               (parse-long* s))]
    (cond
      (nil? n) [:err (str (name nm) ": give a whole number or ~, not \"" s "\"")]
      (<= (long (:min opts)) (long n) (long (:max opts))) [:ok n]
      :else [:err (str (name nm) ": " n " is out of " (:min opts) ".." (:max opts))])))

(defn- parse-double* [^String s]
  (try (Double/parseDouble s) (catch NumberFormatException _ nil)))

(defn- as-dcoord [nm s {:keys [axis]} origin]
  (let [rel? (str/starts-with? s "~")
        n    (if rel?
               (when origin
                 (when-let [off (if (= "~" s) 0.0 (parse-double* (subs s 1)))]
                   (+ (double (nth origin axis)) (double off))))
               (parse-double* s))]
    (cond
      (nil? n) [:err (str (name nm) ": give a number or ~, not \"" s "\"")]
      (> (Math/abs (double n)) 3.0E7) [:err (str (name nm) ": " n " is too far")]
      :else [:ok (double n)])))

(defn- as-item [nm s _opts _origin]
  (let [k (block-kw s)]
    (if (contains? (get @data/registries "item") k)
      [:ok k]
      [:err (str (name nm) ": unknown item \"" s "\"")])))

(defn- as-entity-type [nm s _opts _origin]
  (let [k (block-kw s)]
    (if (mobs/mob-type? k)
      [:ok k]
      [:err (str (name nm) ": cannot summon \"" s "\", only " (str/join ", " (sort (map name (keys mobs/types)))))])))

(defn- as-targets [nm s {:keys [players?]} _origin]
  (let [[_ sel args] (re-matches #"@([saep])(?:\[(.*)\])?" s)
        type (when args (some->> (re-find #"type=([a-z_:]+)" args) second block-kw))]
    (cond
      (nil? sel) [:ok {:name s}]
      (and players? (= "e" sel)) [:err (str (name nm) ": @e is not a player")]
      :else [:ok (cond-> ({"s" {:self true} "a" {:all true} "p" {:nearest true} "e" {:entities true}} sel)
                   type (assoc :type type))])))

(defn- as-block [nm s _opts _origin]
  (let [k (block-kw s)]
    (if (contains? @data/blocks k)
      [:ok k]
      [:err (str (name nm) ": unknown block \"" s "\"")])))

(defn- as-enum [nm s {:keys [values]} _origin]
  (if (contains? values s)
    [:ok s]
    [:err (str (name nm) ": give one of " (str/join ", " (sort values)) ", not \"" s "\"")]))

(defn- as-rule [nm s _opts _origin]
  (if-let [r (rules/rule-of s)]
    [:ok r]
    [:err (str (name nm) ": unknown game rule \"" s "\"")]))

(def ^:private time-units {"" 1 "t" 1 "s" 20 "d" 24000})

(defn- as-duration [nm s opts _origin]
  (let [[_ value unit] (re-matches #"(-?[0-9]*\.?[0-9]+)([a-z]*)" (str s))
        factor (get time-units (or unit ""))]
    (cond
      (nil? factor) [:err (str (name nm) ": give a duration in ticks, or with d, s or t, not \"" s "\"")]
      (nil? value) [:err (str (name nm) ": give a duration, not \"" s "\"")]
      :else (in-range nm (Math/round (* (Double/parseDouble value) (double (long factor)))) opts))))

(defn- as-text [_nm s _opts _origin] [:ok s])
(def ^:private coercers
  {:int as-int, :named-int as-named-int, :coord as-coord, :dcoord as-dcoord, :enum as-enum,
   :block as-block, :item as-item, :entity-type as-entity-type, :targets as-targets,
   :rule as-rule, :text as-text, :duration as-duration})

(defn- coerce [[nm [kind opts] :as arg] s origin]
  (if (nil? s)
    (if (contains? opts :default) [:ok (:default opts)] [:err (str "give the argument " (label arg))])
    ((coercers kind as-enum) nm s opts origin)))

(defn- arg-values [[_ [kind {:keys [min max values default axis names]}]] target]
  (case kind
    :duration ["1d" "1s" "100"]
    :int (->> [default min (quot (+ (long min) (long max)) 2) max]
              (remove nil?) (map str) distinct vec)
    :coord (if target [(str (nth target axis))] [])
    :named-int (let [dn (some (fn [[k v]] (when (= v default) k)) names)]
                 (into (if dn [dn] []) (sort (remove #{dn} (keys names)))))
    :enum (vec (sort values))
    :rule (mapv (comp #(subs % 10) rules/wire-name) (keys rules/table))
    :text []
    :dcoord (if target [(str (nth target axis))] [])
    :item (vec (sort (map block-name (keys (get @data/registries "item")))))
    :entity-type (vec (sort (map name (keys mobs/types))))
    :targets ["@s" "@a" "@p" "@e"]
    :block (into [(block-name default)]
                 (remove #{(block-name default)})
                 (sort (map block-name (keys @data/blocks))))))

(defn usage [path]
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

(defn- parse-args [args tokens path origin]
  (loop [as args, ts tokens, acc []]
    (if-let [a (first as)]
      (let [[st v] (coerce a (first ts) origin)]
        (if (= :err st)
          {:error (str v "\n" (usage path))}
          (recur (next as) (next ts) (conj acc v))))
      {:args acc})))

(defn- delta-of [form tokens path origin]
  (let [r (parse-args (nth form 2) tokens path origin)]
    (if (:error r)
      r
      {:delta (into (nth form 3) (:args r))})))

(defn- no-subcommand [form nm sub]
  {:error (str (if sub (str "unknown subcommand \"" sub "\"") "give a subcommand")
               "\n" (str/join "\n" (usage-lines form [nm])))})

(defn- parse-subcommand [form nm [sub & arg-tokens] origin]
  (if-let [sform (find-form (drop 2 form) sub)]
    (delta-of sform arg-tokens [nm sub] origin)
    (no-subcommand form nm sub)))

(defn parse
  ([text] (parse text nil))
  ([text origin]
   (let [[nm & more] (remove str/blank? (str/split (subs text 1) #"\s+"))
         form (find-form commands nm)]
     (cond
       (nil? form) {:error (str "unknown command" (when nm (str " \"/" nm "\"")))}
       (subcommands? form) (parse-subcommand form nm more origin)
       :else (delta-of form more [nm] origin)))))

(defn- starting-with [prefix xs]
  (let [p (str/lower-case prefix)]
    (vec (filter #(str/starts-with? (str/lower-case %) p) xs))))

(defn- suggest-player [world prefix]
  (starting-with prefix (sort (keys (:players world)))))

(defn- suggest-command [prefix]
  (mapv #(str "/" %) (starting-with prefix (sort (map cmd-name commands)))))

(defn- suggest-subcommand [form prefix]
  (starting-with prefix (sort (map cmd-name (drop 2 form)))))

(defn- suggest-arg [form tokens i target]
  (if-let [a (nth (nth form 2 []) i nil)]
    (starting-with (last tokens) (arg-values a target))
    []))

(defn- suggest-after-command [form tokens target]
  (cond
    (not (subcommands? form)) (suggest-arg form tokens (dec (count tokens)) target)
    (= 1 (count tokens)) (suggest-subcommand form (first tokens))
    :else (suggest-arg (find-form (drop 2 form) (first tokens))
                       tokens
                       (- (count tokens) 2)
                       target)))

(defn suggest
  ([world text] (suggest world text nil))
  ([world text target]
   (let [text (or text "")]
     (if-not (str/starts-with? text "/")
       (suggest-player world (last (str/split text #" " -1)))
       (let [[nm & more] (str/split (subs text 1) #" " -1)
             form (find-form commands nm)]
         (cond
           (empty? more) (suggest-command nm)
           (nil? form) []
           :else (suggest-after-command form more target)))))))

(def ^:private brigadier-integer (keyword "brigadier:integer"))
(def ^:private brigadier-double (keyword "brigadier:double"))
(def ^:private brigadier-bool (keyword "brigadier:bool"))
(defn- argument-nodes [[nm [kind {:keys [min max values]}]]]
  (case kind
    :duration [[(name nm) :time {:min 1}]]
    :int [[(name nm) brigadier-integer {:min min :max max}]]
    :named-int [[(name nm) :time {:min 0}]]
    :coord [[(name nm) :block-pos nil]]
    :dcoord [[(name nm) :vec3 nil]]
    :enum {:literals (sort values)}
    :block [[(name nm) :block-state nil]]
    :item [[(name nm) :item-stack nil]]
    :entity-type [[(name nm) :resource {:registry "minecraft:entity_type"}]]
    :targets [[(name nm) :entity {:single? false :players? false}]]
    :text [[(name nm) (keyword "brigadier:string") {:kind 0}]]
    :rule {:rules true}))

(defn- coords-merged [args]
  (loop [as args acc []]
    (if-let [[nm [kind opts] :as a] (first as)]
      (if (and (#{:coord :dcoord} kind) (= 0 (long (:axis opts 0))))
        (recur (drop 3 as) (conj acc [nm [kind opts]]))
        (recur (rest as) (conj acc a)))
      acc)))

(defn- optional-from [args]
  (or (first (keep-indexed (fn [i _] (when (every? (fn [[_ [_ o]]] (contains? o :default)) (drop i args)) i)) args))
      (count args)))

(declare add-chain)

(defn- add-node! [nodes node]
  (swap! nodes conj node)
  (dec (count @nodes)))

(defn- add-rules! [nodes]
  (vec (for [[rule {:keys [type min max]}] rules/table
             :let [value (add-node! nodes {:type :argument :name "value" :executable? true
                                          :parser (if (= :bool type) brigadier-bool brigadier-integer)
                                          :props (when (= :int type) {:min min :max max})})]]
         (add-node! nodes {:type :literal :name (subs (rules/wire-name rule) 10) :executable? true :children [value]}))))

(defn- add-chain [nodes args i]
  (let [optional (optional-from args)]
    (if (>= i (count args))
      [[] true]
      (let [a (nth args i)
            spec (argument-nodes a)
            [tail-children tail-exec] (if (:rules spec) [[] true] (add-chain nodes args (inc i)))
            exec? (or tail-exec (>= (inc i) optional))]
        (cond
          (:rules spec) [(add-rules! nodes) (>= i optional)]
          (:literals spec)
          [(vec (for [l (:literals spec)]
                  (add-node! nodes {:type :literal :name l :executable? exec? :children tail-children})))
           (>= i optional)]
          :else
          [[(add-node! nodes (let [[n parser props] (first spec)]
                               {:type :argument :name n :parser parser :props props
                                :executable? exec? :children tail-children}))]
           (>= i optional)])))))

(defn- add-form! [nodes form]
  (if (subcommands? form)
    (add-node! nodes {:type :literal :name (cmd-name form)
                      :children (vec (map #(add-form! nodes %) (drop 2 form)))})
    (let [args (coords-merged (nth form 2))
          [children exec?] (add-chain nodes args 0)]
      (add-node! nodes {:type :literal :name (cmd-name form) :executable? exec? :children children}))))

(defn tree []
  (let [nodes (atom [])
        top (vec (map #(add-form! nodes %) commands))]
    (add-node! nodes {:type :root :children top})
    @nodes))
