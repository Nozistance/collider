(ns collider.game.command.tree
  "Player commands, their arguments and their meaning."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.command.item-args :as items]
            [collider.game.command.reader :as r]
            [collider.game.gamerules :as rules]
            [collider.game.mob.mobs :as mobs]
            [collider.game.schema :as schema])
  (:import (java.util UUID)
           (java.util.regex Matcher)))

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

(defn- turn-args []
  [[:yaw [:angle {:default nil :axis 0 :node "rotation"}]]
   [:pitch [:angle {:default nil :axis 1 :node "rotation"}]]])

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
     [:count [:int {:min 1 :max int-max :default 1}]]]
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
    (into (pos-args :x :y :z {:default nil :node "pos"})
          (turn-args))
    [:world :setworldspawn]]
   [:spawnpoint "set respawn points (default: yours, here)"
    (-> [[:targets
          [:targets {:players? true :default {:self true}}]]]
        (into (pos-args :x :y :z {:default nil :node "pos"}))
        (into (turn-args)))
    [:world :spawnpoint]]
   [:weather "set the weather"
    (weather-form :clear "clear the sky" :weather-clear)
    (weather-form :rain "let it rain" :weather-rain)
    (weather-form :thunder "let it storm" :weather-thunder)]
   [:fill "fill a box with a block (~ = your position)"
    (-> (pos-args :x1 :y1 :z1 {:node "from"})
        (into (pos-args :x2 :y2 :z2 {:node "to"}))
        (conj [:block [:block {}]]))
    [:world :fill]]
   [:reload "reread config.edn" [] [:world :reload]]])

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

(defn- in-range [n {:keys [min max]}]
  (cond
    (< (long n) (long min)) [:fail "argument.integer.low" [min n]]
    (> (long n) (long max)) [:fail "argument.integer.big" [max n]]
    :else [:ok n]))

(defn- int-failure [^String s]
  (if (re-matches #"[-0-9.]+" s)
    [:fail "parsing.int.invalid" [s]]
    [:fail "parsing.int.expected" []]))

(defn- as-int [_nm s opts _origin]
  (if-let [n (parse-int* s)]
    (in-range n opts)
    (int-failure s)))

(defn- name-hint [names]
  (str (str/join "/" (take 6 (sort (keys names))))
       (when (> (count names) 6) "/...")))

(defn- as-named-int [nm s {:keys [names] :as opts} _origin]
  (let [k (str/lower-case (str/replace (str s) #"^minecraft:" ""))]
    (if-let [n (or (get names k) (parse-long* s))]
      (in-range n opts)
      [:err (str (name nm) ": give a whole number or"
                 \space (name-hint names) ", not \"" s "\"")])))

(defn- offset-of [^String s]
  (if (= "~" s) 0.0 (parse-double* (subs s 1))))

(defn- block-coord
  "Returns the block coordinate text s names, or nil.
  A ~ offset may have a fraction."
  [^String s axis origin]
  (if (str/starts-with? s "~")
    (when origin
      (when-let [off (offset-of s)]
        (let [at (+ (double (nth origin axis)) (double off))]
          (long (Math/floor at)))))
    (parse-int* s)))

(defn- as-coord [_nm s {:keys [axis]} origin]
  (if-let [n (block-coord s axis origin)]
    [:ok n]
    (int-failure s)))

(defn- centered ^double [^double n ^String s ^long axis]
  (if (and (not= 1 axis) (not (str/includes? s "."))) (+ n 0.5) n))

(defn- exact-coord [^String s axis origin]
  (if (str/starts-with? s "~")
    (when origin
      (when-let [off (offset-of s)]
        (+ (double (nth origin axis)) (double off))))
    (when-let [v (parse-double* s)]
      (centered (double v) s (long axis)))))

(defn- as-dcoord [_nm ^String s {:keys [axis]} origin]
  (if-let [n (exact-coord s axis origin)]
    [:ok (double n) (str/starts-with? s "~")]
    [:fail "parsing.double.expected" []]))

(defn- as-angle
  "Returns [relative? value] of an angle: relative to the turn of
  the source when it starts with ~."
  [_nm ^String s _opts _origin]
  (let [rel? (str/starts-with? s "~")]
    (if-let [v (if rel? (offset-of s) (parse-double* s))]
      [:ok [rel? (double v)]]
      [:fail "parsing.double.expected" []])))

(defn- fail-at [res]
  [:fail-at (:key res) (:args res) (:cursor res)])

(defn- as-item
  "Returns the item stack input of s, or its error with the cursor
  counted from the start of s."
  [_nm s _opts _origin]
  (let [res ((:parse (items/item-stack-arg)) (r/reader s))
        [v [_ end]] (when-not (r/error? res) res)]
    (cond (r/error? res) (fail-at res)
          (< (long end) (count s))
          [:fail-at "command.expected.separator" [] end]
          :else [:ok v])))

(defn- as-entity-type [nm s _opts _origin]
  (let [k (block-kw s)
        known (str/join ", " (sort (map name (keys mobs/types))))]
    (if (mobs/mob-type? k)
      [:ok k]
      [:err (str (name nm) ": cannot summon \"" s
                 "\", only " known)])))

(def ^:private selectors
  {"s" {:self true} "a" {:all true} "p" {:nearest true}
   "r" {:random true} "e" {:entities true}
   "n" {:entities true :nearest true}})

(defn- uuid-of [^String s]
  (try (UUID/fromString s)
       (catch IllegalArgumentException _ nil)))

(defn- range-of
  "Returns [min max] of a range such as 1..5, ..5, 3.. or 3."
  [s]
  (let [[_ a dots b] (re-matches #"([0-9.]*?)(\.\.)?([0-9.]*)" s)
        n #(when (seq %) (parse-double* %))]
    (if dots [(n a) (n b)] [(n b) (n b)])))

(defn- typed [sel ^String v]
  (cond-> (assoc sel :type (block-kw (str/replace v "!" "")))
    (str/starts-with? v "!") (assoc :not-type? true)))

(defn- option [sel ^String o]
  (let [[_ k v] (re-matches #"\s*([a-z_]+)\s*=\s*(\S*)\s*" o)]
    (case k
      "type" (typed sel v)
      "distance" (assoc sel :distance (range-of v))
      (reduced [:fail "argument.entity.options.unknown"
                [(or k (str/trim o))]]))))

(defn- selector-of [c args]
  (let [sel (selectors c)]
    (cond
      (nil? sel)
      [:fail "argument.entity.selector.unknown" [(str "@" c)]]
      (str/blank? args) [:ok sel]
      :else (let [r (reduce option sel (str/split args #","))]
              (if (vector? r) r [:ok r])))))

(defn- name-selector [^String s]
  (cond
    (uuid-of s) [:ok {:uuid (uuid-of s)}]
    (<= 1 (count s) 16) [:ok {:name s}]
    :else [:fail "argument.entity.invalid" []]))

(defn- many? [sel]
  (or (:all sel) (and (:entities sel) (not (:nearest sel)))))

(defn- entities? [sel] (or (:uuid sel) (:entities sel)))

(defn- checked-targets [sel {:keys [players? single?]}]
  (cond
    (and single? (many? sel))
    [:fail (if players? "argument.player.toomany"
               "argument.entity.toomany") [] 0]
    (and players? (entities? sel))
    [:fail "argument.player.entities" [] 0]
    :else [:ok sel]))

(defn- as-targets [_nm s opts _origin]
  (let [[_ c args] (re-matches #"@(.)(?:\[(.*)\])?" s)
        [st sel :as r] (if c (selector-of c args) (name-selector s))]
    (if (= :ok st) (checked-targets sel opts) r)))

(defn- as-block [nm s _opts _origin]
  (let [k (block-kw s)]
    (if (contains? (data/blocks) k)
      [:ok k]
      [:err (str (name nm) ": unknown block \"" s "\"")])))

(defn- as-enum [nm s {:keys [values]} _origin]
  (if (contains? values s)
    [:ok s]
    (let [vs (str/join ", " (sort values))]
      [:err (str (name nm) ": give one of " vs ", not \"" s "\"")])))

(defn- as-rule [nm s _opts _origin]
  (if-let [r (rules/rule-of s)]
    [:ok r]
    [:err (str (name nm) ": unknown game rule \"" s "\"")]))

(def ^:private time-units {"" 1 "t" 1 "s" 20 "d" 24000})

(defn- as-duration [nm s {:keys [min]} _origin]
  (let [re #"(-?[0-9]*\.?[0-9]+)([a-z]*)"
        [_ value unit] (re-matches re (str s))
        factor (get time-units (or unit ""))
        n #(Double/parseDouble value)
        ticks #(Math/round (* (double factor) (double (n))))]
    (cond
      (nil? value)
      [:err (str (name nm) ": give a duration, not \"" s "\"")]
      (nil? factor) [:fail "argument.time.invalid_unit" []]
      (< (long (ticks)) (long min))
      [:fail "argument.time.tick_count_too_low" [min (ticks)]]
      :else [:ok (ticks)])))

(defn- as-text [_nm s _opts _origin] [:ok s])

(def ^:private coercers
  {:int as-int :named-int as-named-int :coord as-coord
   :dcoord as-dcoord :enum as-enum :block as-block :item as-item
   :entity-type as-entity-type :targets as-targets :rule as-rule
   :text as-text :duration as-duration :angle as-angle})

(defn- coerce
  "Returns [:ok value relative?], [:fail key with cursor?],
  [:fail-at key with cursor-in-s] or [:err line] of the text s of
  argument arg; nil s gives its default. A cursor below 0 in s
  means the error points nowhere."
  [[nm [kind opts]] s origin]
  (if (some? s)
    ((coercers kind as-enum) nm s opts origin)
    [:ok (:default opts)]))

(defn- int-values [{:keys [min max default]}]
  (->> [default min (quot (+ (long min) (long max)) 2) max]
       (remove nil?) (map str) distinct vec))

(defn- named-int-values [{:keys [default names]}]
  (let [dn (some (fn [[k v]] (when (= v default) k)) names)]
    (into (if dn [dn] []) (sort (remove #{dn} (keys names))))))

(defn- block-values []
  (vec (sort (map block-name (keys (data/blocks))))))

(defn- coord-values [target axis]
  (if target [(str (nth target axis))] []))

(defn- rule-names []
  (mapv (comp #(subs % 10) rules/wire-name) (keys rules/table)))

(defn- item-names []
  (->> (keys (get (data/registries) "item"))
       (map block-name)
       sort
       vec))

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
    :angle []
    :targets ["@s" "@a" "@p" "@r" "@e" "@n"]
    :block (block-values)))

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

(defn- failure
  "Returns a parse failure: the message of key with, pointing at
  cursor at of the command text when at is given."
  ([key at] (failure key [] at))
  ([key with at]
   (cond-> {:failure {:translate key :with (vec with)}}
     at (assoc :cursor at))))

(defn- unknown-command [cx]
  (failure "command.unknown.command" (:end cx)))

(defn- axis-of [[_ [kind opts]]]
  (when (#{:coord :dcoord :angle} kind) (long (:axis opts 0))))

(defn- missing
  "Returns the failure of an argument a with no text, or nil.
  An axis left out after the first of its group is incomplete."
  [[_ [kind opts] :as a] start cx]
  (cond
    (and start (pos? (long (or (axis-of a) 0))))
    (failure (if (= :angle kind)
               "argument.rotation.incomplete"
               "argument.pos3d.incomplete") start)
    (not (contains? opts :default)) (unknown-command cx)))

(defn- coerced
  "Returns [value relative?] of token [s at] for argument a, or
  {:fail reason}."
  [a [s at] path cx]
  (let [[st v x c] (coerce a s (:origin cx))]
    (case st
      :ok [v x]
      :fail {:fail (failure v x (if (some? c) c at))}
      :fail-at {:fail (failure v x (when (>= (long c) 0) (+ at c)))}
      :err {:fail {:error (str v "\n" (usage path))}})))

(defn- group-start [a [s at] start]
  (let [axis (axis-of a)]
    (cond
      (nil? axis) nil
      (zero? (long axis)) (when s at)
      :else start)))

(defn- leftover [ts acc rel]
  (if-let [[_ at] (first ts)]
    (failure "command.unknown.argument" at)
    {:args acc :relative rel}))

(defn- parse-args
  "Returns the values of args in tokens as :args, and the axes of
  vectors written relative to the source as :relative."
  [args tokens path cx]
  (loop [as args ts tokens acc [] rel #{} start nil]
    (if-let [a (first as)]
      (let [t (first ts) start (group-start a t start)
            m (when-not (first t) (missing a start cx))
            r (when-not m (coerced a t path cx))]
        (cond m m
          (map? r) (:fail r)
          :else (recur (next as) (next ts) (conj acc (first r))
                       (cond-> rel (second r) (conj (axis-of a)))
                       start)))
      (leftover ts acc rel))))

(defn- ways
  "Returns the [args action] pairs of a form without subcommands."
  [form]
  (partition 2 (drop 2 form)))

(defn- way-delta [[args action] tokens path cx]
  (let [r (parse-args args tokens path cx)]
    (if (:args r)
      {:delta (into action (:args r)) :relative (:relative r)}
      r)))

(defn- delta-of [form tokens path cx]
  (let [rs (map #(way-delta % tokens path cx) (ways form))]
    (or (first (filter :delta rs)) (first rs))))

(defn- parse-subcommand [form nm [[sub at] & more] cx]
  (if-let [sform (find-form (drop 2 form) sub)]
    (delta-of sform more [nm sub] cx)
    (if sub
      (failure "command.unknown.argument" at)
      (unknown-command cx))))

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

(defn- parse-in [[[d] & more :as ts] cx dim]
  (if-let [to (dimension-of d)]
    (parse-execute more (update cx :origin scaled dim to) to)
    (if d
      (failure "argument.dimension.invalid" [d] nil)
      (unknown-command cx))))

(defn- parse-execute [[[w at] & more] cx dim]
  (case w
    "in" (parse-in more cx dim)
    "run" (if (seq more)
            (merge {:dim dim :origin (:origin cx)}
                   (parse-words more cx dim))
            (unknown-command cx))
    nil (unknown-command cx)
    (failure "command.unknown.argument" at)))

(defn- parse-words [[[nm at] & more] cx dim]
  (let [nm (get aliases nm nm)
        form (find-form commands nm)]
    (cond
      (= "execute" nm) (parse-execute more cx dim)
      (nil? form) (failure "command.unknown.command" at)
      (subcommands? form) (parse-subcommand form nm more cx)
      :else (delta-of form more [nm] cx))))

(defn- words
  "Returns [word start] of each word of s."
  [^String s]
  (let [m (re-matcher #"\S+" s)]
    (loop [acc []]
      (if (Matcher/.find m)
        (recur (conj acc [(Matcher/.group m) (Matcher/.start m)]))
        acc))))

(defn parse
  "Returns the delta the typed command means, with the axes of its
  vector written relative to the source as :relative.
  Returns the reason it cannot run instead: a :failure message with
  the :cursor it points at in the text after the slash, or an
  :error line. Relative coordinates count from origin, in level
  dim. A command run in another level also returns that level as
  :dim and origin there as :origin."
  ([text] (parse text nil))
  ([text origin] (parse text origin :overworld))
  ([text origin dim]
   (let [s (subs text 1)]
     (parse-words (words s) {:origin origin :end (count s)} dim))))

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
   :angle [:rotation nil]
   :block [:block-state nil]
   :item [:item-stack nil]
   :entity-type [:resource {:registry "minecraft:entity_type"}]
   :text [brigadier-string {:kind 0}]})

(defn- entity-props [single? players?]
  {:single? (boolean single?) :players? (boolean players?)})

(defn- argument-nodes [[nm [kind opts]]]
  (let [{:keys [min max values single? players?]} opts
        n (name nm)]
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
      (if (and (#{:coord :dcoord :angle} kind)
               (= 0 (long (:axis opts 0))))
        (recur (drop (if (= :angle kind) 2 3) as)
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
