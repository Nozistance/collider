(ns collider.game.command.tree
  "Player commands, their arguments and their meaning."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.world.env.dimension :as dimension]
            [collider.game.clock :as clock]
            [collider.game.command.args :as args]
            [collider.game.command.block-args :as blocks]
            [collider.game.command.dispatcher :as d]
            [collider.game.command.item-args :as items]
            [collider.game.game-mode :as game-mode]
            [collider.game.command.reader :as r]
            [collider.game.command.selector :as sel]
            [collider.game.command.snbt :as snbt]
            [collider.game.command.text-codec :as tc]
            [collider.game.out :as out]
            [collider.game.gamerules :as rules]
            [collider.game.schema :as schema]))

(set! *warn-on-reflection* true)

(defn- block-name [kw] (data/snake kw))

(def ^:private int-max 2147483647)

(def ^:private int-min -2147483648)

(def ^:const gamemaster
  "The permission level most commands require."
  2)

(def ^:private open-commands
  "The commands every player may run, as Commands.java requires."
  #{"me" "msg" "tell" "w" "list" "help"})

(defn level-of
  "Returns the permission level command nm requires."
  ^long [nm]
  (if (open-commands nm) 0 gamemaster))

(defn- allowed? [level nm]
  (>= (long (or level gamemaster)) (level-of nm)))

(defn- selectors? [opts]
  (>= (long (or (:level opts) gamemaster)) gamemaster))

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

(defn- mode-arg [values]
  [:mode [:enum {:values values :default nil}]])

(defn- box-args []
  (-> (pos-args :x1 :y1 :z1 {:node "from"})
      (into (pos-args :x2 :y2 :z2 {:node "to"}))
      (conj [:block [:block {}]])))

(def ^:private spectator-opts
  {:single? true :players? true :default :self})

(defn- lit [nm] [(keyword nm) [:literal {:name nm}]])

(def ^:private clone-then
  [:then [:enum {:values #{"force" "move" "normal"} :default nil}]])

(defn- clone-way [[from? to? strict? filtered?]]
  [(cond-> []
     from? (conj (lit "from") [:sourceDimension [:dimension {}]])
     :always (into (pos-args :x1 :y1 :z1 {:node "begin"}))
     :always (into (pos-args :x2 :y2 :z2 {:node "end"}))
     to? (conj (lit "to") [:targetDimension [:dimension {}]])
     :always (into (pos-args :x :y :z {:node "destination"}))
     strict? (conj (lit "strict"))
     filtered? (conj (lit "filtered") [:filter [:block-predicate {}]])
     (not filtered?) (conj (mode-arg #{"masked" "replace"}))
     :always (conj clone-then))
   [:world :clone {:from? from? :to? to? :strict? strict?
                   :filtered? filtered?}]])

(def ^:private clone-form
  (into [:clone "copy a box of blocks to another place"]
        (mapcat clone-way)
        (for [from? [false true] to? [false true]
              strict? [false true] filtered? [false true]]
          [from? to? strict? filtered?])))

(def ^:private clock-ways
  [["set" [[:time [:ticks {:min 0}]]] :time-set]
   ["set" [[:timemarker [:marker {}]]] :time-marker]
   ["add" [[:time [:ticks {:min int-min}]]] :time-add]
   ["pause" [] :time-pause]
   ["resume" [] :time-resume]
   ["rate" [[:rate [:float {:min 1.0E-5 :max 1000.0}]]] :time-rate]
   ["query" [(lit "time")] :time-query]
   ["query" [[:timeline [:timeline {}]]] :time-timeline]
   ["query" [[:timeline [:timeline {}]] (lit "repetition")]
    :time-repetition]])

(defn- own-clock [[nm [kind opts]]]
  [nm [kind (cond-> opts (#{:marker :timeline} kind)
              (assoc :clock-arg 0))]])

(defn- of-way [[sub as op]]
  [(into [[:clock [:clock {}]] (lit sub)] (map own-clock) as)
   [:world op]])

(defn- default-form [sub]
  (into [(keyword sub) (str sub " the clock of this level")]
        (comp (filter #(= sub (first %)))
              (mapcat (fn [[_ as op]] [as [:world op nil]])))
        clock-ways))

(def ^:private time-form
  (-> [:time "change or query the clocks"]
      (into (map default-form)
            ["set" "add" "pause" "resume" "rate"])
      (conj (conj (default-form "query")
                  [(lit "gametime")] [:world :time-gametime]))
      (conj (into [:of "change or query a clock"]
                  (mapcat of-way) clock-ways))))

(def ^:private players-arg
  [:targets [:targets {:players? true}]])

(defn- title-way [kind]
  [[players-arg (lit kind) [:title [:component {}]]]
   [:world (keyword (str "title-" kind))]])

(defn- bare-title-way [kind]
  [[players-arg (lit kind)] [:world (keyword (str "title-" kind))]])

(def ^:private title-form
  (-> [:title "show titles to players"]
      (into (mapcat bare-title-way) ["clear" "reset"])
      (into (mapcat title-way) ["title" "subtitle" "actionbar"])
      (conj [players-arg (lit "times") [:fadeIn [:ticks {:min 0}]]
             [:stay [:ticks {:min 0}]]
             [:fadeOut [:ticks {:min 0}]]]
            [:world :title-times])))

(def ^:private sound-arg [:sound [:sound {}]])

(defn- float-arg [nm lo hi]
  [nm [:float {:min lo :max hi :default nil}]])

(def ^:private sound-tail
  (-> [[:targets [:targets {:players? true :default nil}]]]
      (into (vec-args {:default nil :node "pos"}))
      (conj (float-arg :volume 0.0 (double Float/MAX_VALUE))
            (float-arg :pitch 0.0 2.0)
            (float-arg :minVolume 0.0 1.0))))

(defn- playsound-way [src]
  [(into [sound-arg (lit src)] sound-tail) [:world :playsound src]])

(def ^:private playsound-form
  (into [:playsound "play a sound to players"
         [sound-arg] [:world :playsound nil]]
        (mapcat playsound-way)
        out/sound-sources))

(defn- stop-way [src sound]
  [[players-arg (lit src) sound] [:world :stopsound src]])

(def ^:private stopsound-form
  (-> [:stopsound "stop sounds of players"
       [players-arg] [:world :stopsound nil]]
      (into (stop-way "*" sound-arg))
      (into (mapcat #(stop-way % [:sound [:sound {:default nil}]]))
            out/sound-sources)))

(def ^:private entities-arg [:targets [:targets {}]])

(def ^:private tag-form
  [:tag "tag entities"
   [entities-arg (lit "add") [:name [:word {}]]] [:world :tag-add]
   [entities-arg (lit "remove") [:name [:word {:tags true}]]]
   [:world :tag-remove]
   [entities-arg (lit "list")] [:world :tag-list]])

(defn- targets-at []
  (into [[:targets [:targets {}]]] (vec-args {:node "location"})))

(def ^:private one-arg [:target [:targets {:single? true}]])

(def ^:private rotate-form
  [:rotate "turn an entity"
   [one-arg [:yaw [:angle {:axis 0 :node "rotation"}]]
    [:pitch [:angle {:axis 1 :node "rotation"}]]]
   [:world :rotate]
   [one-arg (lit "facing") (lit "entity")
    [:facingEntity [:targets {:single? true}]]
    [:facingAnchor [:anchor {:default nil}]]]
   [:world :rotate-facing-entity]
   (into [one-arg (lit "facing")] (vec-args {:node "facingLocation"}))
   [:world :rotate-facing]])

(def ^:private swing-form
  [:swing "swing the arms of living entities (default: yours)"
   [[:targets [:targets {:default nil}]]] [:world :swing nil]
   [entities-arg (lit "mainhand")] [:world :swing :main]
   [entities-arg (lit "offhand")] [:world :swing :off]])

(def ^:private chat-forms
  [[:say "say a message to everyone"
    [[:message [:message {}]]] [:world :say]]
   [:me "tell everyone what you do"
    [[:action [:message {}]]] [:world :me]]
   [:msg "whisper to players"
    [players-arg [:message [:message {}]]] [:world :msg]]
   [:tellraw "show a text component to players"
    [players-arg [:message [:component {}]]] [:world :tellraw]]
   [:help "show how to use the commands"
    [] [:world :help] [[:command [:greedy {}]]] [:world :help]]
   [:list "list the players online"
    [] [:world :list] [(lit "uuids")] [:world :list-uuids]]
   title-form playsound-form stopsound-form
   [:clear "clear items from players (default: yours, all)"
    [[:targets [:targets {:players? true :default nil}]]
     [:item [:item-predicate {:default nil}]]
     [:maxCount [:int {:min 0 :max int-max :default nil
                       :quiet true}]]]
    [:world :clear]]
   tag-form rotate-form swing-form
   [:version "show the version of the game" [] [:world :version]]])

(def ^:private base-forms
  [time-form
   [:gamerule "read or set a game rule"
    [[:rule [:rule {}]]
     [:value [:text {:default nil}]]]
    [:world :gamerule]]
   [:teleport "teleport to a position (~ = where you are) or entity"
    (vec-args {:node "location"})
    [:world :tp]
    [[:destination [:targets {:single? true}]]]
    [:world :tp-to]
    (targets-at)
    [:world :tp-targets]
    (conj (targets-at) [:yaw [:angle {:axis 0 :node "rotation"}]]
          [:pitch [:angle {:axis 1 :node "rotation"}]])
    [:world :tp-targets-rotated]
    (conj (targets-at) (lit "facing") (lit "entity")
          [:facingEntity [:targets {:single? true}]]
          [:facingAnchor [:anchor {:default nil}]])
    [:world :tp-targets-facing-entity]
    (-> (targets-at) (conj (lit "facing"))
        (into (vec-args {:node "facingLocation"})))
    [:world :tp-targets-facing]
    [[:targets [:targets {}]]
     [:destination [:targets {:single? true}]]]
    [:world :tp-targets-to]]
   [:give "give items to players"
    [[:targets [:targets {:players? true}]]
     [:item [:item {}]]
     [:count [:int {:min 1 :max int-max :default 1}]]]
    [:world :give]]
   [:kill "kill entities (yourself without a target)"
    [[:targets [:targets {:default :self}]]]
    [:world :kill]]
   [:summon "summon a mob (~ = where you are)"
    (into [[:entity [:entity-type {}]]]
          (vec-args {:default nil :node "pos"}))
    [:world :summon]]
   [:setblock "set one block (~ = your position)"
    (conj (pos-args) [:block [:block {}]]
          (mode-arg #{"destroy" "keep" "replace" "strict"}))
    [:world :setblock]]
   [:setworldspawn
    "set the world spawn point (default: where you are)"
    (into (pos-args :x :y :z {:default nil :node "pos"})
          (turn-args))
    [:world :setworldspawn]]
   [:spawnpoint "set respawn points (default: yours, here)"
    (-> [[:targets
          [:targets {:players? true :default :self}]]]
        (into (pos-args :x :y :z {:default nil :node "pos"}))
        (into (turn-args)))
    [:world :spawnpoint]]
   [:weather "set the weather"
    (weather-form :clear "clear the sky" :weather-clear)
    (weather-form :rain "let it rain" :weather-rain)
    (weather-form :thunder "let it storm" :weather-thunder)]
   [:fill "fill a box with a block (~ = your position)"
    (conj (box-args) (mode-arg #{"destroy" "hollow" "keep" "outline"
                                 "replace" "strict"}))
    [:world :fill]
    (conj (box-args) [:mode [:enum {:values #{"replace"}}]]
          [:filter [:block-predicate {}]]
          [:then [:enum {:values #{"destroy" "hollow" "outline"
                                   "strict"}
                         :default nil}]])
    [:world :fill-where]]
   clone-form
   [:reload "reread config.edn" [] [:world :reload]]
   [:gamemode "set the game mode of players (default: yours)"
    [[:gamemode [:game-mode {}]]
     [:target [:targets {:players? true :default :self}]]]
    [:world :gamemode]]
   [:spectate "look through another entity (none: stop)"
    [[:target [:targets {:single? true :default nil}]]
     [:player [:targets spectator-opts]]]
    [:world :spectate]]
   [:effect "give or clear mob effects"
    [:clear "clear effects (default: yours, all)"
     [[:targets [:targets {:default nil}]]
      [:effect [:mob-effect {:default nil}]]]
     [:world :effect-clear]]
    [:give "give an effect"
     [[:targets [:targets {}]]
      [:effect [:mob-effect {}]]
      [:seconds [:effect-seconds {:default nil}]]
      [:amplifier [:int {:min 0 :max 255 :default nil :quiet true}]]
      [:hideParticles [:bool {:default nil}]]]
     [:world :effect-give]]]
   [:experience "add, set or read the experience of players"
    [:add "give points or levels"
     [[:target [:targets {:players? true}]]
      [:amount [:int {:min int-min :max int-max}]]
      [:unit [:enum {:values #{"points" "levels"} :default nil}]]]
     [:world :xp-add]]
    [:query "read the points or levels of a player"
     [[:target [:targets {:players? true :single? true}]]
      [:unit [:enum {:values #{"points" "levels"}}]]]
     [:world :xp-query]]
    [:set "set the points or levels"
     [[:target [:targets {:players? true}]]
      [:amount [:int {:min 0 :max int-max}]]
      [:unit [:enum {:values #{"points" "levels"} :default nil}]]]
     [:world :xp-set]]]
   [:defaultgamemode "set the game mode of new players"
    [[:gamemode [:game-mode {}]]]
    [:world :defaultgamemode]]])

(def commands (into base-forms chat-forms))

(defn- subcommands? [form] (keyword? (first (nth form 2))))

(defn- cmd-name [form] (name (first form)))

(defn- find-form [forms nm]
  (first (filter #(= nm (cmd-name %)) forms)))

(defn- label [[nm [kind {:keys [min max]}]]]
  (case kind
    :duration (str "<" (name nm) ">")
    :int (str "<" (name nm) " " min "-" max ">")
    (str "<" (name nm) ">")))

(defn- ways [form]
  (partition 2 (drop 2 form)))

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

(def ^:private aliases
  {"tp" "teleport" "xp" "experience" "tell" "msg" "w" "msg"})

(defn- dimension-of [s]
  (let [k (data/kebab (str/replace (str s) #"^minecraft:" ""))]
    (some #{k} schema/dims)))

(defn- scale ^double [from to]
  (/ (double (:coordinate-scale (dimension/type-of from)))
     (double (:coordinate-scale (dimension/type-of to)))))

(defn- scaled [origin from to]
  (if (or (nil? origin) (= from to))
    origin
    (let [k (scale from to)]
      [(* k (double (nth origin 0))) (nth origin 1)
       (* k (double (nth origin 2)))])))

(def ^:private brigadier-integer (keyword "brigadier:integer"))

(def ^:private brigadier-bool (keyword "brigadier:bool"))

(def ^:private brigadier-string (keyword "brigadier:string"))

(def ^:private brigadier-float (keyword "brigadier:float"))

(def ^:private ask-server "minecraft:ask_server")

(def ^:private plain-arguments
  {:duration [:time {:min 1}]
   :coord [:block-pos nil]
   :dcoord [:vec3 nil]
   :angle [:rotation nil]
   :block [:block-state nil]
   :item [:item-stack nil]
   :entity-type [:resource {:registry "minecraft:entity_type"}]
   :text [brigadier-string {:kind 0}]
   :game-mode [:gamemode nil]
   :mob-effect [:resource {:registry "minecraft:mob_effect"}]
   :bool [brigadier-bool nil]
   :greedy [brigadier-string {:kind 2}]
   :item-predicate [:item-predicate nil]
   :block-predicate [:block-predicate nil]
   :anchor [:entity-anchor nil]
   :message [:message nil]
   :component [:component nil]
   :dimension [:dimension nil]})

(defn- entity-props [single? players?]
  {:single? (boolean single?) :players? (boolean players?)})

(defn- clock-nodes [n kind {:keys [min max]}]
  (case kind
    :ticks [[n :time {:min min}]]
    :float [[n brigadier-float {:min min :max max}]]
    (:marker :sound) [[n :resource-location nil ask-server]]
    :clock [[n :resource {:registry "minecraft:world_clock"}]]
    :timeline [[n :resource {:registry "minecraft:timeline"}
                ask-server]]))

(defn- argument-nodes [[nm [kind opts]]]
  (let [{:keys [min max values single? players?]} opts
        n (name nm)]
    (if-let [[parser props] (plain-arguments kind)]
      [[n parser props]]
      (case kind
        :int [[n brigadier-integer {:min min :max max}]]
        :effect-seconds
        {:or-literals ["infinite"]
         :arg [n brigadier-integer {:min 1 :max 1000000}]}
        :enum {:literals (sort values)}
        :literal {:literals [(:name opts)]}
        :targets [[n :entity (entity-props single? players?)]]
        :word (cond-> [n brigadier-string {:kind 0}]
                (:tags opts) (conj ask-server)
                :always vector)
        :rule {:rules true}
        (clock-nodes n kind opts)))))

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

(defn- rule-value [{:keys [type min max] :as spec} way]
  (with-meta
    {:type :argument :name "value" :executable? true
     :parser (if (= :bool type) brigadier-bool brigadier-integer)
     :props (when (= :int type) {:min min :max max})}
    {:arg [:value [:rule-value spec]] :way way}))

(defn- rule-nodes [way]
  (vec (for [[rule spec] rules/table
             :let [id (rules/wire-name rule)]
             nm [(subs id 10) id]]
         (with-meta {:type :literal :executable? true :name nm
                     :children [(rule-value spec way)]}
           {:value rule}))))

(defn- literal-nodes [ls exec? children]
  (mapv (fn [l] {:type :literal :name l :executable? exec?
                 :children children})
        ls))

(defn- spec-nodes [spec exec? children way]
  (cond
    (:rules spec) (rule-nodes way)
    (:or-literals spec)
    (into (spec-nodes [(:arg spec)] exec? children way)
          (map #(with-meta % {:value (keyword (:name %))}))
          (literal-nodes (:or-literals spec) exec? children))
    (:literals spec)
    (mapv (fn [l] {:type :literal :name l :executable? exec?
                   :children children})
          (:literals spec))
    :else
    (let [[n parser props suggests] (first spec)]
      [(cond-> {:type :argument :name n :parser parser :props props
                :executable? exec? :children children}
         suggests (assoc :suggests suggests))])))

(defn- chain [way args ^long i]
  (if (>= i (count args))
    [[] true]
    (let [optional (optional-from args)
          a (nth args i)
          spec (argument-nodes a)
          tail (if (:rules spec) [[] true] (chain way args (inc i)))
          [kids tail-exec] tail
          exec? (or tail-exec (>= (inc i) optional))]
      [(mapv #(vary-meta % merge {:arg a :way way})
             (spec-nodes spec exec? kids way))
       (>= i optional)])))

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
              #(merged-nodes (into (vec %) (:children b))))
      (with-meta (if (and (:executable? b) (not (:executable? a)))
                   (meta b)
                   (meta a)))))

(defn- merged-nodes [nodes]
  (reduce (fn [acc n]
            (if-let [i (index-of acc #(same-node? % n))]
              (update acc i merged-node n)
              (conj acc n)))
          [] nodes))

(defn- form-node [form]
  (if (subcommands? form)
    {:type :literal :name (cmd-name form)
     :children (mapv form-node (drop 2 form))}
    (let [ws (ways form)
          chains (mapv #(chain % (coords-merged (first %)) 0) ws)
          way (some (fn [[w c]] (when (second c) w))
                    (map vector ws chains))]
      (with-meta
        {:type :literal :name (cmd-name form)
         :executable? (some? way)
         :children (merged-nodes (into [] (mapcat first) chains))}
        {:way way}))))

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

(defn- flat [nodes node]
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

(defn- root-node [level]
  (let [ok? #(allowed? level %)
        forms (filter (comp ok? cmd-name) commands)
        alias-nodes (into [] (comp (filter (comp ok? key))
                                   (map alias-node))
                          aliases)]
    {:type :root
     :children (-> (mapv form-node forms)
                   (cond-> (ok? "execute") (conj execute-node))
                   (into alias-nodes))}))

(defn tree
  "Returns the command nodes a client of permission level gets, the
  root last."
  ([] (tree gamemaster))
  ([level] (redirected (first (flat [] (root-node level))))))

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
  "The usage of node i as CommandDispatcher.getSmartUsage writes it."
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

(defn- prefixed [prefix xs]
  (let [p (str/lower-case prefix)]
    (vec (filter #(str/starts-with? (str/lower-case %) p) xs))))

(defn- starting-with
  "The completions xs of prefix: brigadier drops the one equal to it."
  [prefix xs]
  (filterv #(not= prefix %) (prefixed prefix xs)))

(defn- suggest-player [world prefix]
  (prefixed prefix (sort (keys (:players world)))))

(defn- sugg [start texts] {:start start :texts texts})

(defn- unit-suggestions [w start]
  (let [res (r/read-float (r/reader w))]
    (when-not (r/error? res)
      (let [n (long (second (second res)))]
        (sugg (+ (long start) n)
              (starting-with (subs w n) ["d" "s" "t"]))))))

(defn- clock-in [opts cx]
  (if (:clock-arg opts)
    (some #(when (= "clock" (:name (:node %))) (:value %))
          (:nodes (:ctx cx)))
    (clock/default-of (:dim cx))))

(defn- marker-ids [opts cx]
  (let [k (clock-in opts cx)]
    (sort (for [[id m] (when k (clock/markers k)) :when (:show? m)]
            id))))

(defn- timeline-ids [opts cx]
  (let [k (clock-in opts cx)]
    (sort (for [[t {c :clock}] (clock/timelines)
                :when (and k (= k c))]
            t))))

(defn- sound-events [] (get (data/registries) "sound_event"))

(defn- item-names []
  (->> (keys (get (data/registries) "item"))
       (map block-name)
       sort
       vec))

(defn- effect-names []
  (->> (keys (data/mob-effects)) (map data/wire) sort vec))


(def ^:private readers
  {:ticks #(args/time-arg (:min %))
   :float #(r/float-arg (:min %) (:max %))
   :marker (fn [_] (args/id-arg))
   :sound (fn [_] (args/id-arg))
   :item-predicate (fn [_] (items/item-predicate-arg))
   :block (fn [_] (blocks/block-state-arg))
   :block-predicate (fn [_] (blocks/block-predicate-arg))
   :clock (fn [_] (args/resource-arg "world_clock"))
   :timeline (fn [_] (args/resource-arg "timeline"))})

(defn- game-mode-read [rd]
  (let [[w end] (r/read-unquoted rd)]
    (if-let [m (game-mode/named w)]
      [m end]
      (r/error-at end "argument.gamemode.invalid" w))))

(defn- anchor-read [rd]
  (let [[w end] (r/read-unquoted rd)]
    (if (#{"eyes" "feet"} w)
      [(keyword w) end]
      (r/error-at rd "argument.anchor.invalid" w))))

(defn- greedy-read [[s _ :as rd]] [(r/remaining rd) [s (count s)]])

(def ^:private message-limit 256)

(def ^:private skipped-selector-errors
  #{"argument.entity.selector.missing"
    "argument.entity.selector.unknown"})

(defn- part-read [rd]
  (let [p (sel/parse rd true) err (:error p)]
    (cond (nil? err) [(:sel p) (:rd p)]
          (skipped-selector-errors (:key err)) nil
          :else err)))

(defn- message-parts [[s n0 :as rd]]
  (loop [[_ i :as rd] rd acc []]
    (if (and (r/can-read? rd) (= \@ (nth s i)))
      (let [res (part-read rd)
            [sel [_ e :as end]] (when (vector? res) res)]
        (cond (nil? res) (recur [s (inc i)] acc)
              (r/error? res) res
              :else (recur end (conj acc [(- i n0) (- e n0) sel]))))
      (if (r/can-read? rd) (recur [s (inc i)] acc) [acc rd]))))

(defn- message-read [[s _ :as rd] cx]
  (let [text (r/remaining rd) n (count text)]
    (cond (> n (long message-limit))
          (r/error "argument.message.too_long" n message-limit)
          (not (selectors? cx))
          [{:text text :parts []} [s (count s)]]
          :else (let [res (message-parts rd)]
                  (if (r/error? res)
                    res
                    [{:text text :parts (first res)}
                     (second res)])))))

(defn- component-read [rd]
  (let [res (snbt/read-tag rd)]
    (if (r/error? res)
      res
      (let [[tag end] res [op v] (tc/text-of tag)]
        (case op
          :ok [v end]
          (r/error-at rd "argument.component.invalid" (str v)))))))

(defn- toplevel [rd] [(first rd) 0])

(defn- too-many [players?]
  (if players? "argument.player.toomany" "argument.entity.toomany"))

(defn- targets-read [{:keys [single? players?]} rd cx]
  (let [p (sel/parse rd (selectors? cx)) s (:sel p)]
    (cond (:error p) (:error p)
          (and single? (> (long (:limit s)) 1))
          (r/error-at (toplevel rd) (too-many players?))
          (and players? (:entities? s) (not (:self? s)))
          (r/error-at (toplevel rd) "argument.player.entities")
          :else [s (:rd p)])))

(defn- rule-value-read [{:keys [type min max]}]
  (let [f (if (= :bool type)
            r/read-boolean
            (:parse (r/int-arg (or min int-min) (or max int-max))))]
    (fn [[s n :as rd]]
      (let [res (f rd)]
        (if (r/error? res)
          res
          [(subs s n (second (second res))) (second res)])))))

(defn- reader-for [[_ [kind opts]]]
  (case kind
    :int (:parse (r/int-arg (:min opts) (:max opts)))
    :effect-seconds (:parse (r/int-arg 1 1000000))
    :coord (:parse (args/block-pos-arg))
    :dcoord (:parse (args/vec3-arg))
    :angle (:parse (args/rotation-arg))
    :duration (:parse (args/time-arg (:min opts)))
    :game-mode game-mode-read
    :anchor anchor-read
    :mob-effect (:parse (args/resource-arg "mob_effect"))
    :entity-type (:parse (args/resource-arg "entity_type"))
    :bool r/read-boolean
    :word r/read-unquoted
    (:greedy :text) greedy-read
    :component component-read
    :item (:parse (items/item-stack-arg))
    :dimension args/read-id
    :rule-value (rule-value-read opts)
    (:parse ((readers kind) opts))))

(defn- arg-parse [[_ [kind opts] :as a]]
  (case kind
    :targets (fn [rd cx _] (targets-read opts rd cx))
    :message (fn [rd cx _] (message-read rd cx))
    (let [f (reader-for a)] (fn [rd _ _] (f rd)))))

(defn- valid? [a s] (not (r/error? ((reader-for a) (r/reader s)))))

(defn- coord-texts [a ^String rem]
  (let [c (if (str/starts-with? rem "^") "^" "~")
        fs (if (= "" rem) [] (str/split rem #" "))
        n (count fs)
        heads (reductions #(str %1 " " %2) (concat fs (repeat 3 c)))
        full (nth heads 2)]
    (when (and (< n 3) (valid? a full))
      (drop n (take 3 heads)))))

(defn- coord-suggestions [a text start]
  (sel/suggest-strings text start (coord-texts a (subs text start))))

(defn- block-suggestions [kind ^String w start]
  (let [res ((reader-for [:b [kind {}]]) (r/reader w))
        whole? (and (not (r/error? res))
                    (= (count w) (second (second res))))]
    (cond
      whole? (when (and (not (str/includes? w "["))
                        (blocks/varying? (first res)))
               (sugg (+ (long start) (count w)) ["["]))
      (str/includes? w "[") nil
      :else (let [ids (blocks/ids (= :block-predicate kind))]
              (sel/suggest-ids w 0 ids "")))))

(defn- shifted [s start] (when s (update s :start + (long start))))

(defn- ids-of [w ids] (sel/suggest-ids w 0 ids ""))

(def ^:private mode-names
  (delay (mapv name (sort-by game-mode/id (keys game-mode/ids)))))

(defn- arg-suggestions [[_ [kind opts]] w start cx]
  (case kind
    (:ticks :duration) (unit-suggestions w start)
    :marker (sugg start (:texts (ids-of w (marker-ids opts cx))))
    :timeline (sugg start (:texts (ids-of w (timeline-ids opts cx))))
    :sound (let [ids (sort (map data/wire (keys (sound-events))))]
             (sugg start (:texts (ids-of w ids))))
    :clock (let [ids (map data/wire (clock/names))]
             (sugg start (:texts (ids-of w ids))))
    (:block :block-predicate)
    (shifted (block-suggestions kind w 0) start)
    :mob-effect (sugg start (:texts (ids-of w (effect-names))))
    :entity-type (sugg start (:texts (ids-of w (sel/entity-types))))
    :item (sugg start (starting-with w (item-names)))
    :game-mode (sugg start (starting-with w @mode-names))
    :anchor (sugg start (starting-with w ["eyes" "feet"]))
    (:bool :rule-value)
    (sugg start (if (= :int (:type opts))
                  []
                  (starting-with w ["false" "true"])))
    nil))

(defn- target-suggestions [text start cx]
  (let [p (sel/parse [text start] (selectors? cx))]
    (sel/suggestions p text (:players cx))))

(defn- arg-suggest [[_ [kind] :as a]]
  (case kind
    (:coord :dcoord)
    (fn [text start _ _] (coord-suggestions a text start))
    :targets (fn [text start _ cx] (target-suggestions text start cx))
    :dimension
    (fn [text start _ _]
      (let [ids (sort (map data/wire schema/dims))]
        (sel/suggest-ids text start ids "")))
    (fn [^String text start ctx cx]
      (let [w (subs text start)]
        (arg-suggestions a w start (assoc cx :ctx ctx))))))

(defn- with-paths [n path]
  (let [p (if (:name n) (conj path (:name n)) path)]
    (-> (assoc n :path p)
        (update :children (fn [cs] (mapv #(with-paths % p) cs))))))

(defn- arg-of [n]
  (or (:arg (meta n))
      (when (= :dimension (:parser n)) [:dimension [:dimension {}]])))

(defn- engine-node [n]
  (let [m (meta n) a (arg-of n)]
    (with-meta
      (cond-> (update n :children #(mapv engine-node %))
        (:executable? n) (assoc :command (:way m))
        (= :argument (:type n))
        (assoc :parse (arg-parse a) :suggest (arg-suggest a)))
      m)))

(defn- usable-top [n]
  (assoc n :usable? #(allowed? (:level %) (:name n))))

(def ^:private ^:table graph
  (delay (let [root (-> (with-paths (root-node 4) [])
                        engine-node
                        (update :children #(mapv usable-top %)))]
           (into {:root root} (map (fn [n] [(:name n) n]))
                 (:children root)))))

(defn- arg-width ^long [[_ [kind]]]
  (case kind (:coord :dcoord) 3 :angle 2 1))

(defn- resolved [[_ [kind]] v src]
  (case kind
    :coord (mapv long (args/block-pos v src))
    :dcoord (args/position v src)
    :angle (mapv (fn [{k :kind x :value}] [(= :relative k) x]) v)
    [v]))

(defn- node-values [{n :node raw :value} src]
  (let [{a :arg lit :value} (meta n)]
    (cond (nil? a) []
          (not= :literal (:type n)) (resolved a raw src)
          (= :literal (first (second a))) []
          :else [(if (some? lit) lit (:name n))])))

(defn- consumed ^long [nodes]
  (transduce (keep #(some-> (:arg (meta (:node %))) arg-width)) + 0
             nodes))

(def ^:private ^:table self-selector
  (delay (:sel (sel/parse (r/reader "@s") true))))

(defn- default-of [[_ [_ {v :default}]]]
  (if (= :self v) @self-selector v))

(defn- delta [{[args action] :command nodes :nodes} src]
  (-> action
      (into (mapcat #(node-values % src)) nodes)
      (into (map default-of) (drop (consumed nodes) args))))

(defn- relative-of [v]
  (if (= :local (:kind (first v)))
    [0 1 2]
    (keep-indexed #(when (= :relative (:kind %2)) %1) v)))

(defn- relative-axes [nodes]
  (let [kind #(first (second (:arg (meta (:node %)))))]
    (set (some #(when (= :dcoord (kind %)) (relative-of (:value %)))
               nodes))))

(defn- moved-to [src raw]
  (if-let [to (dimension-of raw)]
    (assoc src :dim to :pos (scaled (:pos src) (:dim src) to))
    (r/error "argument.dimension.invalid" raw)))

(defn- stop-at-error [x] (if (r/error? x) (reduced x) x))

(defn- modified [src ctx]
  (reduce (fn [src {n :node raw :value}]
            (if (= :dimension (:parser n))
              (stop-at-error (moved-to src raw))
              src))
          src (:nodes ctx)))

(defn- final-source [chain src]
  (reduce #(stop-at-error (modified %1 %2)) src (butlast chain)))

(defn- failed [{:keys [key args cursor]}]
  (cond-> {:failure {:translate key :with (vec args)}}
    (>= (long cursor) 0) (assoc :cursor cursor)))

(defn- meaning [chain src]
  (let [ctx (last chain)]
    (cond-> {:delta (delta ctx src)
             :relative (relative-axes (:nodes ctx))}
      (next chain) (assoc :dim (:dim src) :origin (:pos src)))))

(defn parse
  "Returns the delta the typed command means.
  It also returns the axes written relative to the source as
  :relative. A command that cannot run returns the reason instead: a
  :failure message with the :cursor it points at. Relative
  coordinates count from origin in level dim, local ones from the
  rotation rot too. A command run through a redirect also returns
  the level and origin it runs in as :dim and :origin."
  ([text] (parse text nil))
  ([text origin] (parse text origin :overworld))
  ([text origin dim] (parse text origin dim gamemaster))
  ([text origin dim level] (parse text origin dim level [0.0 0.0]))
  ([text origin dim level rot]
   (let [res (d/parse @graph (subs text 1) 0 {:level level})
         err (d/failure res)
         chain (d/chain (:ctx res))
         src {:pos (or origin [0.0 0.0 0.0]) :rot rot :dim dim}
         src (when-not err (final-source chain src))]
     (cond err (failed err)
           (r/error? src) (failed src)
           :else (meaning chain src)))))

(defn suggestions
  "Returns the completions for half-typed text as :texts that
  replace it from index :start. A player of permission level sees
  the commands it may run."
  ([world text] (suggestions world text nil))
  ([world text target] (suggestions world text target gamemaster))
  ([world text _target level]
   (let [text (or text "")
         start (inc (long (or (str/last-index-of text " ") -1)))
         cx {:level level :dim (:dim world :overworld)
             :players (sort (keys (:players world)))}]
     (if (str/starts-with? text "/")
       (d/suggestions (d/parse @graph text 1 cx) text cx)
       (sugg start (suggest-player world (subs text start)))))))

(defn suggest
  "Returns the texts of the completions for half-typed text."
  ([world text] (suggest world text nil))
  ([world text target] (:texts (suggestions world text target))))

(defn- help-node [nodes text level]
  (let [res (d/parse @graph text 0 {:level level})
        l (peek (:nodes (:ctx res)))]
    (when l (index-of nodes #(= (:path %) (:path (:node l)))))))

(defn help-lines
  "Returns the lines /help shows a player of permission level, for
  command text or for every command when text is nil. Returns nil
  when text names no command."
  [level text _origin]
  (let [root (with-paths (root-node level) [])
        nodes (redirected (first (flat [] root)))
        i (if text (help-node nodes text level) (dec (count nodes)))
        n (when i (nth nodes i))
        head (if text (str "/" text " ") "/")]
    (when n
      (let [opt? (boolean (:executable? n))]
        (mapv #(str head (smart-usage nodes % opt? false))
              (:children n))))))
