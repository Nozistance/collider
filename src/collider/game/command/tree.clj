(ns collider.game.command.tree
  "Player commands, their arguments and their meaning."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.world.env.dimension :as dimension]
            [collider.game.clock :as clock]
            [collider.game.command.args :as args]
            [collider.game.command.item-args :as items]
            [collider.game.game-mode :as game-mode]
            [collider.game.command.reader :as r]
            [collider.game.command.snbt :as snbt]
            [collider.game.command.text-codec :as tc]
            [collider.game.out :as out]
            [collider.game.gamerules :as rules]
            [collider.game.mob.mobs :as mobs]
            [collider.game.schema :as schema])
  (:import (java.util UUID)
           (java.util.regex Matcher)))

(set! *warn-on-reflection* true)

(defn- block-name [kw] (data/snake kw))

(defn- block-kw [s] (data/kebab (str s)))

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

(def ^:private spectator-opts
  {:single? true :players? true :default {:self true}})

(defn- lit [nm] [(keyword nm) [:literal {:name nm}]])

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
   [:reload "reread config.edn" [] [:world :reload]]
   [:gamemode "set the game mode of players (default: yours)"
    [[:gamemode [:game-mode {}]]
     [:target [:targets {:players? true :default {:self true}}]]]
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

(defn- offset-of [^String s]
  (if (= "~" s) 0.0 (parse-double* (subs s 1))))

(defn- block-coord [^String s axis origin]
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

(defn- as-angle [_nm ^String s _opts _origin]
  (let [rel? (str/starts-with? s "~")]
    (if-let [v (if rel? (offset-of s) (parse-double* s))]
      [:ok [rel? (double v)]]
      [:fail "parsing.double.expected" []])))

(defn- fail-at [res]
  [:fail-at (:key res) (:args res) (:cursor res)])

(defn- as-item [_nm s _opts _origin]
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

(defn- range-of [s]
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
    (cond
      (and (str/starts-with? s "@") (not (selectors? opts)))
      [:fail-at "argument.entity.selector.not_allowed" [] 0]
      (= :ok st) (checked-targets sel opts)
      :else r)))

(def ^:private message-limit 256)

(defn- option-at ^long [^String opts [k]]
  (+ 3 (long (or (str/index-of opts (str k)) 0))))

(defn- message-part [^String s ^long i]
  (let [[m c opts] (re-find #"^@(.)(?:\[([^\]]*)\])?" (subs s i))]
    (when (and m (selectors c))
      (let [[st sel k] (selector-of c opts)]
        (if (= :ok st)
          [i (+ i (count m)) sel]
          [:fail-at sel k (+ i (option-at opts k))])))))

(defn- message-parts [^String s]
  (loop [i 0 acc []]
    (let [j (str/index-of s "@" i)
          p (when j (message-part s j))]
      (cond (nil? j) [:ok {:text s :parts acc}]
            (nil? p) (recur (inc (long j)) acc)
            (= :fail-at (first p)) p
            :else (recur (long (second p)) (conj acc p))))))

(defn- as-message [_nm ^String s opts _origin]
  (cond
    (> (count s) (long message-limit))
    [:fail-at "argument.message.too_long"
     [(count s) message-limit] -1]
    (selectors? opts) (message-parts s)
    :else [:ok {:text s :parts []}]))

(defn- trailing [^String s ^long end]
  (if (= \space (nth s end))
    [:fail-at "command.unknown.argument" [] (inc end)]
    [:fail-at "command.expected.separator" [] end]))

(defn- decoded [tag end s]
  (let [[op v] (tc/text-of tag)]
    (case op
      :ok (if (< (long end) (count s)) (trailing s end) [:ok v])
      :raw [:err "text: this component is not supported yet"]
      [:fail-at "argument.component.invalid" [v] 0])))

(defn- as-component [_nm ^String s _opts _origin]
  (let [res (snbt/read-tag (r/reader s))]
    (if (r/error? res)
      (fail-at res)
      (let [[tag [_ end]] res] (decoded tag end s)))))

(def ^:private word-chars #"^[0-9A-Za-z_.+-]*")

(defn- as-word [_nm ^String s _opts _origin]
  (let [w (re-find word-chars s)]
    (if (< (count w) (count s))
      [:fail-at "command.expected.separator" [] (count w)]
      [:ok w])))

(defn- as-anchor [_nm ^String s _opts _origin]
  (let [w (re-find word-chars s)]
    (cond
      (not (#{"eyes" "feet"} w))
      [:fail-at "argument.anchor.invalid" [w] 0]
      (< (count w) (count s))
      [:fail-at "command.expected.separator" [] (count w)]
      :else [:ok (keyword w)])))

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

(defn- as-game-mode [_nm ^String s _opts _origin]
  (let [w (re-find #"^[0-9A-Za-z_.+-]*" s)
        mode (game-mode/named w)]
    (cond
      (nil? mode) [:fail-at "argument.gamemode.invalid" [w] (count w)]
      (< (count w) (count s))
      [:fail-at "command.expected.separator" [] (count w)]
      :else [:ok mode])))

(def ^:private id-chars #"^[0-9a-z_:/.-]*")

(defn- as-mob-effect [_nm ^String s _opts _origin]
  (let [w (re-find id-chars s)
        [_ ns path] (re-matches #"(?:([^:]*):)?([^:]*)" w)
        k (when path (data/kebab (str (or ns "minecraft") ":" path)))
        id (str (or ns "minecraft") ":" path)]
    (cond
      (or (nil? path) (str/includes? (str ns) "/"))
      [:fail-at "argument.id.invalid" [] 0]
      (not (contains? (data/mob-effects) k))
      [:fail-at "argument.resource.not_found"
       [id "minecraft:mob_effect"] (count w)]
      (< (count w) (count s))
      [:fail-at "command.expected.separator" [] (count w)]
      :else [:ok k])))

(defn- as-effect-seconds [nm ^String s _opts origin]
  (if (= "infinite" s)
    [:ok :infinite]
    (as-int nm s {:min 1 :max 1000000} origin)))

(defn- as-bool [_nm ^String s _opts _origin]
  (let [w (re-find #"^[0-9A-Za-z_.+-]*" s)]
    (cond
      (= "" w) [:fail "parsing.bool.expected" []]
      (not (#{"true" "false"} w)) [:fail "parsing.bool.invalid" [w]]
      (< (count w) (count s))
      [:fail-at "command.expected.separator" [] (count w)]
      :else [:ok (= "true" w)])))

(defn- as-read [_nm ^String s {:keys [arg]} _origin]
  (let [res ((:parse arg) (r/reader s))
        [v [_ end]] (when-not (r/error? res) res)]
    (cond (r/error? res) (fail-at res)
          (< (long end) (count s))
          [:fail-at "command.expected.separator" [] end]
          :else [:ok v])))

(defn- as-literal [_nm s {:keys [name]} _origin]
  (if (= name s) [:ok s] [:miss]))

(def ^:private readers
  {:ticks #(args/time-arg (:min %))
   :float #(r/float-arg (:min %) (:max %))
   :marker (fn [_] (args/id-arg))
   :sound (fn [_] (args/id-arg))
   :item-predicate (fn [_] (items/item-predicate-arg))
   :clock (fn [_] (args/resource-arg "world_clock"))
   :timeline (fn [_] (args/resource-arg "timeline"))})

(defn- as-kind [kind]
  (fn [nm s opts origin]
    (as-read nm s {:arg ((readers kind) opts)} origin)))

(def ^:private coercers
  {:int as-int :coord as-coord
   :dcoord as-dcoord :enum as-enum :block as-block :item as-item
   :entity-type as-entity-type :targets as-targets :rule as-rule
   :text as-text :duration as-duration :angle as-angle
   :game-mode as-game-mode :mob-effect as-mob-effect
   :effect-seconds as-effect-seconds :bool as-bool
   :literal as-literal :ticks (as-kind :ticks)
   :message as-message :component as-component :greedy as-text
   :word as-word :anchor as-anchor
   :float (as-kind :float) :marker (as-kind :marker)
   :sound (as-kind :sound) :item-predicate (as-kind :item-predicate)
   :clock (as-kind :clock) :timeline (as-kind :timeline)})

(defn- coerce
  [[nm [kind opts]] s origin]
  (if (some? s)
    ((coercers kind as-enum) nm s opts origin)
    [:ok (:default opts)]))

(defn- int-values [{:keys [min max default]}]
  (->> [default min (quot (+ (long min) (long max)) 2) max]
       (remove nil?) (map str) distinct vec))

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

(defn- effect-names []
  (->> (keys (data/mob-effects)) (map data/wire) sort vec))

(def ^:private fixed-values
  {:duration ["1d" "1s" "100"] :effect-seconds ["infinite"]
   :bool ["false" "true"] :text [] :angle [] :message [] :greedy []
   :word [] :anchor ["eyes" "feet"]
   :component []
   :targets ["@s" "@a" "@p" "@r" "@e" "@n"]})

(defn- arg-values [[_ [kind {:keys [values axis] :as opts}]] target]
  (case kind
    (:duration :effect-seconds :bool :text :angle :targets :message
     :component :greedy :word :anchor)
    (fixed-values kind)
    :int (if (:quiet opts) [] (int-values opts))
    :mob-effect (effect-names)
    (:coord :dcoord) (coord-values target axis)
    :enum (vec (sort values))
    :rule (rule-names)
    :item (item-names)
    :item-predicate []
    :entity-type (vec (sort (map name (keys mobs/types))))
    :game-mode (mapv name (sort-by game-mode/id (keys game-mode/ids)))
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
  ([key at] (failure key [] at))
  ([key with at]
   (cond-> {:failure {:translate key :with (vec with)}}
     at (assoc :cursor at))))

(defn- unknown-argument [at]
  (failure "command.unknown.argument" at))

(defn- unknown-command [cx]
  (failure "command.unknown.command" (:end cx)))

(defn- axis-of [[_ [kind opts]]]
  (when (#{:coord :dcoord :angle} kind) (long (:axis opts 0))))

(defn- missing
  [[_ [kind opts] :as a] start cx]
  (cond
    (and start (pos? (long (or (axis-of a) 0))))
    (failure (if (= :angle kind)
               "argument.rotation.incomplete"
               "argument.pos3d.incomplete") start)
    (not (contains? opts :default)) (unknown-command cx)))

(defn- coerced
  [a [s at] path cx]
  (let [a (update-in a [1 1] assoc :level (:level cx))
        [st v x c] (coerce a s (:origin cx))]
    (case st
      :ok [v x]
      :miss {:fail (assoc (unknown-argument at) :miss? true)}
      :fail {:fail (failure v x (if (some? c) c at))}
      :fail-at {:fail (failure v x (when (>= (long c) 0) (+ at c)))}
      :err {:fail {:error (str v "\n" (usage path))}})))

(defn- group-start [a [s at] start]
  (let [axis (axis-of a)]
    (cond
      (nil? axis) nil
      (zero? (long axis)) (when s at)
      :else start)))

(defn- leftover [ts acc rel i]
  (if-let [[_ at] (first ts)]
    (assoc (unknown-argument at) :depth i :miss? true)
    {:args acc :relative rel}))

(defn- marked [f _args i] (assoc f :depth i))

(defn- missing-at [a start cx args i]
  (when-let [m (missing a start cx)]
    (cond-> (assoc (marked m args i) :end? true)
      (= (unknown-command cx) m) (assoc :missing? true))))

(defn- kept [acc [_ [kind]] v]
  (if (= :literal kind) acc (conj acc v)))

(def ^:private rest-kinds #{:message :component :greedy})

(defn- rest-token
  "The input from token t to the end, as a greedy argument reads it."
  [[_ at] {:keys [text end]}]
  (let [start (if at
                (inc (count (str/trimr (subs text 0 at))))
                (inc (long end)))]
    (when (< start (count text)) [(subs text start) start])))

(defn- token-of [[_ [kind]] ts cx]
  (if (rest-kinds kind) (rest-token (first ts) cx) (first ts)))

(defn- parse-args [args tokens path cx]
  (loop [i 0 ts tokens acc [] rel #{} start nil]
    (if-let [a (nth args i nil)]
      (let [t (token-of a ts cx) start (group-start a t start)
            m (when-not (first t) (missing-at a start cx args i))
            r (when-not m (coerced a t path cx))
            more (if (rest-kinds (first (second a))) nil (next ts))]
        (cond m m
          (map? r) (marked (:fail r) args i)
          :else (recur (inc i) more (kept acc a (first r))
                       (cond-> rel (second r) (conj (axis-of a)))
                       start)))
      (leftover ts acc rel i))))

(defn- ways [form]
  (partition 2 (drop 2 form)))

(defn- greedy-way? [args]
  (some (fn [[_ [kind]]] (rest-kinds kind)) args))

(defn- way-delta [[args action] tokens path cx]
  (let [r (parse-args args tokens path cx)]
    (if (:args r)
      (cond-> {:delta (into action (:args r)) :relative (:relative r)}
        (greedy-way? args) (assoc :greedy true))
      r)))

(defn- node-at [[args] i]
  (when-let [[nm [kind opts]] (nth (vec args) i nil)]
    [nm kind (:name opts)]))

(defn- span
  "How many words the node at i reads: a position is one node."
  ^long [[args] i]
  (let [[_ [kind] :as a] (nth (vec args) i nil)]
    (cond (not= 0 (axis-of a)) 1
          (= :angle kind) 2
          :else 3)))

(defn- passed? [[w r] i]
  (or (:delta r) (> (long (:depth r)) (+ (long i) (span w i) -1))))

(defn- rank
  "Brigadier's order of parse results: read to the end first, then
  those without an error."
  [r]
  [(if (or (:delta r) (:end? r)) 0 1)
   (if (or (:delta r) (:missing? r) (:miss? r)) 0 1)])

(declare index-of)

(defn- grouped [pairs i]
  (reduce (fn [acc [w :as p]]
            (let [n (node-at w i) j (index-of acc #(= n (first %)))]
              (cond (nil? n) acc
                    j (update-in acc [j 1] conj p)
                    :else (conj acc [n [p]]))))
          [] pairs))

(defn- relevant [groups word]
  (let [lit? (fn [[[_ k]]] (= :literal k))
        hit (filter (fn [[[_ _ l] :as g]] (and (lit? g) (= l word)))
                    groups)]
    (if (seq hit) hit (remove lit? groups))))

(declare chosen)

(defn- after-group
  "The token after group g at i: a greedy argument reads them all."
  [[[_ kind]] i tokens]
  (if (rest-kinds kind) (count tokens) (inc (long i))))

(defn- failed-at [groups at]
  (if (= 1 (count groups))
    (second (first (second (first groups))))
    (assoc (unknown-argument at) :miss? (empty? groups))))

(defn- chosen
  "The result brigadier keeps of the ways in pairs, [way result],
  that agree up to token i."
  [pairs i tokens]
  (if (= i (count tokens))
    (let [rs (map second pairs)]
      (or (first (filter :delta rs)) (first rs)))
    (let [[word at] (nth tokens i)
          gs (relevant (grouped pairs i) word)
          ok (filter #(passed? (first (second %)) i) gs)
          deeper #(chosen (second %) (after-group % i tokens) tokens)]
      (if (seq ok)
        (first (sort-by rank (map deeper ok)))
        (failed-at gs at)))))

(defn- delta-of [form tokens path cx]
  (let [ws (ways form)
        rs (map #(way-delta % tokens path cx) ws)]
    (-> (chosen (map vector ws rs) 0 (vec tokens))
        (dissoc :depth :miss? :missing? :end?))))

(defn- parse-subcommand [form nm [[sub at] & more] cx]
  (if-let [sform (find-form (drop 2 form) sub)]
    (delta-of sform more [nm sub] cx)
    (if sub
      (failure "command.unknown.argument" at)
      (unknown-command cx))))

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

(defn- parse-words [[[typed at] & more] cx dim]
  (let [nm (get aliases typed typed)
        form (find-form commands nm)]
    (cond
      (not (allowed? (:level cx) typed))
      (failure "command.unknown.command" at)
      (= "execute" nm) (parse-execute more cx dim)
      (nil? form) (failure "command.unknown.command" at)
      (subcommands? form) (parse-subcommand form nm more cx)
      :else (delta-of form more [nm] cx))))

(defn- words [^String s]
  (let [m (re-matcher #"\S+" s)]
    (loop [acc []]
      (if (Matcher/.find m)
        (recur (conj acc [(Matcher/.group m) (Matcher/.start m)]))
        acc))))

(defn- open-at?
  "True when parse result r read everything up to end: brigadier
  then stops at the space after it."
  [r end]
  (or (contains? r :delta)
      (= (failure "command.unknown.command" end) r)))

(defn- parsed
  "The result of s with its trailing space as brigadier reads it:
  an alias redirects and reads the space as a separator."
  [s origin dim level]
  (let [t (str/trimr s)
        cx {:origin origin :end (count t) :text s :level level}
        r (parse-words (words t) cx dim)]
    (cond
      (= (count t) (count s)) r
      (:greedy r) r
      (contains? aliases t) (unknown-command {:end (count s)})
      (open-at? r (count t)) (unknown-argument (count t))
      :else r)))

(defn parse
  "Returns the delta the typed command means.
  It also returns the axes written relative to the source as
  :relative. A command that cannot run returns the reason instead. The
  reason is a :failure message with the :cursor it points at, or an
  :error line. Relative coordinates count from origin in level dim. A
  command run in another level also returns that level as :dim and
  origin there as :origin."
  ([text] (parse text nil))
  ([text origin] (parse text origin :overworld))
  ([text origin dim] (parse text origin dim gamemaster))
  ([text origin dim level]
   (dissoc (parsed (subs text 1) origin dim level) :greedy)))

(defn- prefixed [prefix xs]
  (let [p (str/lower-case prefix)]
    (vec (filter #(str/starts-with? (str/lower-case %) p) xs))))

(defn- starting-with
  "The completions xs of prefix: brigadier drops the one equal to it."
  [prefix xs]
  (filterv #(not= prefix %) (prefixed prefix xs)))

(defn- suggest-player [world prefix]
  (prefixed prefix (sort (keys (:players world)))))

(defn- command-names [level]
  (filterv #(allowed? level %)
           (into ["execute"]
                 (concat (keys aliases) (map cmd-name commands)))))

(defn- suggest-command [prefix level]
  (mapv #(str "/" %)
        (starting-with prefix (sort (command-names level)))))

(defn- suggest-subcommand [form prefix]
  (starting-with prefix (sort (map cmd-name (drop 2 form)))))

(defn- next-split [^String input ^long i]
  (let [js (keep #(str/index-of input % i) [\. \_ \/])]
    (when (seq js) (inc (long (reduce min js))))))

(defn- sub-match? [^String pattern ^String input]
  (loop [i 0]
    (cond
      (str/starts-with? (subs input i) pattern) true
      :else (if-let [j (next-split input i)] (recur j) false))))

(defn- resource-match? [^String typed ^String id]
  (let [[ns path] (str/split id #":" 2)]
    (if (str/includes? typed ":")
      (sub-match? typed id)
      (or (sub-match? typed ns) (sub-match? typed path)))))

(defn- matching-resources [typed ids]
  (let [t (str/lower-case typed)]
    (filterv #(and (not= typed %) (resource-match? t %)) ids)))

(defn- sugg [start texts] {:start start :texts texts})

(defn- unit-suggestions [w start]
  (let [res (r/read-float (r/reader w))]
    (when-not (r/error? res)
      (let [n (long (second (second res)))]
        (sugg (+ (long start) n)
              (starting-with (subs w n) ["d" "s" "t"]))))))

(defn- clock-in [opts cx]
  (if-let [i (:clock-arg opts)]
    (nth (:values cx) i)
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

(defn- arg-suggestions [[_ [kind opts] :as a] w start cx]
  (case kind
    :ticks (unit-suggestions w start)
    :float nil
    :marker (sugg start (matching-resources w (marker-ids opts cx)))
    :timeline
    (sugg start (matching-resources w (timeline-ids opts cx)))
    :sound (let [ids (map data/wire (keys (sound-events)))]
             (sugg start (matching-resources w (sort ids))))
    :clock (let [ids (map data/wire (clock/names))]
             (sugg start (matching-resources w ids)))
    :literal (sugg start (starting-with w [(:name opts)]))
    :mob-effect
    (sugg start (matching-resources w (arg-values a (:target cx))))
    :targets (let [vs (when (selectors? cx) (arg-values a nil))]
               (sugg start (starting-with w vs)))
    (sugg start (starting-with w (arg-values a (:target cx))))))

(def ^:private loose #{:coord :dcoord :angle})

(defn- prior-values
  "The values of the words before the last, nil when one fails."
  [args words]
  (reduce (fn [acc [[_ [kind] :as a] w]]
            (let [[st v] (if (loose kind) [:ok nil] (coerce a w nil))]
              (if (= :ok st) (conj acc v) (reduced nil))))
          [] (map vector args words)))

(defn- way-suggestions [cx [args _] words start]
  (when-let [a (nth (vec args) (dec (count words)) nil)]
    (when-let [vs (prior-values args (pop words))]
      (arg-suggestions a (peek words) start (assoc cx :values vs)))))

(defn- expanded [^String text lo {:keys [start texts]}]
  (map #(str (subs text lo start) %) texts))

(defn- merged
  "The suggestions of several arguments as one list, the way
  brigadier merges them: one range, sorted ignoring case."
  [text ss start]
  (let [ss (filter (comp seq :texts) ss)
        lo (if (seq ss) (reduce min (map :start ss)) start)]
    (sugg lo (->> ss (mapcat #(expanded text lo %)) distinct
                  (sort String/CASE_INSENSITIVE_ORDER) vec))))

(defn- form-suggestions [form words start cx text]
  (merged text (keep #(way-suggestions cx % words start) (ways form))
          start))

(defn- after-command [form words start cx text]
  (let [sub (when (subcommands? form)
              (find-form (drop 2 form) (first words)))]
    (cond
      (not (subcommands? form))
      (form-suggestions form words start cx text)
      (= 1 (count words))
      (sugg start (suggest-subcommand form (first words)))
      sub (form-suggestions sub (subvec words 1) start cx text)
      :else (sugg start []))))

(defn- command-suggestions [text start cx]
  (let [[nm & more] (str/split (subs text 1) #" " -1)
        form (find-form commands (get aliases nm nm))]
    (cond
      (empty? more) (sugg start (suggest-command nm (:level cx)))
      (or (nil? form) (not (allowed? (:level cx) nm))) (sugg start [])
      :else (after-command form (vec more) start cx text))))

(defn suggestions
  "Returns the completions for half-typed text as :texts that
  replace it from index :start. A player standing at target sees
  them. A player of permission level sees the commands it may run."
  ([world text] (suggestions world text nil))
  ([world text target] (suggestions world text target gamemaster))
  ([world text target level]
   (let [text (or text "")
         start (inc (long (or (str/last-index-of text " ") -1)))
         cx {:dim (:dim world :overworld) :target target
             :level level}]
     (if (str/starts-with? text "/")
       (command-suggestions text start cx)
       (sugg start (suggest-player world (subs text start)))))))

(defn suggest
  "Returns the texts of the completions for half-typed text."
  ([world text] (suggest world text nil))
  ([world text target] (:texts (suggestions world text target))))

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
   :anchor [:entity-anchor nil]
   :message [:message nil]
   :component [:component nil]})

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

(defn- rule-value [{:keys [type min max]}]
  {:type :argument :name "value" :executable? true
   :parser (if (= :bool type) brigadier-bool brigadier-integer)
   :props (when (= :int type) {:min min :max max})})

(defn- rule-nodes []
  (vec (for [[rule spec] rules/table]
         {:type :literal :executable? true
          :name (subs (rules/wire-name rule) 10)
          :children [(rule-value spec)]})))

(defn- literal-nodes [ls exec? children]
  (mapv (fn [l] {:type :literal :name l :executable? exec?
                 :children children})
        ls))

(defn- spec-nodes [spec exec? children]
  (cond
    (:rules spec) (rule-nodes)
    (:or-literals spec)
    (into (spec-nodes [(:arg spec)] exec? children)
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

(defn- chain [args ^long i]
  (if (>= i (count args))
    [[] true]
    (let [optional (optional-from args)
          spec (argument-nodes (nth args i))
          tail (if (:rules spec) [[] true] (chain args (inc i)))
          [kids tail-exec] tail
          exec? (or tail-exec (>= (inc i) optional))]
      [(mapv #(with-meta % {:arg (nth args i)})
             (spec-nodes spec exec? kids))
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
              #(merged-nodes (into (vec %) (:children b))))))

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

(defn tree
  "Returns the command nodes a client of permission level gets, the
  root last."
  ([] (tree gamemaster))
  ([level]
   (let [ok? #(allowed? level %)
         forms (filter (comp ok? cmd-name) commands)
         top (-> (mapv form-node forms)
                 (cond-> (ok? "execute") (conj execute-node))
                 (into (comp (filter (comp ok? key)) (map alias-node))
                       aliases))]
     (redirected (first (flat [] {:type :root :children top}))))))

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

(defn- width [[_ [kind]]]
  (case kind (:coord :dcoord) 3 :angle 2 1))

(defn- read-ok? [a w axis origin]
  (let [a (cond-> a axis (assoc-in [1 1 :axis] axis))]
    (= :ok (first (coerce a w origin)))))

(defn- reader-of [a n origin]
  (fn [k [w]] (read-ok? a w (when (< 1 (long n)) k) origin)))

(defn- arg-read
  "The tokens left after argument node c reads ts, nil when it
  cannot."
  [c ts text origin]
  (let [[_ [kind] :as a] (:arg (meta c))
        n (width a)]
    (cond
      (nil? a) (rest ts)
      (rest-kinds kind)
      (when (read-ok? a (subs text (second (first ts))) nil origin)
        [])
      (< (count ts) n) nil
      (every? true? (map-indexed (reader-of a n origin) (take n ts)))
      (drop n ts))))

(defn- step-node [nodes n [[w] :as ts] text origin]
  (let [kids (map nodes (:children n))
        named? #(and (= :literal (:type %)) (= w (:name %)))
        lit (some #(when (named? %) %) kids)
        arg #(when-let [more (arg-read % ts text origin)] [% more])]
    (if lit
      [lit (rest ts)]
      (some arg (filter #(= :argument (:type %)) kids)))))

(defn- last-node
  "The index of the last node brigadier reads of text, nil for none."
  [nodes text origin]
  (loop [n (peek nodes) ts (words text) at nil]
    (if-let [[c more] (when (and (seq ts) (not (:redirect n)))
                        (step-node nodes n ts text origin))]
      (recur c more (index-of nodes #(identical? c %)))
      at)))

(defn help-lines
  "Returns the lines /help shows a player of permission level, for
  command text or for every command when text is nil. Returns nil
  when text names no command."
  [level text origin]
  (let [nodes (tree level)
        i (if text (last-node nodes text origin) (dec (count nodes)))
        n (when i (nth nodes i))
        head (if text (str "/" text " ") "/")]
    (when n
      (let [opt? (boolean (:executable? n))]
        (mapv #(str head (smart-usage nodes % opt? false))
              (:children n))))))
