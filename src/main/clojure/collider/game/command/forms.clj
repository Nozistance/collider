(ns collider.game.command.forms
  "The commands players run, with their arguments and permission
  levels."
  (:require [collider.game.out :as out]))

(set! *warn-on-reflection* true)

(def ^:private int-max 2147483647)

(def ^:private int-min -2147483648)

(def ^:const gamemaster
  "The permission level most commands require."
  2)

(def ^:const owner-level
  "The highest permission level."
  4)

(def ^:private open-commands
  "The commands every player may run."
  #{"me" "msg" "tell" "w" "list" "help"})

(defn- level-of ^long [nm]
  (if (open-commands nm) 0 gamemaster))

(defn allowed?
  "Returns true when permission level may run what needs need.
  A nil level counts as the gamemaster level."
  [level ^long need]
  (>= (long (or level gamemaster)) need))

(defn selectors?
  "Returns true when the source in opts may use entity selectors."
  [opts]
  (allowed? (:level opts) gamemaster))

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

(def ^:private data-form
  [:data "read entity data"
   [(lit "get") (lit "entity") one-arg [:path [:nbt-path {:default nil}]]
    [:scale [:double {:default nil}]]]
   [:world :data-get-entity]])

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
   tag-form data-form rotate-form swing-form
   [:version "show the version of the game" [] [:world :version]]])

(def ^:private fill-modes
  #{"destroy" "hollow" "keep" "outline" "replace" "strict"})

(def ^:private fill-then
  [:then [:enum {:values #{"destroy" "hollow" "outline" "strict"}
                 :default nil}]])

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
    (-> [[:entity [:entity-type {}]]]
        (into (vec-args {:default nil :node "pos"}))
        (conj [:nbt [:compound {:default nil}]]))
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
    (conj (box-args) (mode-arg fill-modes))
    [:world :fill]
    (conj (box-args) [:mode [:enum {:values #{"replace"}}]]
          [:filter [:block-predicate {}]]
          fill-then)
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

(def commands
  "The built-in command forms."
  (into base-forms chat-forms))

(defn extra-of
  "Returns the command forms that plugins add to world."
  [world]
  (:plugin-commands (:config world)))

(defn cmd-name
  [form]
  (name (first form)))

(defn- opts-of [form]
  (let [o (nth form 2 nil)] (when (map? o) o)))

(defn plain-form
  "Returns form without its options."
  [form]
  (if (opts-of form) (into (subvec form 0 2) (subvec form 3)) form))

(defn- form-level [f]
  [(cmd-name f) (long (:level (opts-of f) 0))])

(defn needs
  "Returns the permission level each command name needs, with the
  plugin forms extra."
  [extra]
  (let [own (into {} (map form-level) extra)]
    (fn [nm] (or (get own nm) (level-of nm)))))

(defn subcommands?
  "Returns true when form holds subcommand forms."
  [form]
  (keyword? (first (nth form 2))))

(defn ways
  "Returns the [arguments meaning] pairs of form."
  [form]
  (partition 2 (drop 2 form)))

(def aliases
  "The other names of commands."
  {"tp" "teleport" "xp" "experience" "tell" "msg" "w" "msg"})
