(ns collider.game.commands.world
  "Commands on the world, such as time, weather and gamerule."
  (:require [collider.data :as data]
            [collider.game.clock :as clock]
            [collider.game.command.targets :as targets]
            [collider.game.commands.player :as player-commands]
            [collider.game.commands.pos :as pos]
            [collider.game.commands.reply
             :refer [answer dimension-id fail say say* success tell]]
            [collider.game.gamerules :as rules]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.random :as random]
            [collider.world.env.weather :as weather]))

(set! *warn-on-reflection* true)

(defn- rule-hint [rule]
  (if (= :bool (:type (rules/table rule)))
    "true or false"
    "a whole number in range"))

(defn- rule-error [eid id rule text]
  (tell eid (str id ": give " (rule-hint rule)
                 ", not \"" text "\"")))

(defn- clocks-resent [world rule]
  (when (= :advance-time rule)
    [(out/all (out/time (long (:tick world))
                        (clock/full-sync world)))]))

(defn- rule-shown [world rule v]
  (case rule
    (:immediate-respawn :limited-crafting)
    [(out/all (out/rule-flag rule v))]
    :reduced-debug-info
    (for [p (vals (:players world))]
      (out/to p (out/status p (if v :reduced-debug :full-debug))))
    nil))

(defn- rule-set [world eid id rule v]
  (let [w (assoc-in world [:rules rule] v)]
    (concat [[:set-rule rule v]
             (out/all (out/game-rules (:rules w)))]
            (rule-shown w rule v)
            (clocks-resent w rule)
            (say eid "commands.gamerule.set" id
                 (rules/serialize rule v)))))

(defn- rule-query [world eid id rule]
  (let [v (rules/serialize rule (get-in world [:rules rule]))
        msg {:translate "commands.gamerule.query" :with [id v]}]
    (answer [(out/to eid (out/system-chat msg))])))

(defn rule-deltas
  "Returns the deltas of player eid asking for game rule rule, or
  setting it to text when text is given."
  [world eid rule text]
  (let [id (subs (rules/wire-name rule) 10)]
    (if (nil? text)
      (rule-query world eid id rule)
      (if-some [v (rules/parse rule text)]
        (rule-set world eid id rule v)
        (rule-error eid id rule text)))))

(defn- gamerule-deltas [world eid [rule text]]
  (rule-deltas world eid rule text))

(defn- same-spawn? [world dim at turn]
  (and (= dim (:world-spawn-dimension world :overworld))
       (= at (vec (:world-spawn world)))
       (= (mapv double turn) (player/spawn-turn world))))

(defn- world-spawn-set [world eid at [yaw pitch :as turn]]
  (let [dim (targets/source-dim world)
        with (conj (mapv str at) (str yaw) (str pitch)
                   (dimension-id dim))
        msg {:translate "commands.setworldspawn.success" :with with}]
    (concat [[:set-world-spawn dim at [yaw pitch]]]
            (when-not (same-spawn? world dim at turn)
              [(out/everyone (out/default-spawn dim at yaw pitch))])
            (success [(out/to eid (out/system-chat msg))]))))

(defn- world-spawn-deltas
  [world eid [x y z yaw pitch]]
  (if (pos/out-of-bounds? [x y z])
    (fail eid "argument.pos.outofbounds")
    (world-spawn-set world eid (pos/block-under world [x y z])
                     (pos/turn-of world eid yaw pitch))))

(defn- duration-of ^long [world ^long given bounds salt]
  (if (pos? given)
    given
    (weather/sample (random/of-key (:tick world) salt) bounds)))

(defn- clear-parameters [world ^long given]
  (let [d (duration-of world given weather/rain-delay
                       :weather-clear)]
    (weather/parameters d 0 false false)))

(defn- rain-parameters [world ^long given thunder?]
  (let [bounds (if thunder?
                 weather/thunder-duration
                 weather/rain-duration)
        salt (if thunder? :weather-thunder :weather-rain)
        d (duration-of world given bounds salt)]
    (weather/parameters 0 d true thunder?)))

(defn- weather-parameters [world kind given]
  (case kind
    :clear (clear-parameters world given)
    :rain (rain-parameters world given false)
    :thunder (rain-parameters world given true)))

(defn- weather-deltas [kind]
  (fn [world eid [given]]
    (let [given (long (or given 0))
          m (weather-parameters world kind given)
          msg {:translate (str "commands.weather.set." (name kind))}]
      (cons [:set-weather m]
            (success [(out/to eid (out/system-chat msg))])))))

(defn- clock-sent [world k c]
  (let [on (clock/advancing? world)]
    [[:set-clock k c]
     (out/all (out/time (long (:tick world))
                        {k (clock/network-state c on)}))]))

(defn- marker-key [id]
  (str "ResourceKey[minecraft:clock_time_marker / " id "]"))

(defn- to-marker [world eid k id]
  (let [c0 (clock/state world k)
        c (clock/moved-to c0 k id)]
    (concat (clock-sent world k (or c c0))
            (if c
              (say eid "commands.time.set.time_marker"
                   (data/wire k) id)
              (fail eid "commands.time.no_time_marker_found"
                    (marker-key id) (data/wire k))))))

(defn- paused [p] (fn [c _] (assoc c :paused p)))

(def ^:private clock-edits
  {:time-set [clock/set-ticks "commands.time.set.absolute"
              (fn [_ v] [v])]
   :time-add [clock/added "commands.time.set.absolute"
              (fn [c _] [(bigint (:total-ticks c))])]
   :time-pause [(paused true) "commands.time.pause" (fn [_ _] [])]
   :time-resume [(paused false) "commands.time.resume"
                 (fn [_ _] [])]
   :time-rate [(fn [c v] (assoc c :rate (double v)))
               "commands.time.rate" (fn [_ v] [v])]})

(defn- clock-changed [world eid k v [f key with]]
  (let [c (f (clock/state world k) v)]
    (concat (clock-sent world k c)
            (say* eid key (into [(data/wire k)] (with c v))))))

(defn- in-timeline [world eid k id f key]
  (let [t (clock/timeline-of (data/wire id))
        n (clock/ticks world k)]
    (if (= k (:clock t))
      (answer (say eid key (data/wire id) (bigint (f t n))))
      (fail eid "commands.time.wrong_timeline_for_clock"
            (data/wire id) (data/wire k)))))

(def ^:private timeline-reads
  {:time-timeline [clock/timeline-ticks
                   "commands.time.query.timeline"]
   :time-repetition [clock/repetitions
                     "commands.time.query.timeline.repetitions"]})

(defn- clock-query [world eid k]
  (answer (say eid "commands.time.query.absolute" (data/wire k)
               (bigint (clock/ticks world k)))))

(defn- clock-deltas [world eid op k v]
  (if-let [e (clock-edits op)]
    (clock-changed world eid k v e)
    (case op
      :time-marker (to-marker world eid k v)
      :time-query (clock-query world eid k)
      (let [[f key] (timeline-reads op)]
        (in-timeline world eid k v f key)))))

(defn- time-deltas [world eid op [k v]]
  (let [dim (targets/source-dim world)
        k (or k (clock/default-of dim))]
    (cond
      (= :time-gametime op)
      (answer (say eid "commands.time.query.gametime"
                   (bigint (:tick world))))
      (nil? k) (fail eid "commands.time.no_default_clock"
                     (data/wire dim))
      :else (clock-deltas world eid op k v))))

(def ^:private time-ops
  [:time-set :time-add :time-marker :time-pause :time-resume
   :time-rate :time-query :time-timeline :time-repetition
   :time-gametime])

(defn- time-handler [op]
  (fn [world eid args] (time-deltas world eid op args)))

(defn- enforced [world mode]
  (when mode
    (mapcat (fn [[_ dim :as x]]
              (let [ds (player-commands/mode-change world x mode)]
                (targets/in-level world dim ds)))
            (targets/player-entries world))))

(defn- default-mode-deltas
  [world eid [mode]]
  (let [cfg (assoc (:config world) :game-mode mode)
        forced (game-mode/forced-mode (assoc world :config cfg))]
    (concat [[:set-config cfg]]
            (enforced world forced)
            (say eid "commands.defaultgamemode.success"
                 (player-commands/mode-name mode)))))

(defn- reload-deltas [_world eid _args]
  (cons (out/to eid (out/reload))
        (say eid "commands.reload.success")))

(def handlers
  (into {:gamerule gamerule-deltas
         :weather-clear (weather-deltas :clear)
         :weather-rain (weather-deltas :rain)
         :weather-thunder (weather-deltas :thunder)
         :setworldspawn world-spawn-deltas
         :defaultgamemode default-mode-deltas
         :reload reload-deltas}
        (map (juxt identity time-handler))
        time-ops))
