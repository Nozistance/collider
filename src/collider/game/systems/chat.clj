(ns collider.game.systems.chat
  "Chat lines, commands and tab completion."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.command.tree :as cmd]
            [collider.game.entity :as entity]
            [collider.game.gamerules :as rules]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.nav :as nav]
            [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.game.systems.chunks :as chunks]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.weather :as weather])
  (:import (java.util Locale)))

(set! *warn-on-reflection* true)

(def ^:private markers
  [["***" {:bold true :italic true}]
   ["**" {:bold true}]
   ["__" {:underlined true}]
   ["~~" {:strikethrough true}]
   ["*" {:italic true}]
   ["_" {:italic true}]
   ["`" {:color "gray"}]])

(declare parse-runs)

(defn- marker-at [^String s ^long i]
  (some (fn [[^String m st]]
          (when (.startsWith s m i) [m st]))
        markers))

(defn- marked [^String s m st styles start close]
  (let [inner (.substring s start close)]
    (if (= m "`")
      [(merge styles st {:text inner})]
      (parse-runs inner (merge styles st)))))

(defn- span
  "Returns the end index and runs of the span opening at i.
  A marker that never closes stands for itself."
  [^String s styles ^StringBuilder plain ^long i]
  (when-let [[^String m st] (marker-at s i)]
    (let [start (+ i (.length m))
          close (^[String int] String/.indexOf s m start)]
      (if (> close start)
        [(+ close (.length m)) (marked s m st styles start close)]
        (do (.append plain m) [start nil])))))

(defn- flushed [out styles ^StringBuilder plain]
  (if (pos? (.length plain))
    (conj out (assoc styles :text (str plain)))
    out))

(defn- escape-at? [^String s ^long i]
  (and (= \\ (.charAt s i)) (< (inc i) (.length s))
       (or (marker-at s (inc i))
           (= \\ (.charAt s (inc i))))))

(defn- step
  "Returns the parse state after the markup at the index it holds."
  [^String s styles [i ^StringBuilder plain out]]
  (if (escape-at? s i)
    (do (.append plain (.charAt s (inc i)))
        [(+ 2 (long i)) plain out])
    (if-let [[end runs] (span s styles plain i)]
      (if runs
        [end (StringBuilder.) (into (flushed out styles plain) runs)]
        [end plain out])
      (do (.append plain (.charAt s i))
          [(inc (long i)) plain out]))))

(defn parse-runs
  "Returns the styled runs of a line with chat markup."
  ([s] (parse-runs s {}))
  ([^String s styles]
   (loop [st [0 (StringBuilder.) []]]
     (if (>= (long (first st)) (.length s))
       (flushed (peek st) styles (second st))
       (recur (step s styles st))))))

(defn tell
  "Returns the deltas that show the lines to one player."
  [eid & lines]
  (mapv (fn [line] (out/to eid (out/system-chat (parse-runs line))))
        (mapcat #(str/split-lines (str %)) lines)))

(defn- say [eid key & with]
  (let [msg {:translate key :with (vec with)}]
    [(out/to eid (out/system-chat [msg]))]))

(defn- unloaded?
  "Tells whether any position belongs to no chunk the world holds.
  A chunk a player keeps counts even before its data arrives."
  [world positions]
  (let [held (chunks/needed-ids world)]
    (some #(let [id (chunk/block-chunk %)]
             (not (or (contains? (:chunks world) id)
                      (contains? held id))))
          positions)))

(defn- box [[ax ay az bx by bz]]
  (mapv (fn [a b] (sort [(long a) (long b)])) [ax ay az] [bx by bz]))

(defn- fill-changes [chunks [[x1 x2] [y1 y2] [z1 z2]] st]
  (vec (for [x (range x1 (inc x2))
             y (range y1 (inc y2))
             z (range z1 (inc z2))
             :when (not= st (chunk/chunks-get-block chunks [x y z]))]
         [[x y z] st])))

(defn- box-ids [[[x1 x2] _ [z1 z2]]]
  (for [cx (range (bit-shift-right (long x1) 4)
                  (inc (bit-shift-right (long x2) 4)))
        cz (range (bit-shift-right (long z1) 4)
                  (inc (bit-shift-right (long z2) 4)))]
    (chunk/pos->id cx cz)))

(defn- fetched
  "Returns [chunks deltas] with every absent chunk of the box read."
  [world bounds]
  (let [in? #(contains? (:chunks world) %)
        read (fn [id] [id (chunks/read-absent world id)])
        loaded (into {} (comp (remove in?) (map read))
                     (box-ids bounds))]
    [(reduce-kv #(assoc %1 %2 (:chunk %3)) (:chunks world) loaded)
     (chunks/read-absent-deltas loaded)]))

(defn- source-dim
  "Returns the level a command runs in: its player's, unless the
  command was run in another."
  [world]
  (get-in world [:source :dim] (:dim world)))

(defn- source-pos [world eid]
  (if (contains? (:source world) :pos)
    (get-in world [:source :pos])
    (when-let [p (get-in world [:entities eid :pos])]
      [(v/x p) (v/y p) (v/z p)])))

(defn- level-view
  "Returns level dim as a command in world sees it."
  [world dim]
  (let [server (:server world)]
    (if (or (= dim (:dim world)) (nil? server))
      world
      (assoc (state/level server dim) :server server))))

(defn- in-level
  "Returns deltas ds of level dim, handed over to it when the
  command runs in another level."
  [world dim ds]
  (cond
    (empty? ds) nil
    (= dim (:dim world)) (vec ds)
    :else [[:level-deltas dim (vec ds)]]))

(defn- source-level [world] (level-view world (source-dim world)))

(defn- filled [world eid bounds block]
  (let [dim (source-dim world)
        [chunks adds] (fetched (level-view world dim) bounds)
        changes (fill-changes chunks bounds (block/state block))]
    (if (empty? changes)
      (concat (in-level world dim adds)
              (say eid "commands.fill.failed"))
      (concat (in-level world dim
                        (conj (vec adds) [:set-blocks changes]))
              (say eid "commands.fill.success"
                   (str (count changes)))))))

(defn- flat-in? [^long x ^long z]
  (and (<= -30000000 x) (< x 30000000)
       (<= -30000000 z) (< z 30000000)))

(defn- in-world?
  "Level.isInWorldBounds: inside the height of the level and
  the flat bounds of every level."
  [world [x y z]]
  (and (chunk/in-level? world (long y))
       (flat-in? (long x) (long z))))

(defn- pos-error
  "Returns the key of the reason pos is no command target, or nil.
  BlockPosArgument.getLoadedBlockPos."
  [world pos]
  (cond
    (unloaded? world [pos]) "argument.pos.unloaded"
    (not (in-world? world pos)) "argument.pos.outofworld"))

(defn- spawnable?
  "Level.isInSpawnableBounds for the block that holds pos."
  [pos]
  (let [[x y z] (mapv #(long (Math/floor (double %))) pos)]
    (and (<= -20000000 (long y)) (< (long y) 20000000)
         (flat-in? x z))))

(defn- fill-deltas [world eid [ax ay az bx by bz block]]
  (if-let [k (some #(pos-error (source-level world) %)
                   [[ax ay az] [bx by bz]])]
    (say eid k)
    (filled world eid (box [ax ay az bx by bz]) block)))

(defn- rule-hint [rule]
  (if (= :bool (:type (rules/table rule)))
    "true or false"
    "a whole number in range"))

(defn- rule-error [eid id rule text]
  (tell eid (str id ": give " (rule-hint rule)
                 ", not \"" text "\"")))

(defn- rule-set [world eid id rule v]
  (concat [[:set-rule rule v]
           (out/all (out/game-rules (assoc (:rules world) rule v)))]
          (say eid "commands.gamerule.set" id
               (rules/serialize rule v))))

(defn- rule-deltas [world eid rule text]
  (let [id (subs (rules/wire-name rule) 10)]
    (if (nil? text)
      (say eid "commands.gamerule.query" id
           (rules/serialize rule (get-in world [:rules rule])))
      (if-some [v (rules/parse rule text)]
        (rule-set world eid id rule v)
        (rule-error eid id rule text)))))

(defn- set-rule-delta [world eid [k v]]
  (when-let [r (rules/rule-of k)]
    (rule-deltas world eid r v)))

(defn- rules-event-deltas [world [tag eid entries]]
  (case tag
    :rules-request [(out/to eid (out/game-rules (:rules world)))]
    :set-rules (mapcat #(set-rule-delta world eid %) entries)
    nil))

(defn- entity-name [e]
  (if (= :player (:type e))
    (:name e)
    {:translate (str "entity.minecraft." (data/snake (:type e)))}))

(defn- item-name [item]
  {:translate (str "item.minecraft." (data/snake item))})

(defn- typed-ids [world type not-type? ids]
  (if type
    (filter #(= (boolean not-type?)
                (not= type (get-in world [:entities % :type])))
            ids)
    ids))

(defn- nearest-id [world eid ids]
  (let [p (get-in world [:entities eid :pos])
        at (fn [id] (get-in world [:entities id :pos]))
        d (fn [id] [(v/dist-sq p (at id)) id])]
    (when-let [near (first (sort-by d ids))]
      [near])))

(defn- targets [world eid sel]
  (let [{:keys [self all nearest entities type not-type? name]} sel
        players (map key (state/player-entries world))
        typed #(typed-ids world type not-type? %)]
    (cond
      self [eid]
      all (typed players)
      nearest (nearest-id world eid (typed players))
      entities (typed (keys (:entities world)))
      name (when-let [id (get-in world [:players name])] [id]))))

(defn- where
  "Returns [dim entity] of entity id, in any level of the server.
  A level seen alone knows only its own entities."
  [world id]
  (let [server (:server world)
        dim (when server (state/dim-of server id))]
    (if (and dim (not= dim (:dim world)))
      [dim (get-in (state/level server dim) [:entities id])]
      [(:dim world) (get-in world [:entities id])])))

(defn- destination [world eid sel]
  (if-let [nm (:name sel)]
    (get-in world [:players nm])
    (first (targets world eid sel))))

(defn- crossed [lv eid dim pos yaw pitch]
  (let [e (get-in lv [:entities eid])
        known (sort-by chunk/id->pos (seq (:sent-chunks e)))
        seen (sort (seq (:tracking e)))]
    [[:change-dimension eid dim pos yaw pitch]
     (out/to eid
             (out/change-dimension dim pos yaw pitch known seen))]))

(defn- player-moved [world id from e to pos yaw pitch]
  (in-level
    world from
    (if (= from to)
      (cond-> [[:teleport id pos]]
        (not= [yaw pitch] [(:yaw e) (:pitch e)])
        (conj [:merge-entity id {:yaw yaw :pitch pitch}])
        :always (conj (out/to id (out/teleport pos yaw pitch))))
      (crossed (level-view world from) id to pos yaw pitch))))

(def ^:private still (v/v3 [0.0 0.0 0.0]))

(defn- placed-props [e pos yaw pitch]
  (cond-> {:pos (v/v3 pos) :yaw yaw :pitch pitch :vel still
           :on-ground true}
    (:nav e) (assoc :nav (:nav (nav/stop e)))))

(defn- recreated
  "Returns entity e of id made anew at pos, as a level it enters
  loads it. It keeps its uuid; the level gives it a new id."
  [world id e pos yaw pitch]
  (let [t (:tick world)]
    (when-let [m (entity/loaded (entity/saved e t) t)]
      (assoc m :pos (v/v3 pos) :yaw yaw :pitch pitch :vel still
             :uuid (entity/uuid-of id e)))))

(defn- entity-moved [world id from e to pos yaw pitch]
  (if (= from to)
    (in-level world from
              [[:merge-entity id (placed-props e pos yaw pitch)]])
    (concat
      (in-level world from [[:remove-entity id]])
      (when-let [m (recreated world id e pos yaw pitch)]
        (in-level world to [[:spawn-entity m]])))))

(defn- moved
  "Returns the deltas that put entity id of level from at pos in
  level to, turned to yaw and pitch."
  [world [id from e] to pos [yaw pitch]]
  (let [yaw (double yaw) pitch (double pitch)]
    (if (= :player (:type e))
      (player-moved world id from e to pos yaw pitch)
      (entity-moved world id from e to pos yaw pitch))))

(defn- located [world ids]
  (keep (fn [id]
          (let [[dim e] (where world id)]
            (when e [id dim e])))
        ids))

(defn- coord [x]
  (String/format Locale/ROOT "%f" (object-array [(double x)])))

(defn- pos-report [eid placed pos]
  (let [at (mapv coord pos)]
    (if (= 1 (count placed))
      (say eid "commands.teleport.success.location.single"
           (entity-name (nth (first placed) 2)) (at 0) (at 1) (at 2))
      (say eid "commands.teleport.success.location.multiple"
           (count placed) (at 0) (at 1) (at 2)))))

(defn- own-turn [[_ _ e]] [(:yaw e 0.0) (:pitch e 0.0)])

(defn- tp-pos-deltas [world eid ids pos]
  (let [placed (located world ids)
        to (source-dim world)]
    (cond
      (empty? placed) (say eid "argument.entity.notfound.entity")
      (not (spawnable? pos))
      (say eid "commands.teleport.invalidPosition")
      :else
      (concat (mapcat #(moved world % to pos (own-turn %)) placed)
              (pos-report eid placed pos)))))

(defn- entity-report [eid placed d]
  (if (= 1 (count placed))
    (say eid "commands.teleport.success.entity.single"
         (entity-name (nth (first placed) 2)) (entity-name d))
    (say eid "commands.teleport.success.entity.multiple"
         (count placed) (entity-name d))))

(defn- to-entity [world eid placed dim d]
  (let [pos (vec (:pos d)) turn [(:yaw d 0.0) (:pitch d 0.0)]]
    (concat (mapcat #(moved world % dim pos turn) placed)
            (entity-report eid placed d))))

(defn- tp-entity-deltas [world eid ids sel]
  (let [id (destination world eid sel)
        [dim d] (when id (where world id))
        placed (located world ids)]
    (cond
      (or (nil? d) (empty? placed))
      (say eid "argument.entity.notfound.entity")
      (not (spawnable? (:pos d)))
      (say eid "commands.teleport.invalidPosition")
      :else (to-entity world eid placed dim d))))

(defn- tp-deltas [world eid pos]
  (tp-pos-deltas world eid [eid] (vec pos)))

(defn- tp-to-deltas [world eid [sel]]
  (tp-entity-deltas world eid [eid] sel))

(defn- tp-targets-deltas [world eid [sel & pos]]
  (tp-pos-deltas world eid (targets world eid sel) (vec pos)))

(defn- tp-targets-to-deltas [world eid [sel dest]]
  (tp-entity-deltas world eid (targets world eid sel) dest))

(defn- given [world eid id item n]
  (let [e (get-in world [:entities id])
        inv (or (:inventory e) {})
        [changes left] (items/add-stack inv {:item item :count n})]
    (concat
      (map (fn [[slot stack]] [:set-slot id slot stack]) changes)
      (when left
        [[:spawn-entity (items/dropped world id left true 0)]])
      (say eid "commands.give.success.single"
           n (item-name item) (:name e)))))

(defn- give-deltas [world eid [sel item n]]
  (let [ids (targets world eid sel)]
    (if (empty? ids)
      (say eid "argument.entity.notfound.player")
      (mapcat #(given world eid % item n) ids))))

(defn- killed [world id]
  (if (= :player (get-in world [:entities id :type]))
    [[:merge-entity id {:health 0.0}]]
    [[:remove-entity id]]))

(defn- kill-report [world eid ids]
  (if (= 1 (count ids))
    (say eid "commands.kill.success.single"
         (entity-name (get-in world [:entities (first ids)])))
    (say eid "commands.kill.success.multiple" (count ids))))

(defn- kill-deltas [world eid [sel]]
  (let [ids (targets world eid sel)]
    (if (empty? ids)
      (say eid "argument.entity.notfound.entity")
      (concat (mapcat #(killed world %) ids)
              (kill-report world eid ids)))))

(defn- summoned [world eid type at]
  (let [t (:tick world)
        dim (source-dim world)
        kind {:translate (str "entity.minecraft." (name type))}
        msg {:translate "commands.summon.success" :with [kind]}
        mob (mobs/egg-mob type at [t eid :summon] t dim)]
    (concat (in-level world dim [[:spawn-entity mob]])
            [(out/to eid (out/system-chat [msg]))])))

(defn- summon-deltas [world eid [type x y z]]
  (let [p (source-pos world eid)
        at [(double (or x (nth p 0)))
            (double (or y (nth p 1)))
            (double (or z (nth p 2)))]]
    (if (spawnable? at)
      (summoned world eid type at)
      (say eid "commands.summon.invalidPosition"))))

(defn- block-set [world eid [x y z :as pos] st]
  (concat (in-level world (source-dim world)
                    [[:set-blocks [[pos st]]]])
          (say eid "commands.setblock.success"
               (str x) (str y) (str z))))

(defn- setblock-deltas [world eid [x y z block]]
  (let [pos [x y z]
        st (block/state block)
        lv (source-level world)
        k (pos-error lv pos)]
    (cond
      k (say eid k)
      (= st (chunk/chunks-get-block (:chunks lv) pos))
      (say eid "commands.setblock.failed")
      :else (block-set world eid pos st))))

(defn- block-under [world eid [x y z]]
  (let [p (source-pos world eid)
        at #(long (Math/floor (double (nth p %))))]
    [(long (or x (at 0))) (long (or y (at 1))) (long (or z (at 2)))]))

(defn- dimension-id [dim] (str "minecraft:" (data/snake dim)))

(defn- spawn-pos-deltas
  "Returns the deltas of f unless the given position is out of
  bounds. BlockPosArgument.getSpawnablePos."
  [f world eid args]
  (if (or (some nil? args) (spawnable? args))
    (f world eid args)
    (say eid "argument.pos.outofbounds")))

(defn- same-spawn? [world dim at]
  (and (= dim (:world-spawn-dimension world :overworld))
       (= at (vec (:world-spawn world)))))

(defn- world-spawn-deltas
  "Returns the deltas that move the world spawn to the level the
  command runs in. Players hear of it only when it moves."
  [world eid args]
  (let [at (block-under world eid args)
        dim (source-dim world)
        with (conj (mapv str at) "0.0" "0.0" (dimension-id dim))
        msg {:translate "commands.setworldspawn.success"
             :with with}]
    (concat [[:set-world-spawn dim at]]
            (when-not (same-spawn? world dim at)
              [(out/all (out/default-spawn dim at))])
            [(out/to eid (out/system-chat [msg]))])))

(defn- spawnpoint-set-deltas [world eid args]
  (let [at (block-under world eid args)
        dim (source-dim world)
        e (get-in world [:entities eid])
        with (conj (mapv str at) "0.0" "0.0" (dimension-id dim)
                   (entity-name e))
        spawn {:dimension dim :pos at :yaw 0.0 :pitch 0.0}
        msg {:translate "commands.spawnpoint.success.single"
             :with with}]
    [[:merge-entity eid {:forced-spawn spawn}]
     (out/to eid (out/system-chat [msg]))]))

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

(defn- weather-deltas [world eid kind given]
  (let [given (long (or given 0))
        m (weather-parameters world kind given)
        msg {:translate (str "commands.weather.set." (name kind))}]
    [[:set-weather m]
     (out/to eid (out/system-chat [msg]))]))

(defn- time-query [world what]
  (case what
    "daytime" (format "the time is **%d**"
                      (long (:time-of-day world 0)))
    "gametime" (format "the game time is **%d**"
                       (long (:tick world 0)))))

(defn- time-set-deltas [eid ^long t]
  (cons [:set-time t]
        (tell eid (format "set the time to **%d**" t))))

(defn- time-add-deltas [world eid amount]
  (let [t (+ (long (:time-of-day world 0)) (long amount))
        line (format "added **%d** to the time" amount)]
    (cons [:set-time t] (tell eid line))))

(defn- time-deltas [world eid op args]
  (case op
    :time-set (time-set-deltas eid (long (first args)))
    :time-add (time-add-deltas world eid (first args))
    :time-query (tell eid (time-query world (first args)))))

(defn- weather-command-deltas [world eid op args]
  (let [given (first args)]
    (case op
      :weather-clear (weather-deltas world eid :clear given)
      :weather-rain (weather-deltas world eid :rain given)
      :weather-thunder (weather-deltas world eid :thunder given))))

(def ^:private commands
  {:tp tp-deltas :tp-to tp-to-deltas :tp-targets tp-targets-deltas
   :tp-targets-to tp-targets-to-deltas :give give-deltas
   :kill kill-deltas
   :summon summon-deltas :setblock setblock-deltas
   :setworldspawn (partial spawn-pos-deltas world-spawn-deltas)
   :spawnpoint (partial spawn-pos-deltas spawnpoint-set-deltas)
   :fill fill-deltas})

(defn- world-command-deltas [world eid [_ op & args]]
  (if-let [f (commands op)]
    (f world eid args)
    (case op
      :gamerule (rule-deltas world eid (first args) (second args))
      (:weather-clear :weather-rain :weather-thunder)
      (weather-command-deltas world eid op args)
      (:time-set :time-add :time-query)
      (time-deltas world eid op args)
      (tell eid (str "unknown world command: " op)))))

(defn- sourced
  "Returns world with the level and position the command r runs
  at, as :source."
  [world r origin]
  (let [pos (if (contains? r :origin) (:origin r) origin)]
    (assoc world :source {:dim (:dim r (:dim world)) :pos pos})))

(defn- command-deltas [world eid text]
  (let [origin (when-let [p (get-in world [:entities eid :pos])]
                 [(v/x p) (v/y p) (v/z p)])
        r (cmd/parse text origin (:dim world :overworld))]
    (if-let [error (:error r)]
      (tell eid error)
      (let [w (sourced world r origin)]
        (world-command-deltas w eid (:delta r))))))

(defn- public-deltas [world eid text]
  (when-let [e (get-in world [:entities eid])]
    [(out/all (out/player-chat (:name e) (parse-runs text)))]))

(defn- said-deltas [world eid raw]
  (let [text (str/trim (str raw))]
    (cond
      (str/blank? text) nil
      (str/starts-with? text "/") (command-deltas world eid text)
      :else (public-deltas world eid text))))

(defn- tab-deltas [world eid text target id]
  (let [start (inc (.lastIndexOf ^String text " "))
        len (- (count text) start)
        sug (cmd/suggest world text target)]
    [(out/to eid (out/suggestions (or id 0) start len sug))]))

(defn- event-deltas [world [tag eid text target id]]
  (case tag
    :chat (said-deltas world eid text)
    :tab-complete (tab-deltas world eid text target id)
    nil))

(defn- one-deltas [world ev]
  (vec (concat (event-deltas world ev)
               (rules-event-deltas world ev))))

(defn- chat-deltas [world events]
  (state/fold-events world events one-deltas))

(defn chat
  "Turns the chat lines and commands of this tick into deltas."
  [world d]
  (let [events (:input d)]
    [#(chat-deltas world events)]))
