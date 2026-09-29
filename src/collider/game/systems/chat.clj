(ns collider.game.systems.chat
  "Chat lines, commands and tab completion."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.camera :as camera]
            [collider.game.clock :as clock]
            [collider.game.effect :as effect]
            [collider.game.experience :as xp]
            [collider.game.command.item-args :as item-args]
            [collider.game.command.reader :as cmd-reader]
            [collider.game.command.tree :as cmd]
            [collider.game.entity :as entity]
            [collider.game.game-mode :as game-mode]
            [collider.game.gamerules :as rules]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.nav :as nav]
            [collider.game.out :as out]
            [collider.game.schema :as schema]
            [collider.game.stack :as stack]
            [collider.game.state :as state]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.chunks :as chunks]
            [collider.game.systems.effects :as effects]
            [collider.game.systems.items :as items]
            [collider.game.systems.sleep :as sleep]
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
          (when (String/.startsWith s m i) [m st]))
        markers))

(defn- marked [^String s m st styles start close]
  (let [inner (String/.substring s start close)]
    (if (= m "`")
      [(merge styles st {:text inner})]
      (parse-runs inner (merge styles st)))))

(defn- span [^String s styles ^StringBuilder plain ^long i]
  (when-let [[^String m st] (marker-at s i)]
    (let [n (String/.length m)
          start (+ i n)
          close (^[String int] String/.indexOf s m start)]
      (if (> close start)
        [(+ close n) (marked s m st styles start close)]
        (do (^[String] StringBuilder/.append plain m)
            [start nil])))))

(defn- flushed [out styles ^StringBuilder plain]
  (if (pos? (StringBuilder/.length plain))
    (conj out (assoc styles :text (str plain)))
    out))

(defn- escape-at? [^String s ^long i]
  (and (= \\ (String/.charAt s i))
       (< (inc i) (String/.length s))
       (or (marker-at s (inc i))
           (= \\ (String/.charAt s (inc i))))))

(defn- step
  [^String s styles [i ^StringBuilder plain out]]
  (if (escape-at? s i)
    (do (^[char] StringBuilder/.append plain
                 (String/.charAt s (inc i)))
        [(+ 2 (long i)) plain out])
    (if-let [[end runs] (span s styles plain i)]
      (if runs
        [end (StringBuilder.) (into (flushed out styles plain) runs)]
        [end plain out])
      (do (^[char] StringBuilder/.append plain (String/.charAt s i))
          [(inc (long i)) plain out]))))

(defn parse-runs
  "Returns the styled runs of a line with chat markup."
  ([s] (parse-runs s {}))
  ([^String s styles]
   (loop [st [0 (StringBuilder.) []]]
     (if (>= (long (first st)) (String/.length s))
       (flushed (peek st) styles (second st))
       (recur (step s styles st))))))

(defn- joined [runs]
  (case (count runs)
    0 ""
    1 (first runs)
    {:text "" :extra runs}))

(defn tell
  "Returns the deltas that show the lines to one player."
  [eid & lines]
  (mapv #(out/to eid (out/system-chat (joined (parse-runs %))))
        (mapcat #(str/split-lines (str %)) lines)))

(defn- reports [ds admins?]
  (mapv (fn [d]
          (if (= :fx (nth d 0))
            [:fx (assoc (nth d 1) :feedback true :admins admins?)]
            d))
        ds))

(defn- success [ds] (reports ds true))

(defn- answer [ds] (reports ds false))

(defn- say* [eid key with]
  (let [msg {:translate key :with (vec with)}]
    (success [(out/to eid (out/system-chat msg))])))

(defn- say [eid key & with] (say* eid key with))

(defn- failure [text]
  (out/system-chat {:text "" :color "red" :extra [text]}))

(defn- fail [eid key & with]
  [(out/to eid (failure {:translate key :with (vec with)}))])

(defn- unloaded? [world positions]
  (let [held (chunks/needed-ids world)]
    (some #(let [id (chunk/block-chunk %)]
             (not (or (contains? (:chunks world) id)
                      (contains? held id))))
          positions)))

(defn- box [[ax ay az bx by bz]]
  (mapv (fn [a b] (sort [(long a) (long b)])) [ax ay az] [bx by bz]))

(defn- fill-changes
  [[[x1 x2] [y1 y2] [z1 z2]] st]
  (vec (for [z (range z1 (inc z2))
             y (range y1 (inc y2))
             x (range x1 (inc x2))]
         [[x y z] st])))

(defn- box-ids [[[x1 x2] _ [z1 z2]]]
  (for [cx (range (bit-shift-right (long x1) 4)
                  (inc (bit-shift-right (long x2) 4)))
        cz (range (bit-shift-right (long z1) 4)
                  (inc (bit-shift-right (long z2) 4)))]
    (chunk/pos->id cx cz)))

(defn- fetched [world bounds]
  (let [in? #(contains? (:chunks world) %)
        read (fn [id] [id (chunks/read-absent world id)])
        loaded (into {} (comp (remove in?) (map read))
                     (box-ids bounds))]
    [(reduce-kv #(assoc %1 %2 (:chunk %3)) (:chunks world) loaded)
     (chunks/read-absent-deltas loaded)]))

(defn- source-dim [world]
  (get-in world [:source :dim] (:dim world)))

(defn- source-pos [world]
  (get-in world [:source :pos]))

(defn- level-view [world dim]
  (let [server (:server world)]
    (if (or (= dim (:dim world)) (nil? server))
      world
      (assoc (state/level server dim) :server server))))

(defn- in-level [world dim ds]
  (cond
    (empty? ds) nil
    (= dim (:dim world)) (vec ds)
    :else [[:level-deltas dim (vec ds)]]))

(defn- source-level [world] (level-view world (source-dim world)))

(defn- filled [world eid bounds block]
  (let [dim (source-dim world)
        lv (level-view world dim)
        [chunks adds] (fetched lv bounds)
        changes (fill-changes bounds (block/state block))
        lv (assoc lv :chunks chunks)
        [ds n] (edit/command-deltas lv changes)]
    (if (zero? (long n))
      (concat (in-level world dim adds)
              (fail eid "commands.fill.failed"))
      (concat (in-level world dim (into (vec adds) ds))
              (say eid "commands.fill.success" (str n))))))

(defn- flat-in? [^long x ^long z]
  (and (<= -30000000 x) (< x 30000000)
       (<= -30000000 z) (< z 30000000)))

(defn- in-world?
  [world [x y z]]
  (and (chunk/in-level? world (long y))
       (flat-in? (long x) (long z))))

(defn- pos-error [world pos]
  (cond
    (unloaded? world [pos]) "argument.pos.unloaded"
    (not (in-world? world pos)) "argument.pos.outofworld"))

(defn- spawnable? [pos]
  (let [[x y z] (mapv #(long (Math/floor (double %))) pos)]
    (and (<= -20000000 (long y)) (< (long y) 20000000)
         (flat-in? x z))))

(defn- fill-deltas [world eid [ax ay az bx by bz block]]
  (if-let [k (some #(pos-error (source-level world) %)
                   [[ax ay az] [bx by bz]])]
    (fail eid k)
    (filled world eid (box [ax ay az bx by bz]) block)))

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

(defn- rule-set [world eid id rule v]
  (let [w (assoc-in world [:rules rule] v)]
    (concat [[:set-rule rule v]
             (out/all (out/game-rules (:rules w)))]
            (clocks-resent w rule)
            (say eid "commands.gamerule.set" id
                 (rules/serialize rule v)))))

(defn- rule-query [world eid id rule]
  (let [v (rules/serialize rule (get-in world [:rules rule]))
        msg {:translate "commands.gamerule.query" :with [id v]}]
    (answer [(out/to eid (out/system-chat msg))])))

(defn- rule-deltas [world eid rule text]
  (let [id (subs (rules/wire-name rule) 10)]
    (if (nil? text)
      (rule-query world eid id rule)
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
  (let [k (str "entity.minecraft." (data/snake (:type e)))
        nm (if (= :player (:type e)) (:name e) {:translate k})
        hover {:action :show-entity :id (:type e) :uuid (:uuid e)
               :name nm}]
    (cond
      (nil? (:uuid e)) nm
      (= :player (:type e))
      {:text nm :insertion nm :hover hover
       :click {:action :suggest-command
               :command (str "/tell " nm " ")}}
      :else (assoc nm :hover hover :insertion (str (:uuid e))))))

(def ^:private rarity-colors
  {:common "white" :uncommon "yellow" :rare "aqua"
   :epic "light_purple"})

(defn- item-name [item]
  {:translate "chat.square_brackets"
   :with [{:text "" :extra [(data/item-title item)]}]
   :color (rarity-colors (data/rarity item))
   :hover {:action :show-item :id item}})

(defn- levels [world]
  (if (:server world)
    (map (fn [d] [d (level-view world d)]) schema/dims)
    [[(:dim world) world]]))

(defn- player-entries [world]
  (sort-by first
           (for [[dim lv] (levels world)
                 [id e] (state/player-entries lv)]
             [id dim e])))

(defn- entity-entries [world]
  (for [[dim lv] (levels world)
        [id e] (sort-by key (:entities lv))]
    [id dim e]))

(defn- alive? [e]
  (not (and (:health e) (<= (double (:health e)) 0.0))))

(defn- typed? [{:keys [type not-type?]} e]
  (or (nil? type) (= (boolean not-type?) (not= type (:type e)))))

(defn- xyz [p] [(v/x p) (v/y p) (v/z p)])

(def ^:private still (v/v3 [0.0 0.0 0.0]))

(defn- dist-sq ^double [from e]
  (let [p (:pos e)
        d #(- (double %1) (double (nth from %2)))
        dx (d (v/x p) 0) dy (d (v/y p) 1) dz (d (v/z p) 2)]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- in-distance? [[lo hi] from e]
  (let [d (dist-sq from e) sq #(* (double %) (double %))]
    (and (or (nil? lo) (>= d (double (sq lo))))
         (or (nil? hi) (<= d (double (sq hi)))))))

(defn- near? [world sel dim e]
  (let [d (:distance sel)]
    (or (nil? d)
        (and (= dim (source-dim world))
             (in-distance? d (source-pos world) e)))))

(defn- matches? [world sel [_ dim e]]
  (and (typed? sel e)
       (or (not (:entities sel)) (alive? e))
       (near? world sel dim e)))

(defn- picked [world sel xs]
  (let [from (source-pos world)]
    (cond
      (:random sel)
      (when (seq xs)
        (let [r (random/of-key (:tick world) :selector)]
          [(nth (vec xs) (long (* (double r) (count xs))))]))
      (:nearest sel) (take 1 (sort-by #(dist-sq from (nth % 2)) xs))
      :else xs)))

(defn- by-uuid [world players? u]
  (some (fn [[id _ e :as x]]
          (when (= u (entity/uuid-of id e)) x))
        (if players? (player-entries world) (entity-entries world))))

(defn- by-name [world nm]
  (some #(when (= nm (:name (nth % 2))) %) (player-entries world)))

(defn- self-entry [world eid sel]
  (let [x [eid (:dim world) (get-in world [:entities eid])]]
    (when (and (peek x) (matches? world (dissoc sel :entities) x))
      [x])))

(defn- selected [world eid sel]
  (cond
    (:self sel) (self-entry world eid sel)
    (:name sel) (keep identity [(by-name world (:name sel))])
    (:uuid sel) (keep identity [(by-uuid world false (:uuid sel))])
    :else (->> (if (:entities sel)
                 (entity-entries world)
                 (player-entries world))
               (filter #(matches? world sel %))
               (picked world sel))))

(defn- player-selected [world eid sel]
  (if (:uuid sel)
    (keep identity [(by-uuid world true (:uuid sel))])
    (selected world eid sel)))

(declare rel-bits)

(defn- crossed [lv eid dim pos [yaw pitch] rel]
  (let [e (get-in lv [:entities eid])
        known (sort-by chunk/id->pos (seq (:sent-chunks e)))
        seen (sort (seq (:tracking e)))
        sent (if (set? rel)
               [0.0 0.0 (rel-bits rel false)]
               [yaw pitch 0])]
    [[:change-dimension eid dim pos yaw pitch]
     (out/to eid (out/change-dimension dim pos sent known seen))]))

(defn- rel-bits [rel same?]
  (reduce (fn [^long b ^long a]
            (cond-> (bit-or b (bit-shift-left 1 (+ 5 a)))
              same? (bit-or (bit-shift-left 1 a))))
          (bit-or 8 16) rel))

(defn- kept-vel [e rel grounded?]
  (let [old (or (:vel e) still)
        at #(if (contains? rel %) (double (nth (xyz old) %)) 0.0)]
    (v/v3 [(at 0) (if grounded? 0.0 (at 1)) (at 2)])))

(defn- packet-pos [e pos rel]
  (let [p (:pos e)]
    (mapv (fn [a] (if (contains? rel a)
                    (- (double (nth pos a)) (double (nth (xyz p) a)))
                    (double (nth pos a))))
          [0 1 2])))

(defn- player-teleport [id e pos turn rel]
  (let [[yaw pitch] turn]
    (if (= :entity rel)
      [(out/to id (out/teleport pos yaw pitch))]
      (let [bits (rel-bits rel true)
            at (packet-pos e pos rel)]
        [(out/to id (out/teleport at 0.0 0.0 bits))]))))

(defn- stood [ds e]
  (let [of (fn [tag] (some #(when (= tag (nth % 0)) (nth % 2)) ds))]
    (assoc e :pos (v/v3 (of :teleport)) :sleeping nil
           :yaw (:yaw (of :merge-entity)) :pitch 0.0)))

(defn- woken [lv id e]
  (if (:sleeping e)
    (let [ds (vec (sleep/wake-deltas lv id))
          left (dec (count (sleep/sleepers lv)))]
      [(conj (pop ds) (sleep/announcement lv left) (peek ds))
       (stood ds e)])
    [nil e]))

(defn- shifted [pos rel e0 e]
  (let [from (xyz (:pos e0)) to (xyz (:pos e))]
    (mapv (fn [a]
            (if (contains? rel a)
              (+ (- (double (nth pos a)) (double (nth from a)))
                 (double (nth to a)))
              (nth pos a)))
          [0 1 2])))

(defn- player-placed [id e pos [yaw pitch :as turn] rel]
  (into [[:teleport id pos]
         [:merge-entity id
          {:yaw yaw :pitch pitch :head-yaw yaw :on-ground true
           :vel (kept-vel e (if (set? rel) rel #{}) true)}]]
        (player-teleport id e pos turn rel)))

(defn- player-moved [world id from e0 to pos turn rel]
  (let [lv (level-view world from)
        [wake e] (woken lv id e0)
        own? (set? rel)
        turn (if own? [(:yaw e 0.0) (:pitch e 0.0)] turn)]
    (in-level
      world from
      (concat
        wake
        (if (= from to)
          (let [pos (if own? (shifted pos rel e0 e) pos)]
            (player-placed id e pos turn rel))
          (crossed lv id to pos turn rel))))))

(defn- placed-props [e pos yaw pitch rel]
  (cond-> {:pos (v/v3 pos) :yaw yaw :pitch pitch :head-yaw yaw
           :vel (kept-vel e rel true) :on-ground true}
    (:nav e) (assoc :nav (:nav (nav/stop e)))))

(defn- recreated [world id e pos yaw pitch rel]
  (let [t (:tick world)]
    (when-let [m (entity/loaded (entity/saved e t) t)]
      (assoc m :pos (v/v3 pos) :yaw yaw :pitch pitch :head-yaw yaw
             :vel (kept-vel e rel false)
             :uuid (entity/uuid-of id e)))))

(defn- arrival-chunk [world to pos]
  (let [lv (level-view world to)
        cid (chunk/block-chunk (mapv #(long (Math/floor %)) pos))]
    (when-not (or (contains? (:chunks lv) cid)
                  (contains? (:loading lv) cid))
      (chunks/read-absent-deltas
       {cid (chunks/read-absent lv cid)}))))

(defn- entity-moved [world id from e to pos [yaw pitch] rel]
  (let [rel (if (set? rel) rel #{})]
    (if (= from to)
      (let [m (placed-props e pos yaw pitch rel)]
        (in-level world from [[:merge-entity id m]]))
      (concat
        (in-level world from [[:remove-entity id]])
        (when-let [m (recreated world id e pos yaw pitch rel)]
          (let [ds (arrival-chunk world to pos)]
            (in-level world to (concat ds [[:spawn-entity m]]))))))))

(defn- moved
  [world [id from e] to pos [yaw pitch] rel]
  (let [turn [(double yaw) (double pitch)]]
    (if (= :player (:type e))
      (player-moved world id from e to pos turn rel)
      (entity-moved world id from e to pos turn rel))))

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

(defn- tp-pos-deltas [world eid placed pos]
  (let [to (source-dim world)
        rel (get-in world [:source :relative] #{})]
    (cond
      (empty? placed) (fail eid "argument.entity.notfound.entity")
      (not (spawnable? pos))
      (fail eid "commands.teleport.invalidPosition")
      :else
      (concat (mapcat #(moved world % to pos (own-turn %) rel)
                      placed)
              (pos-report eid placed pos)))))

(defn- entity-report [eid placed d]
  (if (= 1 (count placed))
    (say eid "commands.teleport.success.entity.single"
         (entity-name (nth (first placed) 2)) (entity-name d))
    (say eid "commands.teleport.success.entity.multiple"
         (count placed) (entity-name d))))

(defn- to-entity [world eid placed dim d]
  (let [pos (vec (xyz (:pos d))) turn [(:yaw d 0.0) (:pitch d 0.0)]]
    (concat (mapcat #(moved world % dim pos turn :entity) placed)
            (entity-report eid placed d))))

(defn- tp-entity-deltas [world eid placed sel]
  (let [[_ dim d] (first (selected world eid sel))]
    (cond
      (or (nil? d) (empty? placed))
      (fail eid "argument.entity.notfound.entity")
      (not (spawnable? (xyz (:pos d))))
      (fail eid "commands.teleport.invalidPosition")
      :else (to-entity world eid placed dim d))))

(defn- self [world eid]
  (selected world eid {:self true}))

(defn- tp-deltas [world eid pos]
  (tp-pos-deltas world eid (self world eid) (vec pos)))

(defn- tp-to-deltas [world eid [sel]]
  (tp-entity-deltas world eid (self world eid) sel))

(defn- tp-targets-deltas [world eid [sel & pos]]
  (tp-pos-deltas world eid (selected world eid sel) (vec pos)))

(defn- tp-targets-to-deltas [world eid [sel dest]]
  (tp-entity-deltas world eid (selected world eid sel) dest))

(defn- given [world eid [id dim e] proto n]
  (let [lv (level-view world dim)
        inv (or (:inventory e) {})
        item (:item proto)
        [changes left] (items/add-stack inv (assoc proto :count n))
        drop #(items/dropped lv id % true 0)]
    (concat
      (in-level world dim
                (concat
                  (map (fn [[slot stack]] [:set-slot id slot stack])
                       changes)
                  (when left [[:spawn-entity (drop left)]])))
      (say eid "commands.give.success.single"
           n (item-name item) (entity-name e)))))

(defn- give-limit [proto]
  (* 100 (long (or (stack/component proto :max-stack-size) 1))))

(defn- give-deltas [world eid [sel input n]]
  (let [xs (player-selected world eid sel)
        proto (item-args/item-stack input 1)
        most (when-not (cmd-reader/error? proto) (give-limit proto))]
    (cond
      (empty? xs) (fail eid "argument.entity.notfound.player")
      (cmd-reader/error? proto)
      (apply fail eid (:key proto) (:args proto))
      (> (long n) (long most))
      (fail eid "commands.give.failed.toomanyitems" most
            (item-name (:item proto)))
      :else (mapcat #(given world eid % proto n) xs))))

(defn- killed [world [id dim e]]
  (in-level world dim
            (cond
              (not= :player (:type e)) [[:remove-entity id]]
              (state/client-loaded? e (long (:tick world)))
              [[:merge-entity id {:health 0.0}]])))

(defn- kill-report [eid xs]
  (if (= 1 (count xs))
    (say eid "commands.kill.success.single"
         (entity-name (nth (first xs) 2)))
    (say eid "commands.kill.success.multiple" (count xs))))

(defn- kill-deltas [world eid [sel]]
  (let [xs (selected world eid sel)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.entity")
      (concat (mapcat #(killed world %) xs)
              (kill-report eid xs)))))

(defn- summoned [world eid type at]
  (let [t (:tick world)
        dim (source-dim world)
        kind {:translate (str "entity.minecraft." (name type))}
        msg {:translate "commands.summon.success" :with [kind]}
        mob (mobs/egg-mob type at [t eid :summon] t dim)]
    (concat (in-level world dim [[:spawn-entity mob]])
            (success [(out/to eid (out/system-chat msg))]))))

(defn- summon-deltas [world eid [type x y z]]
  (let [p (source-pos world)
        at [(double (or x (nth p 0)))
            (double (or y (nth p 1)))
            (double (or z (nth p 2)))]]
    (if (spawnable? at)
      (summoned world eid type at)
      (fail eid "commands.summon.invalidPosition"))))

(defn- block-set [world eid [x y z :as pos] st]
  (let [dim (source-dim world)
        lv (level-view world dim)
        [ds n] (edit/command-deltas lv [[pos st]])]
    (if (zero? (long n))
      (fail eid "commands.setblock.failed")
      (concat (in-level world dim ds)
              (say eid "commands.setblock.success"
                   (str x) (str y) (str z))))))

(defn- setblock-deltas [world eid [x y z block]]
  (let [pos [x y z]
        k (pos-error (source-level world) pos)]
    (if k
      (fail eid k)
      (block-set world eid pos (block/state block)))))

(defn- block-under [world [x y z]]
  (let [p (source-pos world)
        at #(long (Math/floor (double (nth p %))))]
    [(long (or x (at 0)))
     (long (or y (at 1)))
     (long (or z (at 2)))]))

(defn- dimension-id [dim] (str "minecraft:" (data/snake dim)))

(defn- wrapped [a]
  (let [r (float (rem (float a) (float 360.0)))]
    (cond
      (>= r (float 180.0)) (float (- r (float 360.0)))
      (< r (float -180.0)) (float (+ r (float 360.0)))
      :else r)))

(defn- turn-of [world eid yaw pitch]
  (let [e (get-in world [:entities eid])
        get (fn [[rel? v] own]
              (let [base (if rel? (double (or own 0.0)) 0.0)]
                (float (+ (double v) base))))
        y (if yaw (get yaw (:yaw e)) (float 0.0))
        p (if pitch (get pitch (:pitch e)) (float 0.0))]
    [(wrapped y) (float (max -90.0 (min 90.0 (double p))))]))

(defn- out-of-bounds? [pos]
  (and (every? some? pos) (not (spawnable? pos))))

(defn- same-spawn? [world dim at turn]
  (and (= dim (:world-spawn-dimension world :overworld))
       (= at (vec (:world-spawn world)))
       (= (mapv double turn) (state/spawn-turn world))))

(defn- world-spawn-set [world eid at [yaw pitch :as turn]]
  (let [dim (source-dim world)
        with (conj (mapv str at) (str yaw) (str pitch)
                   (dimension-id dim))
        msg {:translate "commands.setworldspawn.success" :with with}]
    (concat [[:set-world-spawn dim at [yaw pitch]]]
            (when-not (same-spawn? world dim at turn)
              [(out/everyone (out/default-spawn dim at yaw pitch))])
            (success [(out/to eid (out/system-chat msg))]))))

(defn- world-spawn-deltas
  [world eid [x y z yaw pitch]]
  (if (out-of-bounds? [x y z])
    (fail eid "argument.pos.outofbounds")
    (world-spawn-set world eid (block-under world [x y z])
                     (turn-of world eid yaw pitch))))

(defn- spawn-report [eid xs with]
  (if (= 1 (count xs))
    (say* eid "commands.spawnpoint.success.single"
          (conj with (entity-name (nth (first xs) 2))))
    (say* eid "commands.spawnpoint.success.multiple"
          (conj with (count xs)))))

(defn- spawnpoint-set [world eid xs at [yaw pitch]]
  (let [dim (source-dim world)
        spawn {:dimension dim :pos at :yaw (double yaw)
               :pitch (double pitch)}
        set (fn [[id d]]
              (in-level world d
                        [[:merge-entity id {:forced-spawn spawn}]]))
        with (conj (mapv str at) (str yaw) (str pitch)
                   (dimension-id dim))]
    (concat (mapcat set xs) (spawn-report eid xs with))))

(defn- spawnpoint-deltas [world eid [sel x y z yaw pitch]]
  (let [xs (player-selected world eid sel)]
    (cond
      (empty? xs) (fail eid "argument.entity.notfound.player")
      (out-of-bounds? [x y z]) (fail eid "argument.pos.outofbounds")
      :else (let [at (block-under world [x y z])
                  turn (turn-of world eid yaw pitch)]
              (spawnpoint-set world eid xs at turn)))))

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
    (cons [:set-weather m]
          (success [(out/to eid (out/system-chat msg))]))))

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
  (let [dim (source-dim world)
        k (or k (clock/default-of dim))]
    (cond
      (= :time-gametime op)
      (answer (say eid "commands.time.query.gametime"
                   (bigint (:tick world))))
      (nil? k) (fail eid "commands.time.no_default_clock"
                     (data/wire dim))
      :else (clock-deltas world eid op k v))))

(defn- weather-command-deltas [world eid op args]
  (let [given (first args)]
    (case op
      :weather-clear (weather-deltas world eid :clear given)
      :weather-rain (weather-deltas world eid :rain given)
      :weather-thunder (weather-deltas world eid :thunder given))))

(defn- mode-name [mode]
  {:translate (str "gameMode." (name mode))})

(defn- told-of-mode [world id mode]
  (when (get-in world [:rules :send-command-feedback] true)
    [(out/to id (out/system-chat
                  {:translate "gameMode.changed"
                   :with [(mode-name mode)]}))]))

(defn- mode-report
  [world eid [id _ e] mode]
  (if (= id eid)
    (say eid "commands.gamemode.success.self" (mode-name mode))
    (concat (told-of-mode world id mode)
            (say eid "commands.gamemode.success.other"
                 (entity-name e) (mode-name mode)))))

(defn- mode-change [world [id dim e] mode]
  (game-mode/change (level-view world dim) id e mode))

(defn- mode-set
  [world eid mode [_ dim :as x]]
  (when-let [ds (seq (mode-change world x mode))]
    (concat (in-level world dim ds) (mode-report world eid x mode))))

(defn- gamemode-deltas [world eid [mode sel]]
  (let [xs (player-selected world eid sel)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.player")
      (mapcat #(mode-set world eid mode %) xs))))

(defn- enforced [world mode]
  (when mode
    (mapcat (fn [[_ dim :as x]]
              (in-level world dim (mode-change world x mode)))
            (player-entries world))))

(defn- default-mode-deltas
  [world eid [mode]]
  (let [cfg (assoc (:config world) :game-mode mode)
        forced (game-mode/forced-mode (assoc world :config cfg))]
    (concat [[:set-config cfg]]
            (enforced world forced)
            (say eid "commands.defaultgamemode.success"
                 (mode-name mode)))))

(defn- reload-deltas [_world eid _args]
  (cons (out/to eid (out/reload))
        (say eid "commands.reload.success")))

(defn- camera-set
  [world [id dim e :as x] [tid tdim t]]
  (if (or (nil? t) (= dim tdim))
    (let [lv (level-view world dim)]
      (in-level world dim (camera/set-deltas lv id e tid)))
    (concat (moved world x tdim (vec (xyz (:pos t))) (own-turn x) #{})
            (in-level world tdim
                      [[:merge-entity id {:camera tid}]
                       (out/to id (out/camera tid))]))))

(defn- spectate-report [eid t]
  (let [k (if t "started" "stopped")
        msg {:translate (str "commands.spectate.success." k)
             :with (if t [(entity-name t)] [])}]
    (answer [(out/to eid (out/system-chat msg))])))

(defn- spectated
  [world eid [id _ e :as x] [tid _ t :as target]]
  (cond
    (= id tid) (fail eid "commands.spectate.self")
    (not (game-mode/spectator? e))
    (fail eid "commands.spectate.not_spectator" (entity-name e))
    :else (concat (camera-set world x target)
                  (spectate-report eid t))))

(defn- spectate-deltas [world eid [target player]]
  (let [x (first (player-selected world eid player))
        t (when target (first (selected world eid target)))]
    (cond
      (nil? x) (fail eid "argument.entity.notfound.player")
      (and target (nil? t))
      (fail eid "argument.entity.notfound.entity")
      :else (spectated world eid x t))))

(defn- entity-tp-deltas [world eid u]
  (let [x [eid (:dim world) (get-in world [:entities eid])]
        [_ dim d] (by-uuid world false u)]
    (when (and d (game-mode/spectator? (nth x 2)))
      (concat (camera/set-deltas world eid (nth x 2) nil)
              (moved world x dim (vec (xyz (:pos d)))
                     [(:yaw d 0.0) (:pitch d 0.0)] :entity)))))

(defn- effect-title [k]
  {:translate (str "effect.minecraft." (data/snake k))})

(defn- given-ticks ^long [k secs]
  (cond
    (nil? secs) (if (effect/instant? k) 1 600)
    (= :infinite secs) effect/infinite
    (effect/instant? k) (long secs)
    :else (* 20 (long secs))))

(defn- effect-changed [world [id dim e] f]
  (when (effects/living? e)
    (let [acc (f (effects/account id e))]
      (when (:landed? acc)
        (or (in-level world dim (effects/deltas acc e)) [])))))

(defn- effect-report [eid xs n key-of with]
  (if (= 1 (count xs))
    (apply say eid (key-of "single")
           (with (entity-name (nth (first xs) 2))))
    (apply say eid (key-of "multiple") (with n))))

(defn- effect-run [world eid xs f fail-key key-of with]
  (let [dss (keep #(effect-changed world % f) xs)]
    (if (empty? dss)
      (fail eid fail-key)
      (concat (apply concat dss)
              (effect-report eid xs (count xs) key-of with)))))

(defn- effect-give-deltas [world eid [sel k secs amp hide]]
  (let [xs (selected world eid sel)
        d (given-ticks k secs)
        i (effect/instance d (or amp 0) false (not hide))
        key-of #(str "commands.effect.give.success." %)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.entity")
      (effect-run world eid xs #(effects/land % k i)
                  "commands.effect.give.failed" key-of
                  (fn [who] [(effect-title k) who (quot d 20)])))))

(defn- clear-parts [k]
  (if k
    ["specific" #(effects/take-off % k) #(vector (effect-title k) %)]
    ["everything" effects/take-all vector]))

(defn- effect-clear-deltas [world eid [sel k]]
  (let [xs (if sel (selected world eid sel) (self world eid))
        [what f with] (clear-parts k)
        base (str "commands.effect.clear." what)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.entity")
      (effect-run world eid xs f (str base ".failed")
                  #(str base ".success." %) with))))

(defn- chimes [e acc]
  (for [vol (:chimes acc)]
    (out/all (out/sound :player/levelup (:pos e) vol 1.0))))

(defn- xp-of [world e]
  (xp/account e (- (long (:tick world)) (long (:born e 0)))))

(defn- xp-changed [world [id dim e] f]
  (let [acc (f (xp-of world e))
        ds (cons [:merge-entity id (xp/marks acc)] (chimes e acc))]
    (in-level world dim ds)))

(defn- xp-unit [unit] (or unit "points"))

(defn- xp-report [eid xs op unit amount]
  (let [base (str "commands.experience." op "." unit ".success.")]
    (if (= 1 (count xs))
      (say eid (str base "single") amount
           (entity-name (nth (first xs) 2)))
      (say eid (str base "multiple") amount (count xs)))))

(defn- xp-add-deltas [world eid [sel amount unit]]
  (let [xs (player-selected world eid sel)
        unit (xp-unit unit)
        f (if (= "levels" unit) xp/give-levels xp/give-points)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.player")
      (concat (mapcat #(xp-changed world % (fn [a] (f a amount))) xs)
              (xp-report eid xs "add" unit amount)))))

(defn- settable? [world unit amount [_ _ e]]
  (or (= "levels" unit)
      (< (long amount) (xp/needed (:level (xp-of world e))))))

(defn- xp-set-deltas [world eid [sel amount unit]]
  (let [xs (player-selected world eid sel)
        unit (xp-unit unit)
        f (if (= "levels" unit) xp/set-levels xp/set-points)
        g (fn [a] (f a amount))
        ok (filter #(settable? world unit amount %) xs)]
    (cond
      (empty? xs) (fail eid "argument.entity.notfound.player")
      (empty? ok) (fail eid "commands.experience.set.points.invalid")
      :else (concat (mapcat #(xp-changed world % g) ok)
                    (xp-report eid xs "set" unit amount)))))

(defn- xp-query-deltas [world eid [sel unit]]
  (if-let [[_ _ e] (first (player-selected world eid sel))]
    (let [acc (xp-of world e)
          n (if (= "levels" unit) (:level acc) (xp/points acc))]
      (say eid (str "commands.experience.query." unit)
           (entity-name e) n))
    (fail eid "argument.entity.notfound.player")))

(def ^:private commands
  {:xp-add xp-add-deltas :xp-set xp-set-deltas
   :xp-query xp-query-deltas
   :effect-give effect-give-deltas :effect-clear effect-clear-deltas
   :tp tp-deltas :tp-to tp-to-deltas :tp-targets tp-targets-deltas
   :tp-targets-to tp-targets-to-deltas :give give-deltas
   :kill kill-deltas
   :summon summon-deltas :setblock setblock-deltas
   :setworldspawn world-spawn-deltas :spawnpoint spawnpoint-deltas
   :fill fill-deltas :reload reload-deltas
   :gamemode gamemode-deltas :defaultgamemode default-mode-deltas
   :spectate spectate-deltas})

(defn- world-command-deltas [world eid [_ op & args]]
  (if-let [f (commands op)]
    (f world eid args)
    (case op
      :gamerule (rule-deltas world eid (first args) (second args))
      (:weather-clear :weather-rain :weather-thunder)
      (weather-command-deltas world eid op args)
      (:time-set :time-add :time-marker :time-pause :time-resume
       :time-rate :time-query :time-timeline :time-repetition
       :time-gametime)
      (time-deltas world eid op args)
      (tell eid (str "unknown world command: " op)))))

(defn- sourced [world r origin]
  (let [pos (if (contains? r :origin) (:origin r) origin)]
    (assoc world :source
           {:dim (:dim r (:dim world)) :pos pos
            :relative (:relative r #{})})))

(def ^:private here
  {:translate "command.context.here" :color "red" :italic true})

(defn- context [^String s at]
  (let [c (min (long at) (count s))]
    {:text "" :color "gray"
     :click {:action :suggest-command :command (str "/" s)}
     :extra (cond-> (if (> c 10) ["..."] [])
              :always (conj (subs s (max 0 (- c 10)) c))
              (< c (count s))
              (conj {:text (subs s c) :color "red"
                     :underlined true})
              :always (conj here))}))

(defn- parse-failed [eid text r]
  (let [at (:cursor r)]
    (cond-> [(out/to eid (failure (:failure r)))]
      at (conj (out/to eid (failure (context (subs text 1) at)))))))

(defn- feedback? [world ds]
  (reduce (fn [on d]
            (if (= [:set-rule :send-command-feedback] (take 2 d))
              (nth d 2)
              on))
          (get-in world [:rules :send-command-feedback] true) ds))

(defn- admin-report [world eid m]
  (when-let [e (get-in world [:entities eid])]
    (out/except eid (out/system-chat
                      {:translate "chat.type.admin"
                       :with [(entity-name e) (:text m)]
                       :color "gray" :italic true}))))

(defn- report-deltas [world eid on? d]
  (let [m (when (= :fx (nth d 0)) (nth d 1))]
    (cond (not (:feedback m)) [d]
          (not on?) nil
          :else (cond-> [[:fx (dissoc m :feedback :admins)]]
                  (:admins m) (conj (admin-report world eid m))))))

(defn- reported [world eid ds]
  (let [on? (feedback? world ds)]
    (into [] (comp (mapcat #(report-deltas world eid on? %))
                   (remove nil?))
          ds)))

(defn- gamemaster? [world eid]
  (when-let [e (get-in world [:entities eid])]
    (<= (long cmd/gamemaster) (state/permission-level e))))

(def ^:private hidden
  {:failure {:translate "command.unknown.command" :with []}
   :cursor 0})

(defn- parsed [world eid text origin]
  (if (gamemaster? world eid)
    (cmd/parse text origin (:dim world :overworld))
    hidden))

(defn- command-deltas [world eid text]
  (let [origin (when-let [p (get-in world [:entities eid :pos])]
                 [(v/x p) (v/y p) (v/z p)])
        r (parsed world eid text origin)]
    (cond
      (:failure r) (parse-failed eid text r)
      (:error r) (tell eid (:error r))
      :else (let [w (sourced world r origin)
                  ds (world-command-deltas w eid (:delta r))]
              (reported w eid ds)))))

(defn- public-deltas [world eid text]
  (when-let [e (get-in world [:entities eid])]
    [(out/all (out/player-chat
                {:translate "chat.type.text"
                 :with [(entity-name e) text]}))]))

(defn- said-deltas [world eid raw]
  (let [text (str/trim (str raw))]
    (cond
      (str/blank? text) nil
      (str/starts-with? text "/") (command-deltas world eid text)
      :else (public-deltas world eid text))))

(defn- tab-deltas [world eid text target id]
  (let [{:keys [start texts]} (cmd/suggestions world text target)
        len (- (count text) (long start))]
    [(out/to eid (out/suggestions (or id 0) start len texts))]))

(defn- mode-changed [world eid mode]
  (when (gamemaster? world eid)
    (let [x [eid (:dim world) (get-in world [:entities eid])]]
      (reported world eid (mode-set world eid mode x)))))

(defn- event-deltas [world [tag eid text target id]]
  (case tag
    :chat (said-deltas world eid text)
    :tab-complete (tab-deltas world eid text target id)
    :change-game-mode (mode-changed world eid text)
    :teleport-to-entity (entity-tp-deltas world eid text)
    nil))

(defn- distance-fx [old new k fx]
  (let [n (get new k)]
    (when (not= (get old k) n) [(out/everyone (fx n))])))

(defn- config-loaded [world m]
  (let [old (:config world)]
    (concat [[:set-config (merge old m)]
             (out/everyone (out/reloaded))]
            (distance-fx old m :view-distance out/view-distance)
            (distance-fx old m :simulation-distance
              out/simulation-distance))))

(defn- commit-synced
  [world [_ commit]]
  [[:set-config (assoc (:config world) :commit commit)]])

(defn- config-event-deltas [world [tag eid m :as ev]]
  (case tag
    :config-loaded (config-loaded world m)
    :config-failed (fail eid "commands.reload.failure")
    :commit-synced (commit-synced world ev)
    nil))

(defn- one-deltas [world ev]
  (vec (concat (event-deltas world ev)
               (rules-event-deltas world ev)
               (config-event-deltas world ev))))

(defn- chat-deltas [world events]
  (state/fold-events world events one-deltas))

(defn chat
  "Turns the chat lines and commands of this tick into deltas."
  [world d]
  (let [events (:input d)]
    [#(chat-deltas world events)]))
