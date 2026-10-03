(ns collider.game.commands.teleport
  "The tp, rotate and spectate commands."
  (:require [collider.game.camera :as camera]
            [collider.game.command.selector :as sel]
            [collider.game.commands.pos :as pos]
            [collider.game.commands.reply
             :refer [answer entity-name fail say]]
            [collider.game.entity :as entity]
            [collider.game.level :as level]
            [collider.game.mob.nav :as nav]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.sleep :as sleep]
            [collider.vec :as v]
            [collider.world.chunk :as chunk])
  (:import (collider.game.mob Steer)
           (java.util Locale)))

(set! *warn-on-reflection* true)

(defn- xyz [p] [(v/x p) (v/y p) (v/z p)])

(def ^:private still (v/v3 [0.0 0.0 0.0]))

(defn- f32 ^double [x] (double (float x)))

(defn- pitch-set ^double [x]
  (f32 (max -90.0 (min 90.0 (f32 (rem (float x) (float 360.0)))))))

(defn- rel-bits [rel same?]
  (reduce (fn [^long b ^long a]
            (cond-> (bit-or b (bit-shift-left 1 (+ 5 a)))
              same? (bit-or (bit-shift-left 1 a))))
          (cond-> 0 (:y-rot rel) (bit-or 8) (:x-rot rel) (bit-or 16))
          (filter int? rel)))

(defn- crossed [lv eid dim pos [yaw pitch] rel]
  (let [e (get-in lv [:entities eid])
        known (sort-by chunk/id->pos (seq (:sent-chunks e)))
        seen (sort (seq (:tracking e)))
        sent (if (set? rel)
               [0.0 0.0 (rel-bits rel false)]
               [yaw pitch 0])]
    [[:change-dimension eid dim pos yaw pitch]
     (out/to eid (out/change-dimension dim pos sent known seen))]))

(defn- sent-angle [rel? v old]
  (if rel? (sel/wrapped (- (double v) (double old))) v))

(defn- sent-turn [e [yaw pitch] rel]
  [(sent-angle (:y-rot rel) yaw (:yaw e 0.0))
   (sent-angle (:x-rot rel) pitch (:pitch e 0.0))])

(defn- known-vel [e]
  (if (= :player (:type e)) (:client-vel e) (:vel e)))

(defn- kept-vel [e rel grounded?]
  (let [old (or (known-vel e) still)
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
            at (packet-pos e pos rel)
            [y p] (sent-turn e turn rel)]
        [(out/to id (out/teleport at y p bits))]))))

(defn- woken [lv id e]
  (if (:sleeping e)
    (let [{up :pos yaw :yaw ds :deltas} (sleep/wake lv id)
          ds (vec ds)
          left (dec (count (sleep/sleepers lv)))]
      [(conj (pop ds) (sleep/announcement lv left) (peek ds))
       (assoc e :pos (v/v3 up) :sleeping nil :yaw yaw :pitch 0.0)])
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
  (let [lv (sel/level-view world from)
        [wake e] (woken lv id e0)
        own? (set? rel)
        turn (if (:own-turn rel) [(:yaw e 0.0) (:pitch e 0.0)] turn)
        moved (if (= from to)
                (let [pos (if own? (shifted pos rel e0 e) pos)]
                  (player-placed id e pos turn rel))
                (crossed lv id to pos turn rel))]
    (sel/in-level world from (concat wake moved))))

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
  (let [lv (sel/level-view world to)
        cid (chunk/block-chunk (mapv #(long (Math/floor %)) pos))]
    (when-not (or (contains? (:chunks lv) cid)
                  (contains? (:loading lv) cid))
      (level/read-absent-deltas
       {cid (level/read-absent lv cid)}))))

(defn- arrived [world id e to pos [yaw pitch] rel]
  (when-let [m (recreated world id e pos yaw pitch rel)]
    (let [ds (arrival-chunk world to pos)]
      (sel/in-level world to (concat ds [[:spawn-entity m]])))))

(defn- entity-moved [world id from e to pos [yaw pitch :as turn] rel]
  (let [rel (if (set? rel) rel #{})]
    (if (= from to)
      (let [m (placed-props e pos yaw pitch rel)]
        (sel/in-level world from [[:merge-entity id m]]))
      (concat
        (sel/in-level world from [[:remove-entity id]])
        (arrived world id e to pos turn rel)))))

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

(def ^:private own-rotation #{:y-rot :x-rot :own-turn})

(defn- tp-moves [world placed pos turn-of look]
  (let [to (sel/source-dim world)
        rel (get-in world [:source :relative] #{})]
    (mapcat (fn [x]
              (let [[t r] (turn-of x)]
                (concat (moved world x to pos t (into rel r))
                        (when look (look x pos)))))
            placed)))

(defn- own-turn-of [x] [(own-turn x) own-rotation])

(defn- tp-at
  ([world eid placed pos] (tp-at world eid placed pos nil nil))
  ([world eid placed pos turn-of look]
   (cond
     (empty? placed) (fail eid "argument.entity.notfound.entity")
     (not (pos/spawnable? pos))
     (fail eid "commands.teleport.invalidPosition")
     :else
     (concat (tp-moves world placed pos (or turn-of own-turn-of) look)
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

(defn- tp-entity-deltas [world eid placed s]
  (let [[_ dim d] (first (sel/selected world eid s))]
    (cond
      (or (nil? d) (empty? placed))
      (fail eid "argument.entity.notfound.entity")
      (not (pos/spawnable? (xyz (:pos d))))
      (fail eid "commands.teleport.invalidPosition")
      :else (to-entity world eid placed dim d))))

(defn- tp-deltas [world eid p]
  (tp-at world eid (sel/self world eid) (vec p)))

(defn- tp-to-deltas [world eid [s]]
  (tp-entity-deltas world eid (sel/self world eid) s))

(defn- tp-targets-deltas [world eid [s & p]]
  (tp-at world eid (sel/selected world eid s) (vec p)))

(defn- source-turn [world eid [ry yv] [rp pv]]
  (let [src (get-in world [:entities eid])]
    [(f32 (+ (double yv) (if ry (double (:yaw src 0.0)) 0.0)))
     (f32 (+ (double pv) (if rp (double (:pitch src 0.0)) 0.0)))]))

(defn- turned-by [rel? v old]
  (if rel?
    (+ (double old) (sel/wrapped (- (double v) (double old))))
    (sel/wrapped v)))

(defn- rotated-turn [[_ _ e] [yaw pitch] ry rp]
  [[(turned-by ry yaw (:yaw e 0.0))
    (pitch-set (turned-by rp pitch (:pitch e 0.0)))]
   (cond-> #{} ry (conj :y-rot) rp (conj :x-rot))])

(defn- tp-rotated-deltas [world eid [s x y z yaw pitch]]
  (let [turn (source-turn world eid yaw pitch)]
    (tp-at world eid (sel/selected world eid s) [x y z]
           #(rotated-turn % turn (first yaw) (first pitch)) nil)))

(def ^:private deg (f32 (/ 180.0 (f32 Math/PI))))

(defn- look-angles
  "Returns the yaw and pitch that look from feet at from to pos."
  [[fx fy fz] [px py pz]]
  (let [xd (- (double px) (double fx)) yd (- (double py) (double fy))
        zd (- (double pz) (double fz))
        sd (Math/sqrt (+ (* xd xd) (* zd zd)))
        pitch (sel/wrapped (float (- (* (Steer/atan2 yd sd) deg))))
        turn (float (* (Steer/atan2 zd xd) deg))
        yaw (sel/wrapped (- turn (float 90.0)))]
    [(f32 yaw) (pitch-set pitch)]))

(defn- anchored [e anchor]
  (let [[x y z] (xyz (:pos e))]
    (if (= :eyes anchor)
      [x (+ (double y) (double (float (entity/eye-height e)))) z]
      [x y z])))

(defn- rotated [world id dim yaw pitch fx]
  (let [turn {:yaw yaw :head-yaw yaw :pitch pitch}]
    (sel/in-level world dim
                  (cond-> [[:merge-entity id turn]]
                    fx (conj (out/to id fx))))))

(defn- look-from [world target fx-of]
  (fn [[id dim e] p]
    (let [[yaw pitch] (look-angles p target)]
      (rotated world id dim yaw pitch
               (when (= :player (:type e)) (fx-of target))))))

(defn- tp-facing-deltas [world eid [s x y z fx fy fz]]
  (let [target [fx fy fz]]
    (tp-at world eid (sel/selected world eid s) [x y z] nil
           (look-from world target #(out/look-at :feet % nil nil)))))

(defn- facing-entity-at [world eid s p o oid anchor]
  (tp-at world eid (sel/selected world eid s) p nil
         (look-from world (anchored o anchor)
                    #(out/look-at :feet % oid anchor))))

(defn- tp-facing-entity-deltas [world eid [s x y z other anchor]]
  (let [[oid _ o] (first (sel/selected world eid other))]
    (if o
      (facing-entity-at world eid s [x y z] o oid (or anchor :feet))
      (fail eid "argument.entity.notfound.entity"))))

(defn- tp-targets-to-deltas [world eid [s dest]]
  (tp-entity-deltas world eid (sel/selected world eid s) dest))

(defn- turned-to
  "Returns the yaw and pitch that the rotation arguments give entity
  e, and the values the player packet carries."
  [world eid e [ry yv] [rp pv]]
  (let [src (get-in world [:entities eid])
        arg #(f32 (if %1 (+ (double %2) (double (or %3 0.0))) %2))
        y (arg ry yv (:yaw src)) x (arg rp pv (:pitch src))
        dy (if ry (f32 (- y (f32 (:yaw e 0.0)))) y)
        dx (if rp (f32 (- x (f32 (:pitch e 0.0)))) x)
        ay (if ry (f32 (+ (f32 (:yaw e 0.0)) dy)) dy)
        ax (if rp (f32 (+ (f32 (:pitch e 0.0)) dx)) dx)
        sent [dy (boolean ry) dx (boolean rp)]]
    [ay (pitch-set (max -90.0 (min 90.0 ax))) sent]))

(defn- rotate-report [eid e]
  (say eid "commands.rotate.success" (entity-name e)))

(defn- rotate-deltas [world eid [s yaw pitch]]
  (if-let [[id dim e] (first (sel/selected world eid s))]
    (let [[ay ax sent] (turned-to world eid e yaw pitch)
          fx (when (= :player (:type e))
               (apply out/player-rotation sent))]
      (concat (rotated world id dim ay ax fx) (rotate-report eid e)))
    (fail eid "argument.entity.notfound.entity")))

(defn- faced [world eid [id dim e] pos fx]
  (let [[yaw pitch] (look-angles (xyz (:pos e)) pos)]
    (concat (rotated world id dim yaw pitch
                     (when (= :player (:type e)) fx))
            (rotate-report eid e))))

(defn- facing-deltas [world eid [s x y z]]
  (if-let [x0 (first (sel/selected world eid s))]
    (faced world eid x0 [x y z] (out/look-at :feet [x y z] nil nil))
    (fail eid "argument.entity.notfound.entity")))

(defn- facing-entity-deltas [world eid [s other anchor]]
  (let [x0 (first (sel/selected world eid s))
        [oid _ o] (first (sel/selected world eid other))
        anchor (or anchor :feet)]
    (if (and x0 o)
      (let [p (anchored o anchor)]
        (faced world eid x0 p (out/look-at :feet p oid anchor)))
      (fail eid "argument.entity.notfound.entity"))))

(defn- camera-set
  [world [id dim e :as x] [tid tdim t]]
  (if (or (nil? t) (= dim tdim))
    (let [lv (sel/level-view world dim)]
      (sel/in-level world dim (camera/set-deltas lv id e tid)))
    (let [ds [[:merge-entity id {:camera tid}]
              (out/to id (out/camera tid))]]
      (concat (moved world x tdim (vec (xyz (:pos t))) (own-turn x)
                     own-rotation)
              (sel/in-level world tdim ds)))))

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
  (let [x (first (sel/player-selected world eid player))
        t (when target (first (sel/selected world eid target)))]
    (cond
      (nil? x) (fail eid "argument.entity.notfound.player")
      (and target (nil? t))
      (fail eid "argument.entity.notfound.entity")
      :else (spectated world eid x t))))

(defn spectator-teleport
  "Returns the deltas of spectator eid going to the entity of uuid u,
  nil when eid is no spectator or u is not found."
  [world eid u]
  (let [x [eid (:dim world) (get-in world [:entities eid])]
        [_ dim d] (sel/by-uuid world false u)]
    (when (and d (game-mode/spectator? (nth x 2)))
      (concat (camera/set-deltas world eid (nth x 2) nil)
              (moved world x dim (vec (xyz (:pos d)))
                     [(:yaw d 0.0) (:pitch d 0.0)] :entity)))))

(def handlers
  {:tp tp-deltas :tp-to tp-to-deltas :tp-targets tp-targets-deltas
   :tp-targets-to tp-targets-to-deltas
   :tp-targets-rotated tp-rotated-deltas
   :tp-targets-facing tp-facing-deltas
   :tp-targets-facing-entity tp-facing-entity-deltas
   :rotate rotate-deltas :rotate-facing facing-deltas
   :rotate-facing-entity facing-entity-deltas
   :spectate spectate-deltas})
