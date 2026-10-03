(ns collider.game.turn.tnt
  "The turn of a primed TNT. It moves, and when its fuse runs out it
  blasts in the same turn."
  (:require [collider.data.long-map :as lm]
            [collider.game.apply :as apply]
            [collider.game.areas :as areas]
            [collider.game.blast :as blast]
            [collider.game.block.tnt :as tnt]
            [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.entity.sections :as sections]
            [collider.game.entity.size :as size]
            [collider.game.mode :as game-mode]
            [collider.game.mob.push :as push]
            [collider.game.turn.overlay :as overlay]
            [collider.par :as par]
            [collider.vec :as v]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.motion :as motion]
            [collider.world.phys :as phys])
  (:import (collider.world Move)))

(set! *warn-on-reflection* true)

(defn- tnt-half ^double [] (size/half :tnt))

(defn- tnt-height ^double [] (size/height :tnt))

(defn- liquid-push [world pos vel]
  (liquid/entity-push (:chunks world) pos (tnt-half) (tnt-height) vel
                      (:dim world)))

(defn- stuck-now [world pos]
  (motion/stuck-speed (:chunks world) pos (tnt-half) (tnt-height)))

(def ^:private ^:const drag (double (float 0.98)))

(defn- stepped-vel
  "Returns the motion of a TNT after its drag, its bounce on the
  ground and the push of a liquid."
  [world pos [mx my mz] on-ground]
  (let [v (mapv #(* (double %) drag) [mx my mz])
        v (if on-ground (mapv * v [0.7 -0.5 0.7]) v)]
    (v/add v (liquid-push world pos v))))

(defn- tnt-move ^Move [world e]
  (let [[vx vy vz] (:vel e)
        d [(double vx) (- (double vy) 0.04) (double vz)]
        stuck (:stuck e)]
    (phys/move (:chunks world) (:pos e) (if stuck (mapv * d stuck) d)
               (tnt-half) (tnt-height))))

(defn- moved-speed [world mv pos on-ground stuck]
  (let [vel (phys/vel mv)]
    (cond stuck [0.0 0.0 0.0]
          on-ground (motion/stepped-speed (:chunks world) pos vel)
          :else vel)))

(defn- stepped
  "Returns the fields of TNT eid, e, after its move in its turn."
  [world eid e]
  (let [^Move mv (tnt-move world e)
        pos (phys/pos mv)
        on-ground (phys/on-ground? mv)
        stuck' (stuck-now world pos)
        moved (moved-speed world mv pos on-ground (:stuck e))]
    (cond-> {:pos pos :origin nil
             :vel (stepped-vel world pos moved on-ground)
             :on-ground on-ground
             :fuse (dec (long (:fuse e)))
             :arrived (push/arrived e pos (:tick world) eid)}
      (or stuck' (:stuck e)) (assoc :stuck stuck'))))

(defn- due? [e]
  (and (not (:origin e)) (<= (long (:fuse e)) 1)))

(defn- after-deltas
  "Returns the entities that the turns ds of the other kinds moved,
  as they are after them, by eid."
  [world ds]
  (let [es (:entities world) t (:tick world)
        f (fn [m eid eds]
            (if-let [e (get es eid)]
              (assoc m eid (apply/entity t e eds))
              m))]
    (reduce-kv f (lm/long-map) (deltas/entities-of ds))))

(defn- removals [ds]
  (into (sorted-set)
        (keep (fn [d] (when (= :remove-entity (nth d 0)) (nth d 1))))
        (deltas/world-of ds)))

(defn- now-of
  "Returns what gives a body by id as turn t sees it: as the turns of
  the other kinds left it when it comes before t in the tick list,
  else as it was."
  [s ^long t]
  (let [after (:after s) cur (:cur s)]
    (fn [^long id] (or (when (< id t) (get after id)) (get cur id)))))

(defn- gone [s id]
  (-> s
      (update :cur dissoc id) (update :after dissoc id)
      (update :idx #(some-> % (sections/removed id)))))

(defn- ended
  "Returns s without the bodies from lo to hi that the turns of the
  other kinds removed in their own turns."
  [s ^long lo ^long hi]
  (reduce gone s (subseq (:removed s) >= lo <= hi)))

(defn- indexed [s world t]
  (let [ds (:ds s)
        s (assoc s :after (after-deltas world ds) :mark t
                   :removed (removals ds)
                   :primed (tnt/primed-origins world))
        now (now-of s t)
        body (fn [[id _]]
               (let [e (now id)]
                 (when-not (game-mode/spectator? e) [id e])))
        s (assoc s :idx (sections/of (keep body (:cur s))))]
    (ended s Long/MIN_VALUE (dec (long t)))))

(defn- advanced
  "Returns s with the bodies that moved in the turns before t of the
  other kinds where they stand after them."
  [s ^long t]
  (let [f (fn [idx id e]
            (if (contains? (:at idx) id)
              (sections/placed idx id (:pos e))
              idx))
        lo (long (:mark s))
        moved (lm/range (:after s) lo (dec t))]
    (-> (assoc s :idx (reduce-kv f (:idx s) moved) :mark t)
        (ended lo (dec t)))))

(defn- written
  "Returns s after the deltas ds of body id."
  [s tick id ds]
  (let [f (fn [m] (if-let [e (get m id)]
                    (assoc m id (apply/entity tick e ds))
                    m))]
    (-> s (update :cur f) (update :after f))))

(defn- moved [s tick eid fields]
  (let [ds [[:merge-entity eid fields]]
        place #(some-> % (sections/placed eid (:pos fields)))]
    (-> (written s tick eid ds)
        (update :out into ds)
        (update :idx place))))

(defn- after-hit [tick m id ds]
  (when-let [e (get m id)] (apply/entity tick e ds)))

(defn- hit-of
  "Returns the function that gives [id h e a] of a body that blast b
  hits in s, where h is the hit and e and a are the body in :cur and
  in :after once it took it."
  [s tick b]
  (let [cur (:cur s) after (:after s)]
    (fn [body]
      (let [id (nth body 0) h (blast/hit b body) ds (:ds h)]
        (if (:gone? h)
          [id h]
          [id h (after-hit tick cur id ds)
           (after-hit tick after id ds)])))))

(defn- put [m id x] (if x (assoc! m id x) m))

(defn- took
  "Returns s after the hits of one blast, with the motions of the
  players it pushes by eid."
  [s hits]
  (let [step (fn [[cur after out mo] [id h e a]]
               [(put cur id e) (put after id a)
                (if (neg? (long id)) out (reduce conj! out (:ds h)))
                (cond-> mo (:motion h) (assoc id (:motion h)))])
        init [(transient (:cur s)) (transient (:after s))
              (transient (:out s)) {}]
        [cur after out mo] (reduce step init hits)
        s (assoc s :cur (persistent! cur) :after (persistent! after)
                   :out (persistent! out))]
    [(reduce gone s (keep #(when (:gone? (nth % 1)) (nth % 0)) hits))
     mo]))

(defn- spawned [s spawns]
  (reduce (fn [s e]
            (let [id (dec (long (:fresh s)))]
              (-> s
                  (assoc :fresh id)
                  (update :cur assoc id e)
                  (update :idx sections/placed id (:pos e))
                  (update :out conj [::fresh id]))))
          s spawns))

(defn- source
  "Returns the damage source of the blast of TNT eid, e. Its owner
  causes it while alive."
  [s eid e]
  (let [owner (:owner e)
        o (when owner (get (:cur s) owner))]
    {:type (if o :player-explosion :explosion) :direct eid
     :cause (when o owner) :player? (= :player (:type o))}))

(defn- spec
  "Returns the blast of TNT eid, e, at pos."
  [s eid e pos]
  (let [src (source s eid e)]
    {:center [(v/x pos) (+ (v/y pos) (/ (tnt-height) 16.0)) (v/z pos)]
     :power 4.0 :source :tnt :fire? false :by eid :src src
     :causer (some->> (:cause src) (get (:cur s)))
     :primed (:primed s)}))

(defn- blasted
  "Returns s after TNT eid, e, blasts at pos in world. Bodies take
  the blast before blocks."
  [s world eid e pos]
  (let [t (:tick world)
        b (blast/of world (spec s eid e pos))
        hits (blast/struck b (:idx s) (now-of s eid) (hit-of s t b))
        [s motions] (took s hits)
        {:keys [ds spawns]} (blast/finish b motions)]
    (-> s
        (update :w overlay/wrote eid ds)
        (update :out into ds)
        (spawned spawns))))

(defn- fused
  "Returns s after TNT eid, e, whose fuse ran out, left world, and
  blasts in tw where its move took it."
  [s world tw eid e fields]
  (let [blasts? (get-in world [:rules :tnt-explodes] true)
        s (if (or (:idx s) (not blasts?)) s (indexed s world eid))
        s (update (gone s eid) :out conj [:remove-entity eid])]
    (if blasts? (blasted s tw eid e (:pos fields)) s)))

(defn- turn [world s [eid _]]
  (let [s (if (:idx s) (advanced s eid) s)
        e (get (:cur s) eid)
        tw (overlay/seen (:w s) eid)]
    (cond
      (nil? e) s
      (due? e) (fused s world tw eid e (stepped tw eid e))
      :else (moved s (:tick world) eid (stepped tw eid e)))))

(defn- finished [{:keys [out cur]}]
  (into [] (keep (fn [d]
                   (if (= ::fresh (nth d 0))
                     (when-let [e (get cur (nth d 1))]
                       [:spawn-entity e])
                     d)))
        out))

(defn turns
  "Returns the deltas of every primed TNT in an active chunk, each in
  its turn after the turns ds of the other entities in world. A blast
  reaches the bodies where they are at its turn: those before it in
  the tick list moved, the others not yet. What it does to them, its
  craters and the bodies it spawns, the later turns see."
  [world ds]
  (let [tnts (areas/active-of-types world [:tnt])]
    (when (pos? (count tnts))
      (finished
        (reduce #(turn world %1 %2)
                {:w world :cur (par/keyed (:entities world)) :ds ds
                 :out [] :fresh 0}
                tnts)))))
