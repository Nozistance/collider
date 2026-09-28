(ns collider.game.systems.blocks.dig
  "Breaking blocks."
  (:require [collider.data :as data]
            [collider.game.game-mode :as game-mode]
            [collider.game.block.blockentity :as be]
            [collider.game.block.container :as container]
            [collider.game.entity :as entity]
            [collider.game.state :as state]
            [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.blocks.reach :as reach]
            [collider.game.systems.items :as items]
            [collider.world.block :as block]
            [collider.world.blocks.connect :as connect]
            [collider.world.blocks.fire :as fire]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private upper-types
  (into block/door-types (conj connect/pair-types :pitcher-crop)))

(defn- first-half? [old]
  (let [{:keys [half part]} (block/props-of old)
        t (block/type-of old)]
    (or (and (= :upper half) (contains? upper-types t))
        (and (= :bed t) (= :foot part)))))

(defn- kept-partner [world e pos old]
  (when (and (state/infinite-materials? e) (first-half? old))
    (connect/partner (:chunks world) pos old)))

(defn- jukebox-break-deltas [world pos]
  (let [e (be/at world pos)
        above (mapv + pos [0 1 0])]
    (when (and (= :jukebox (:kind e)) (:record e))
      [[:spawn-entity (items/popped world above (:record e) :jukebox)]
       (out/all (out/level-event
                  out/sound-stop-jukebox-song pos 0))])))

(defn- centre [[x y z]]
  [(+ (double x) 0.5) (+ (double y) 0.5) (+ (double z) 0.5)])

(defn- shulker-drop [world pos e]
  (let [item (block/block-of (edit/block-at world pos))
        vel (entity/pop-velocity [(:tick world) pos :shulker])]
    [:spawn-entity
     (entity/item (centre pos) vel (be/to-stack item e))]))

(defn- shulker-break-deltas [world pos]
  (let [e (be/at world pos)]
    (when (= :shulker-box (:kind e))
      (concat
        (when (container/animation world pos)
          [[:shulker-anim pos nil]])
        (when (some some? (:items e))
          [(shulker-drop world pos e)])))))

(defn- lectern-break-deltas [world pos]
  (for [e (container/dropped-book world pos)] [:spawn-entity e]))

(defn- spilled [world pos i stack]
  (map (fn [part]
         [:spawn-entity (items/popped world pos part [:spill i])])
       (items/split-drop world pos stack [:spill i])))

(defn- spill-deltas [world pos]
  (let [e (be/at world pos)]
    (when (contains? be/spill-kinds (:kind e))
      (mapcat (fn [[i s]] (when s (spilled world pos i s)))
              (map-indexed vector (:items e))))))

(defn- break-shown [eid pos old]
  (if (fire/fire-state? old)
    [(out/all (out/extinguish pos))]
    [(out/except eid (out/break-effect pos old))]))

(defn- entity-deltas [world pos]
  (vec (concat (jukebox-break-deltas world pos)
               (shulker-break-deltas world pos)
               (lectern-break-deltas world pos)
               (spill-deltas world pos))))

(defn- gone-deltas
  [world eid pos old [ppos pst :as kept]]
  (let [gone (cond->> [[pos (block/emptied old)]]
               kept (cons [ppos (block/emptied pst)]))]
    (concat
      (break-shown eid pos old)
      (edit/change-deltas world gone)
      (when kept [(out/except eid (out/break-effect ppos pst))]))))

(defn- break-deltas [world eid pos]
  (let [old (edit/block-at world pos)
        e (get-in world [:entities eid])
        kept (kept-partner world e pos old)]
    (if (pos? old)
      (into (entity-deltas world pos)
            (gone-deltas world eid pos old kept))
      [(edit/own-change world eid pos)])))

(defn- may-break? [e]
  (let [it (get (data/items) (:item (state/hand-stack e :main)))]
    (not (or (false? (:breaks? it))
             (and (state/infinite-materials? e)
                  (false? (:creative-break? it)))))))

(defn- restricted [world eid status pos]
  (when (zero? (long status)) [(edit/own-change world eid pos)]))

(defn dig-deltas [world [eid status pos _face]]
  (let [e (get-in world [:entities eid])
        low? (<= (long (nth pos 1)) (chunk/level-max-y world))]
    (when (and (#{0 2} status) (reach/in-reach? e pos))
      (cond
        (not low?) [(edit/own-change world eid pos)]
        (game-mode/spectator? e) (restricted world eid status pos)
        (may-break? e) (break-deltas world eid pos)
        :else [(edit/own-change world eid pos)]))))
