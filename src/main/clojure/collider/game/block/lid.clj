(ns collider.game.block.lid
  "Container lids: who holds them open, their sounds and the shulker
  box animation."
  (:require [collider.game.changes :as changes]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.chest :as chest]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def ^:private axis-index {:x 0 :y 1 :z 2})

(def ^:private positive? #{:up :south :east})

(defn- half-free? [chunks pos facing]
  (let [st (chest/state-at chunks (mapv + pos (dir/offset facing)))
        ax (long (axis-index (dir/axis facing)))
        far? (contains? positive? facing)]
    (not-any? (fn [box]
                (let [lo (double (nth box ax))
                      hi (double (nth box (+ ax 3)))]
                  (if far? (< lo 8.0) (> hi 8.0))))
              (block/collision-boxes st))))

(defn animation
  "Returns the lid animation of the shulker box at pos, or nil."
  [world pos]
  (get (:shulker-anim world) pos))

(defn can-open?
  "Returns true when the shulker box st at pos has room to open its
  lid or is already moving it."
  [world pos ^long st]
  (or (some? (animation world pos))
      (half-free? (:chunks world) pos (:facing (block/props-of st)))))

(defn positions
  "Returns the positions of the containers whose lids menu m holds
  open."
  [m]
  (case (:kind m)
    (:lectern :bench) []
    :ender [(:pos m)]
    (:cells m)))

(defn- covers? [m pos]
  (boolean (some #(= pos %) (positions m))))

(defn- viewers ^long [world pos]
  (let [sees? (fn [[_ e]]
                (and (:menu e) (covers? (:menu e) pos)
                     (not (game-mode/spectator? e))))]
    (count (filter sees? (:entities world)))))

(defn- pitch [world pos salt]
  (random/hinge-pitch [(:tick world) pos salt]))

(def ^:private copper-hinge
  {:weathered-copper-chest       :block.copper-chest-weathered
   :waxed-weathered-copper-chest :block.copper-chest-weathered
   :oxidized-copper-chest        :block.copper-chest-oxidized
   :waxed-oxidized-copper-chest  :block.copper-chest-oxidized})

(defn- hinge-base [^long st]
  (if (contains? chest/copper-types (block/type-of st))
    (get copper-hinge (block/block-of st) :block.copper-chest)
    :block.chest))

(defn- chest-sound-name [^long st open?]
  (let [base (hinge-base st)]
    (keyword (str (name base) (if open? ".open" ".close")))))

(defn- face-centre [[x y z] facing]
  (let [[dx dy dz] (dir/offset facing)]
    [(+ (double x) 0.5 (* 0.5 (double dx)))
     (+ (double y) 0.5 (* 0.5 (double dy)))
     (+ (double z) 0.5 (* 0.5 (double dz)))]))

(defn- lid-sound [world pos kind p]
  [(out/all (out/sound kind p 0.5 (pitch world pos :lid)))])

(defn- centre-sound [world pos kind]
  [(out/all (out/block-sound kind pos 0.5 (pitch world pos :lid)))])

(defn- chest-sound [world pos ^long st open?]
  (let [kind (chest-sound-name st open?)
        side (:type (block/props-of st))]
    (case side
      :left nil
      :right (lid-sound world pos kind
               (face-centre pos (chest/connected-direction st)))
      (centre-sound world pos kind))))

(defn- barrel-sound [world pos ^long st open?]
  (let [facing (:facing (block/props-of st))
        kind (if open? :block.barrel.open :block.barrel.close)]
    (lid-sound world pos kind (face-centre pos facing))))

(defn- shulker-sound [world pos open?]
  (let [kind (if open?
               :block.shulker-box.open
               :block.shulker-box.close)]
    (centre-sound world pos kind)))

(defn- ender-sound [world pos open?]
  (let [kind (if open?
               :block.ender-chest.open
               :block.ender-chest.close)]
    (centre-sound world pos kind)))

(def ^:private ^:const recheck-delay 5)

(def ^:private open-step (float 0.1))

(defn- shulker-step [status up down]
  (case status
    :opening (if (>= (float up) (float 1.0))
               {:status :opened :progress (float 1.0)}
               {:status :opening :progress up})
    :closing (when (> (float down) (float 0.0))
               {:status :closing :progress down})))

(defn- animated [world [pos {:keys [status progress]}]]
  (let [p (float progress)
        up (float (+ p open-step))
        down (float (- p open-step))
        st (chest/state-at (:chunks world) pos)]
    (if (not= :shulker-box (block/type-of st))
      [[:shulker-anim pos nil]]
      (case status
        (:opening :closing)
        [[:shulker-anim pos (shulker-step status up down)]]
        nil))))

(defn animate-deltas
  "Returns the deltas of one tick of every shulker box lid that
  moves."
  [world]
  (mapcat #(animated world %) (:shulker-anim world)))

(defn- trigger-deltas [pos ^long after]
  (cond
    (zero? after) [[:shulker-anim pos {:status :closing}]]
    (= 1 after) [[:shulker-anim pos {:status :opening}]]
    :else nil))

(defn- opener-count ^long [world pos]
  (long (get-in world [:openers pos] 0)))

(def ^:private counted
  (conj chest/types :barrel :ender-chest))

(defn- barrel-open-state [^long st open?]
  (let [open (if open? :true :false)
        props (assoc (block/props-of st) :open open)]
    (block/state (block/block-of st) props)))

(defn- edge-sound [world pos st t open?]
  (cond
    (contains? chest/types t) (chest-sound world pos st open?)
    (= :barrel t) (barrel-sound world pos st open?)
    (= :ender-chest t) (ender-sound world pos open?)))

(defn- edge-deltas [world pos st t open?]
  (concat
    (edge-sound world pos st t open?)
    (when (= :barrel t)
      (let [st' (barrel-open-state st open?)]
        (changes/set-deltas world [[pos st']])))))

(defn- lid-event [pos ^long n]
  (out/all (out/block-event pos 1 n)))

(defn- changed-deltas [pos t ^long n]
  (when (not= :barrel t) [(lid-event pos n)]))

(defn- recheck-at [pos ^long base]
  (let [id (chunk/block-pos->id pos)]
    [:schedule-ticks {(+ base recheck-delay) [id]}]))

(defn- counter-deltas [world pos st t step]
  (let [prev (opener-count world pos)
        n (+ prev (long step))
        opened? (and (pos? step) (zero? prev))]
    (concat
      [[:openers pos step]]
      (cond
        opened? (edge-deltas world pos st t true)
        (and (neg? step) (zero? n))
        (edge-deltas world pos st t false))
      (when opened? [(recheck-at pos (dec (long (:tick world))))])
      (changed-deltas pos t n))))

(defn- shulker-deltas [world pos ^long step]
  (let [prev (opener-count world pos)
        n (+ (if (pos? step) (max prev 0) prev) step)
        heard? (if (pos? step) (= 1 n) (<= n 0))]
    (concat
      [[:openers pos (- n prev)]]
      (when heard? (shulker-sound world pos (pos? step)))
      [(lid-event pos n)]
      (trigger-deltas pos n))))

(defn opener-deltas
  "Returns the deltas of a player who opens the container at pos
  with step 1 or closes it with step -1. Spectators never call it."
  [world pos ^long step]
  (let [st (chest/state-at (:chunks world) pos)
        t (block/type-of st)]
    (cond
      (= :shulker-box t) (shulker-deltas world pos step)
      (contains? counted t) (counter-deltas world pos st t step))))

(defn- edge [^long prev ^long n]
  (when (not= prev n)
    (cond (zero? prev) true
          (zero? n) false)))

(defn- recount-deltas [world pos st t prev n open?]
  (concat (when (not= prev n) [[:openers pos (- n prev)]])
          (when (some? open?) (edge-sound world pos st t open?))
          (changed-deltas pos t n)
          (when (pos? n) [(recheck-at pos (:tick world))])))

(defn recheck
  "Returns the block changes and deltas that recount who has the
  container at pos open. Returns nil for a block that does not count
  them."
  [world pos st]
  (let [t (block/type-of st)]
    (when (contains? counted t)
      (let [prev (opener-count world pos)
            n (viewers world pos)
            open? (edge prev n)]
        {:reach 0
         :changes (when (and (some? open?) (= :barrel t))
                    [[pos (barrel-open-state st open?)]])
         :deltas (recount-deltas world pos st t prev n open?)}))))
