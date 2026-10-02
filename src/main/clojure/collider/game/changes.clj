(ns collider.game.changes
  "Block changes as deltas, with their updates and effects."
  (:require [collider.data :as data]
            [collider.game.block.blockentity :as be]
            [collider.game.block.spill :as spill]
            [collider.game.block.tnt :as tnt]
            [collider.game.item :as item]
            [collider.game.level :as level]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.campfire :as campfire]
            [collider.world.blocks.geyser :as geyser]
            [collider.world.chunk :as chunk]
            [collider.world.env.attribute :as attribute]
            [collider.world.neighbors :as neighbors]))

(set! *warn-on-reflection* true)

(defn block-at
  "Returns the block state at pos in the level world."
  ^long [world pos]
  (chunk/chunks-get-block (:chunks world) pos))

(defn- drying? [world [_ st]]
  (and (number? st)
       (= :wet-sponge (block/block-of (long st)))
       (attribute/water-evaporates? (:dim world))))

(defn- dried-fx [world pos]
  (let [roll (random/of-key (:tick world) pos :sponge-dries)
        pitch (* (+ 1.0 (* (double roll) 0.2)) 0.7)]
    [(out/all (out/level-event :particles-water-evaporating pos 0))
     (out/all (out/block-sound :wet-sponge/dries pos 1.0 pitch))]))

(defn dried
  "Returns the changes with wet sponges dried where water evaporates.
  The effects of the drying come with them."
  [world changes]
  (let [dry? #(drying? world %)
        sponge (block/state :sponge)]
    [(mapv (fn [[pos :as c]] (if (dry? c) [pos sponge] c)) changes)
     (into [] (comp (filter dry?) (map first)
                    (mapcat #(dried-fx world %)))
           changes)]))

(defn- dropped [world pos [_ old]]
  (when (get-in world [:rules :block-drops] true)
    (let [salt (fn [salt] (random/of-key (:tick world) pos salt))]
      (for [[i stack] (map-indexed vector (block/drops old salt))]
        [:spawn-entity (item/popped world pos stack i)]))))

(def ^:private ^:const anvil-hurt 2.0)

(def ^:private ^:const hurt-max 40)

(defn- hurts
  "Returns the damage per block fallen of a falling block st, as
  AnvilBlock.falling:84 sets it, or hurt as the effect gave it."
  [st hurt]
  (cond hurt hurt
        (= :anvil (block/type-of st)) anvil-hurt))

(defn- falling-block [[x y z :as pos] [_ st hurt]]
  (let [h (hurts st hurt)]
    [[:spawn-entity
      (cond-> {:type :falling-block
               :pos [(+ (long x) 0.5) (double y) (+ (long z) 0.5)]
               :vel [0.0 0.0 0.0] :yaw 0.0 :pitch 0.0
               :on-ground false :block (block/without-water st)
               :start pos :time 0}
        h (assoc :hurt h :hurt-max hurt-max))]]))

(defn- primed [world pos]
  (when (get-in world [:rules :tnt-explodes] true)
    (let [e (tnt/primed pos [(:tick world) pos])]
      [[:spawn-entity e]
       (out/all (out/sound :tnt/primed (:pos e) 1.0 1.0))])))

(def ^:private drip-events
  {:water out/sound-drip-water-into-cauldron
   :lava out/sound-drip-lava-into-cauldron})

(def ^:private eruption-sounds
  {:erupting :block.potent-sulfur.geyser-eruption
   :continuous :block.potent-sulfur.geyser-continuous-eruption})

(defn sulfur-placed-fx
  "Returns the effects of potent sulfur taking state st at pos.
  A geyser that starts is heard and runs its block event."
  [[x y z :as pos] ^long st]
  (when-let [kind (eruption-sounds (geyser/phase st))]
    (let [at [(+ (long x) 0.5) (+ (long y) 0.5) (+ (long z) 0.5)]]
      [(out/all (out/sound kind at 1.0 1.0))
       (out/all (out/block-event pos 0 0))])))

(defn- countdown-reset [world pos]
  (when-let [e (be/at world pos)]
    (when (= :potent-sulfur (:kind e))
      [[:set-block-entity pos (assoc e :countdown -1)]])))

(def ^:private change-effects
  "What each effect a change names does, by its kind."
  {:fizz (fn [_ pos _] [(out/all (out/fizz pos))])
   :break (fn [_ pos [_ old]] [(out/all (out/break-effect pos old))])
   :drop dropped
   :fall (fn [_ pos e] (falling-block pos e))
   :prime (fn [world pos _] (primed world pos))
   :dry (fn [world pos _] (dried-fx world pos))
   :sound (fn [_ pos [_ kind volume pitch]]
            [(out/all (out/block-sound kind pos volume pitch))])
   :drip (fn [_ pos [_ fluid]]
           [(out/all (out/level-event (drip-events fluid) pos 0))])
   :schedule (fn [_ _ [_ at-ids]] [[:schedule-ticks at-ids]])
   :event (fn [_ pos [_ id]] [(out/all (out/level-event id pos 0))])
   :trail (fn [_ pos [_ target color ticks]]
            (let [at (mapv #(+ (double %) 0.5) pos)]
              [(out/all (out/trail at target color ticks))]))
   :geyser-start (fn [_ pos [_ st]] (sulfur-placed-fx pos st))
   :reset-countdown (fn [world pos _] (countdown-reset world pos))})

(defn change-fx
  "Returns the deltas of the effects the changes carry.
  A change [pos st fx] names them in fx. Each is a kind such as
  :fizz, or a kind with its data such as [:drop st]. A change
  [pos st] has none."
  [world changes]
  (for [[pos _ fx] changes
        e fx
        :let [e (if (keyword? e) [e] e)]
        d ((change-effects (first e)) world pos e)]
    d))

(defn block-changes
  "Returns the changes as [pos st], without their effects."
  [changes]
  (mapv (fn [[pos st :as c]] (if (== 2 (count c)) c [pos st]))
        changes))

(defn- heard? [sent [p]]
  (some #(= p %) sent))

(def ^:private ^:const scan-limit 16)

(defn- unheard
  [{:keys [records sent]}]
  (if (and (<= (count sent) scan-limit)
           (every? #(heard? sent %) records))
    []
    (let [sent (set sent)]
      (into [] (comp (map first) (remove sent) (distinct)) records))))

(def ^:private ^:table holds-entity
  (delay (block/state-table :boolean (comp some? be/kind))))

(defn- removed? [[_ old st flags]]
  (and (aget ^booleans @holds-entity (long old))
       (be/removed? old st flags)))

(defn- removal-deltas [world writes]
  (let [gone (fn [[p old]] (spill/removed-deltas world p old))]
    (into [] (comp (filter removed?) (mapcat gone)) writes)))

(defn- with-fx? [rec] (boolean (get rec 2)))

(defn settled-deltas
  "Returns the deltas of s, the result of a run of block updates.
  Given by, it is the author of the changes, or runs [by n] of the
  authors of n changes in a row."
  ([world s] (settled-deltas world s nil))
  ([world s by]
   (let [recs (:records s)
         quiet (not-empty (unheard s))
         d (cond-> [:set-blocks (block-changes recs) (:ticks s)]
             (or quiet by) (conj quiet)
             by (conj by))]
     (cond-> (into [d] (removal-deltas world (:writes s)))
       (some with-fx? recs) (into (change-fx world recs))))))

(defn- joined [a b]
  (let [[_ ca ta _ by] a [_ cb tb] b
        d [:set-blocks (into ca cb) (into ta tb)]]
    (if by (conj d nil by) d)))

(defn- joinable? [d]
  (and (= :set-blocks (nth d 0)) (some? (nth d 2 nil))
       (nil? (nth d 3 nil))))

(defn joined-into
  "Returns out with d added.
  A run of plain block writes of one author joins into one."
  [out d]
  (let [n (count out)
        top (when (pos? n) (nth out (dec n)))]
    (if (and top (joinable? top) (joinable? d)
             (= (nth top 4 nil) (nth d 4 nil)))
      (conj! (pop! out) (joined top d))
      (conj! out d))))

(defn- author-of [runs]
  (if (== 1 (count runs)) (nth (nth runs 0) 0) (not-empty runs)))

(defn- run-deltas [world changes base f]
  (let [[changes fx] (dried world changes)
        ctx (level/level-ctx world base)
        s (f (:chunks world) ctx changes)
        by (author-of (:authors s))]
    [(into (settled-deltas world s by) fx) s]))

(defn- given-ops [changes]
  (mapv (fn [c]
          (if (keyword? (c 0)) c [:set c (neighbors/flags-of c 3)]))
        changes))

(defn- as-given [chunks ctx changes]
  (neighbors/run chunks ctx (given-ops changes)))

(defn change-deltas
  "Returns the deltas for the changes, each set as it is.
  Each runs its updates at once. The fourth element of a change
  holds its flags, 3 when none. A change that starts with a keyword
  is an op of neighbors/run. Base is the tick the changes are
  made on. A player's edit comes between ticks, after tick base."
  ([world changes]
   (change-deltas world changes (dec (long (:tick world)))))
  ([world changes base]
   (first (run-deltas world changes base as-given))))

(defn set-deltas
  "Returns the deltas for the changes the level makes in its tick.
  Each is set as it is and runs its updates at once. Given runs [by
  n], n changes in a row of author by, each record has its author."
  ([world changes]
   (change-deltas world changes (:tick world)))
  ([world changes runs]
   (let [f #(neighbors/run-authored %1 %2 (given-ops %3) runs)]
     (first (run-deltas world changes (:tick world) f)))))

(defn shaped-deltas
  "Returns the deltas for changes placed in the shape they take.
  The shape comes from the neighbours, and the updates run at once. A
  change made between ticks has base one before the tick of world."
  ([world changes] (shaped-deltas world changes (:tick world)))
  ([world changes base]
   (first (run-deltas world changes base neighbors/set-blocks))))

(defn flagged-deltas
  "Returns the deltas that set each change as it is with its flags.
  They update the neighbours of each cell of notified. A change made
  between ticks has base one before the tick of world."
  ([world changes flags]
   (flagged-deltas world changes flags nil (:tick world)))
  ([world changes flags notified base]
   (let [ops #(concat (map (fn [c] [:set c flags]) %)
                      (map (fn [p] [:notify p]) notified))
         run (fn [chunks ctx cs] (neighbors/run chunks ctx (ops cs)))]
     (first (run-deltas world changes base run)))))

(defn command-deltas
  "Returns [deltas n placed] for the changes of /setblock or /fill.
  They are made between ticks with the options of neighbors/commanded.
  n counts the cells they affected, placed the cells they set."
  ([world changes] (command-deltas world changes nil))
  ([world changes opts]
   (let [base (dec (long (:tick world)))
         f #(neighbors/commanded %1 %2 %3 opts)
         [ds s] (run-deltas world changes base f)]
     [ds (:count s) (count (:placed s))])))

(defn ops-deltas
  "Returns [deltas n] for the ops of neighbors/run-counted, made
  between ticks. n counts the puts that changed their cell."
  [world ops]
  (let [base (dec (long (:tick world)))
        [ds s] (run-deltas world ops base neighbors/run-counted)]
    [ds (:count s)]))

(defn- ghast-placed-fx [pos ^long state]
  (let [kind (if (block/waterlogged? state)
               :block.dried-ghast.place-in-water
               :block.dried-ghast.place)]
    [(out/all (out/block-sound kind pos 1.0 1.0))]))

(defn- placed-by-fx [pos ^long state]
  (case (block/type-of state)
    :dried-ghast (ghast-placed-fx pos state)
    nil))

(defn- place-sound [world eid pos state]
  (let [item (:item (player/use-stack world eid))
        {:keys [kind volume pitch]}
        (data/placed-sound (block/block-of state) item)]
    (out/except eid (out/block-sound kind pos volume pitch))))

(defn placed-deltas
  "Returns the deltas of a player placing blocks.
  The place sound goes to everyone else."
  ([world eid pos state] (placed-deltas world eid [[pos state]]))
  ([world eid changes]
   (let [base (dec (long (:tick world)))
         deltas (shaped-deltas world changes base)
         [[pos state]] (first (dried world changes))]
     (-> deltas
         (into (placed-by-fx pos state))
         (conj (place-sound world eid pos state))))))

(defn be-changed
  "Returns the deltas that set the block entity at pos and show it."
  [pos e]
  [[:set-block-entity pos e] (out/all (out/block-entity pos))])

(defn- unlit [^long st]
  (block/state (block/block-of st)
               (assoc (block/props-of st) :lit :false)))

(defn candle-out-deltas
  "Returns the deltas that put out a lit candle at pos.
  Returns nil when it is already unlit."
  [world pos]
  (let [cur (block-at world pos)]
    (when (= :true (:lit (block/props-of cur)))
      (let [deltas (change-deltas world [[pos (unlit cur)]])
            snuff (out/block-sound :candle/extinguish pos 1.0 1.0)]
        (concat deltas [(out/all snuff)])))))

(defn campfire-out-deltas
  "Returns the deltas that dowse a campfire at pos.
  The deltas carry its level event."
  [world pos]
  (when-let [st (campfire/dowsed (block-at world pos))]
    (concat (change-deltas world [[pos st]])
            [(out/all (out/level-event
                        out/sound-extinguish-fire pos))])))

(defn dowse-deltas
  "Returns the deltas of a candle or a campfire dowsed by water."
  [world pos]
  (case (block/type-of (block-at world pos))
    (:candle :candle-cake) (candle-out-deltas world pos)
    :campfire (campfire-out-deltas world pos)
    nil))
