(ns collider.game.entity.hurt
  "Damage, fire, the void and death of entities, and the deltas
  that show them."
  (:require [collider.data :as data]
            [collider.game.attribute :as attribute]
            [collider.game.changes :as changes]
            [collider.game.delta :as delta]
            [collider.game.entity :as entity]
            [collider.game.experience :as xp]
            [collider.game.input :as input]
            [collider.game.item :as item]
            [collider.game.level :as level]
            [collider.game.loot :as loot]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.shell :as shell]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.game.stack :as stack]
            [collider.game.scoreboard :as scoreboard]
            [collider.game.team :as team]
            [collider.num :as num]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.cauldron :as cauldron]
            [collider.world.blocks.inside :as inside]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.motion :as motion]
            [collider.world.chunk :as chunk]
            [collider.world.env.difficulty :as difficulty]
            [collider.world.env.signal :as signal]
            [collider.world.env.weather :as weather]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ^:private ^:const void-damage 4.0)

(def ^:private ^:const panic-ticks 40)

(def ^:private ^:const voice-pitch-spread 0.2)

(def ^:private ^:const baby-voice-pitch 1.5)

(def ^:private ^:const adult-voice-pitch 1.0)

(def ^:private ^:const fire-damage-period 20)

(def ^:private ^:const ticks-per-second 20)

(def ^:private effect-sounds
  {"drowning" :player/hurt-drown "burning" :player/hurt-on-fire
   "poking"   :player/hurt-sweet-berry-bush
   "freezing" :player/hurt-freeze})

(def ^:private ^:table player-hurts
  (delay (update-vals (update-keys (data/pack "damage_type") data/kebab)
                      #(effect-sounds (get % "effects") :player/hurt))))

(defn- hurt-sound [e src]
  (if (entity/player? e)
    (if (pos? (double (:health e)))
      (@player-hurts (:type src) :player/hurt)
      :player/death)
    (if (pos? (double (:health e)))
      (mobs/sound-of e :hurt)
      (mobs/sound-of e :death))))

(defn- sound-pitch ^double [world eid e]
  (let [t (long (:tick world))
        base (if (:baby-until e) baby-voice-pitch adult-voice-pitch)
        r (- (random/of-longs t eid (hash :hurt1))
             (random/of-longs t eid (hash :hurt2)))]
    (+ base (* voice-pitch-spread r))))

(def ^:private ^:table bypassing
  (delay
    (set (data/tag-values "damage_type" "bypasses_invulnerability"))))

(defn- tagged [[tag rule]]
  (map (fn [t] [t rule]) (data/tag-values "damage_type" tag)))

(def ^:private ^:table rule-of
  (delay
    (into {}
          (mapcat tagged)
          [["is_freezing" :freeze-damage]
           ["is_fire" :fire-damage]
           ["is_fall" :fall-damage]
           ["is_drowning" :drowning-damage]])))

(def ^:private ^:table scaling
  (delay (into {}
               (map (fn [[id m]] [(data/kebab id) (get m "scaling")]))
               (data/pack "damage_type"))))

(defn- cause-of [world src]
  (when-let [c (:cause src)] (get-in world [:entities c])))

(defn- spared?
  "Returns true when player e takes no damage from src.
  A game rule, the pvp rule or the mode of e can spare it."
  [world e src]
  (let [t (:type src) rule (get @rule-of t) a (cause-of world src)]
    (or (and rule (not (get-in world [:rules rule] true)))
        (and (entity/player? a)
             (or (not (get-in world [:rules :pvp] true))
                 (and (entity/player? e)
                      (team/spares? (:teams world) e a))))
        (and (game-mode/invulnerable? e)
             (not (contains? @bypassing t))))))

(defn- scales?
  [world src]
  (case (get @scaling (:type src))
    "always" true
    "when_caused_by_living_non_player"
    (mobs/mob-type? (:type (cause-of world src)))
    false))

(defn- by-difficulty
  ^double [world ^double n]
  (let [n (float n)]
    (case (long (difficulty/id world))
      0 0.0
      1 (double (min (+ (/ n (float 2.0)) (float 1.0)) n))
      3 (double (/ (* n (float 3.0)) (float 2.0)))
      (double n))))

(defn taken
  "Returns the damage entity e takes of amount from src, or nil.
  A player takes no damage in a mode that keeps it from harm or when a
  rule spares it. The difficulty scales the damage to a player. Any
  other entity takes amount."
  [world e amount src]
  (cond
    (and (contains? (:effects e) :fire-resistance)
         (= :fire-damage (get @rule-of (:type src))))
    nil
    (entity/player? e)
    (when-not (spared? world e src)
      (let [n (double amount)
            n (if (scales? world src) (by-difficulty world n) n)]
        (when-not (zero? n) n)))
    :else amount))

(defn hurts?
  "Returns true when amount from src hurts living entity e. Inside
  its hurt window only more than its last hurt does."
  [world e amount src]
  (let [n (taken world e amount src)]
    (boolean
      (and n (pos? (double (:health e 0.0)))
           (or (<= (long (or (:hurt-resist e) 0)) 10)
               (> (num/f32 n)
                  (num/f32 (or (:last-damage e) 0.0))))))))

(defn damage-deltas
  "Returns the deltas of entity eid taking amount from src, none when
  it takes nothing."
  [world eid e amount src]
  (when-let [n (taken world e amount src)]
    [[:damage eid n src]]))

(def ^:private ^:const fire-seconds 8)

(def ^:private ^:const lava-seconds 15)

(def ^:private ^:const lava-damage 4.0)

(defn- box-of [e] (entity/box e))

(defn- burns? [st]
  (or (block/lava? st) (block/fire? st) (= :soul-fire (block/type-of st))))

(defn- in-lava?
  "Returns true when entity e is in lava. It never is in its first
  tick."
  [world e]
  (and (not= 1 (entity/tick-count world e))
       (let [[half height] (box-of e)]
         (pos? (liquid/fluid-height
                 (:chunks world) (:pos e) half height :lava)))))

(defn- burning-flag [eid e ^long fire]
  (let [lit? (pos? fire)]
    (when (not= lit? (boolean (:burning? e)))
      [[:merge-entity eid {:burning? lit?}]])))

(def ^:private on-fire {:type :on-fire})

(def ^:private in-fire {:type :in-fire})

(def ^:private in-lava {:type :lava})

(def ^:private out-of-world {:type :out-of-world})

(defn- burn-tick-deltas [world eid e fire]
  (when (pos? fire)
    (let [due? (and (zero? (rem fire fire-damage-period))
                    (not (in-lava? world e)))]
      (cond-> [[:merge-entity eid {:fire (dec fire)}]]
              due? (conj [:damage eid 1.0 on-fire])))))

(defn- fizz-cell [e]
  (mapv (fn [c] (long (Math/floor (double c))))
        [(v/x (:pos e)) (v/y (:pos e)) (v/z (:pos e))]))

(defn- douse-deltas [eid e fire wet?]
  (when (and wet? (pos? (long fire)))
    (cons [:merge-entity eid {:fire 0 :burning? false}]
          [(out/all (out/fizz (fizz-cell e)))])))

(def ^:private ^:const fire-rest -20)

(def ^:private ^:const inside-margin 1.0E-5)

(defn- capped ^long [^long f] (min f 1))

(defn- cell-span [^double lo ^double hi]
  (range (long (Math/floor (+ lo inside-margin)))
         (inc (long (Math/floor (- hi inside-margin))))))

(defn- inside-cells [e]
  (let [p (:pos e)
        [half height] (box-of e)
        h (double half)
        x (v/x p) y (v/y p) z (v/z p)]
    (for [cx (cell-span (- x h) (+ x h))
          cy (cell-span y (+ y (double height)))
          cz (cell-span (- z h) (+ z h))]
      [cx cy cz])))

(defn- cell-state ^long [chunks [x y z]]
  (chunk/block-state chunks x y z))

(defn- lo-cell ^long [^double a]
  (long (Math/floor (+ a inside-margin))))

(defn- hi-cell ^long [^double a]
  (long (Math/floor (- a inside-margin))))

(def ^:private ^:table occupied
  (delay (let [a (boolean-array (data/block-state-count) true)]
           (aset a 0 false)
           a)))

(def ^:private ^:table snowy
  (delay (let [n (data/block-state-count) a (boolean-array n)]
           (dotimes [st n]
             (when (= :powder-snow (block/type-of st)) (aset a st true)))
           a)))

(defn- some-inside? [chunks e marks]
  (let [p (:pos e)
        [half height] (box-of e)
        h (double half) x (v/x p) y (v/y p) z (v/z p)
        top (+ y (double height))]
    (phys/some-cell?
      chunks marks
      (lo-cell (- x h)) (lo-cell y) (lo-cell (- z h))
      (inc (hi-cell (+ x h))) (inc (hi-cell top))
      (inc (hi-cell (+ z h))))))

(defn- all-air? [chunks e]
  (not (some-inside? chunks e @occupied)))

(defn- snow-cells
  "Returns the cells of powder snow entity e is in."
  [world e]
  (if (all-air? (:chunks world) e)
    []
    (let [chunks (:chunks world)]
      (filterv #(= :powder-snow (block/type-of (cell-state chunks %)))
               (inside-cells e)))))

(defn rained-on?
  "Returns true when rain falls on the feet or the head of e."
  [world e]
  (let [p (:pos e)
        [_ height] (box-of e)
        x (long (Math/floor (v/x p)))
        z (long (Math/floor (v/z p)))
        top (long (Math/floor (+ (v/y p) (double height))))
        rain? #(weather/raining-at? world (:chunks world) %)]
    (or (rain? [x (long (Math/floor (v/y p))) z]) (rain? [x top z]))))

(defn- capper
  "Returns the cap on the fire of player e.
  The cap is one tick when the mode of e keeps it from harm."
  [e]
  (if (game-mode/invulnerable? e) capped identity))

(defn- added-ticks
  "Returns the one or two ticks that fire adds to player eid."
  ^long [world eid]
  (let [r (random/of-longs (:tick world) eid (hash :fire-ticks))]
    (inc (long (* 2.0 r)))))

(defn- fire-touched ^long [cap ^long f ^long added]
  (let [f (if (neg? f) (inc f) (long (cap (+ f added))))]
    (if (neg? f)
      f
      (long (cap (max f (* ticks-per-second fire-seconds)))))))

(defn- fire-cell-damage [st]
  (case (block/type-of st) :fire 1.0 :soul-fire 2.0 nil))

(def ^:private ^:table burners
  (delay (block/state-table
           :boolean #(or (some? (fire-cell-damage %))
                         (some? (block/liquid-class %))
                         (cauldron/layered? %) (cauldron/lava? %)))))

(defn- fire-lit ^long [^long r]
  (let [r (if (neg? r) (inc r) r)]
    (if (neg? r) r (max r (* ticks-per-second fire-seconds)))))

(defn- stepped-fire
  "Returns the fn that takes fire r of a body, its hurts, the changes
  of the cauldrons it lowers and whether it thawed on to after the
  blocks met of one step of its move, in the order a step applies
  them. Fn lit lights a fire r and cap caps one. Water puts it out
  when water? is true, a cauldron of water or snow when pots? is,
  lowered first when r still burns."
  [lit cap water? pots?]
  (fn [[r hurts lows thawed?] met]
    (let [fires (keep #(fire-cell-damage (nth % 3)) met)
          lavas (filter #(or (= :lava (nth % 5)) (cauldron/lava? (nth % 3)))
                        met)
          pots (when pots? (filter #(cauldron/layered? (nth % 3)) met))
          r (long r)
          r (if (seq fires) (long (lit r)) r)
          hurts (into hurts (map (fn [d] [d in-fire])) fires)
          r (if (seq lavas)
              (long (cap (max r (* ticks-per-second lava-seconds))))
              r)
          hurts (into hurts (map (fn [_] [lava-damage in-lava])) lavas)
          lows (if (pos? r)
                 (into lows (map (fn [[x y z st]]
                                   [[x y z] (cauldron/doused st)]))
                       pots)
                 lows)
          r (if (or (seq pots)
                    (and water? (some #(= :water (nth % 5)) met)))
              (min 0 r)
              r)]
      [r hurts lows (boolean (or thawed? (seq fires) (seq lavas)))])))

(defn- lowered-deltas
  "Returns the deltas of the cauldrons body eid, e, lowered."
  [world eid e lows]
  (when (seq lows)
    (delta/authored (changes/change-deltas world lows)
                    (delta/entity-author eid e))))

(defn- hurt-deltas
  "Returns the deltas of hurts [amount source] of entity eid, e."
  [world eid e hurts]
  (mapcat (fn [[d src]] (damage-deltas world eid e d src)) hurts))

(defn- met-by
  "Returns the blocks body e met on moves this tick, by step, none when
  it is dead."
  [world e moves]
  (let [[half height] (box-of e)]
    (when (pos? (double (:health e)))
      (partition-by #(nth % 4)
                    (inside/met (:chunks world) moves half height
                                @burners)))))

(defn met-fire
  "Returns the fire of body e, r0, after the fire, lava and water it
  met on moves this tick, step by step, and the rain on it. A body
  that does not burn and was not lit keeps no fire."
  ^long [world e r0 moves]
  (let [[half height] (box-of e)
        met (inside/met (:chunks world) moves half height @burners)
        steps (partition-by #(nth % 4) met)
        r (long (first (reduce (stepped-fire fire-lit identity true true)
                               [r0 [] [] false] steps)))
        r (if (rained-on? world e) (min 0 r) r)]
    (if (and (<= r 0) (<= r (long r0))) 0 r)))

(defn- player-moves
  "Returns the moves the packets of player eid, e, made this tick, or
  its stay where it is when they made none."
  [world eid e]
  (or (seq (input/moves-of world eid)) [[(:pos e) (:pos e) nil]]))

(defn- player-fire [world eid e snow groups]
  (let [cap (capper e)
        added (added-ticks world eid)
        lit #(fire-touched cap % added)
        f0 (long (or (:fire e) 0))
        f1 (if (pos? f0) (long (cap (dec f0))) f0)
        [f hurts lows thawed?] (reduce (stepped-fire lit cap true true)
                                       [f1 [] [] false] groups)
        lit (long (first (reduce (stepped-fire lit cap false false)
                                 [f1 [] [] false] groups)))
        wet? (or (seq snow) (rained-on? world e))
        f (if wet? (min 0 (long f)) (long f))]
    [f0 f1 lit (if (and (<= f 0) (<= f f1)) fire-rest f) hurts lows
     thawed?]))

(defn- melt-effect [chunks c]
  (out/all (out/break-effect c (cell-state chunks c))))

(defn- melt-deltas [world cells]
  (when (seq cells)
    (concat (changes/set-deltas world (mapv (fn [c] [c 0]) cells))
            (map #(melt-effect (:chunks world) %) cells))))

(defn put-out-sound
  "Returns the hiss of entity eid whose fire went out."
  [world eid e]
  (let [t (long (:tick world))
        r (- (random/of-longs t eid (hash :put-out1))
             (random/of-longs t eid (hash :put-out2)))]
    (out/all (out/sound :generic/extinguish-fire (:pos e) 0.7
                        (+ 1.6 (* 0.4 r))
                        (if (entity/player? e) :players :neutral)))))

(def ^:private ^:const freeze-ticks 140)

(def ^:private ^:const freeze-period 40)

(def ^:private freezing {:type :freeze})

(def ^:private ^:table immune-types
  (delay (set (data/tag-values "entity_type" "freeze_immune_entity_types"))))

(def ^:private ^:table immune-wear
  (delay (set (data/tag-values "item" "freeze_immune_wearables"))))

(defn- can-freeze? [e]
  (and (not (game-mode/spectator? e))
       (not (contains? @immune-types (:type e)))
       (not-any? #(contains? @immune-wear (:item %))
                 (vals (dissoc (player/equipment e) :mainhand :offhand)))))

(defn- frost-of
  "Returns how much frost f takes off the speed of entity e, nil when
  none. Over air it takes nothing."
  [world e ^long f]
  (when (and (pos? f)
             (not (zero? (motion/below-state (:chunks world) (:pos e)
                                             (:support e) 0.2))))
    (num/fmul (num/f32 -0.05)
              (num/f32 (/ (double (min f freeze-ticks)) freeze-ticks)))))

(defn- freeze-deltas
  "Returns the deltas of the frost of entity eid after a tick in or
  out of powder snow, from none when thawed? is true."
  [world eid e snow? thawed?]
  (let [f (long (or (:ticks-frozen e) 0))
        f0 (if thawed? 0 f)
        can? (can-freeze? e)
        f' (if (and snow? can?)
             (min freeze-ticks (inc f0))
             (max 0 (- f0 2)))
        lived (entity/tick-count world e)
        frost (frost-of world e f')
        changes (cond-> {}
                  (not= f f') (assoc :ticks-frozen f')
                  (not= frost (:frost e)) (assoc :frost frost)
                  (and (not= frost (:frost e)) (entity/player? e))
                  (assoc :dirty-attributes
                         (conj (or (:dirty-attributes e) #{})
                               :movement-speed)))]
    (concat (when (seq changes) [[:merge-entity eid changes]])
            (when (and can? (>= f' freeze-ticks)
                       (zero? (rem lived freeze-period)))
              (damage-deltas world eid e 1.0 freezing)))))

(defn- player-fire-deltas [world eid e]
  (let [snow (snow-cells world e)
        groups (met-by world e (player-moves world eid e))
        [f0 f1 lit f hurts lows thawed?]
        (player-fire world eid e snow groups)]
    (concat (hurt-deltas world eid e hurts)
            (lowered-deltas world eid e lows)
            (burning-flag eid e f1)
            (when (not= f f0) [[:merge-entity eid {:fire f}]])
            (when (pos? (long lit))
              (delta/authored
                (melt-deltas world snow)
                (delta/entity-author eid e)))
            (when (and (pos? (long f1)) (<= (long f) 0))
              [(put-out-sound world eid e)])
            (freeze-deltas world eid e (boolean (seq snow)) thawed?))))

(defn- met-fire-deltas
  "Returns the deltas of the fire, lava and water mob eid, e, met on
  its moves this tick, step by step, and whether it thawed. With no
  moves it stays where it is."
  [world eid e moves]
  (let [r0 (long (or (:fire e) 0))
        moves (or (seq moves) [[(:pos e) (:pos e) nil]])
        [r hurts lows thawed?]
        (reduce (stepped-fire fire-lit identity true true) [r0 [] [] false]
                (met-by world e moves))
        r (if (rained-on? world e) (min 0 (long r)) (long r))
        r (if (and (<= r 0) (<= r r0)) 0 r)]
    [(concat (burning-flag eid e r0)
             (hurt-deltas world eid e hurts)
             (lowered-deltas world eid e lows)
             (when (not= r r0) [[:merge-entity eid {:fire r}]])
             (when (and (pos? r0) (<= r 0)) [(put-out-sound world eid e)]))
     thawed?]))

(defn fire-deltas
  "Returns the deltas of the fire, lava and frost entity eid touches.
  A mob meets them on its moves this tick."
  ([world eid e] (fire-deltas world eid e nil))
  ([world eid e moves]
   (if (entity/player? e)
     (player-fire-deltas world eid e)
     (let [[ds thawed?] (met-fire-deltas world eid e moves)]
       (concat ds
               (freeze-deltas world eid e
                              (some-inside? (:chunks world) e @snowy)
                              thawed?))))))

(def ^:private ^:const burn-volume 0.4)

(def ^:private ^:const burn-pitch 2.0)

(def ^:private ^:const burn-pitch-spread 0.4)

(def ^:private ^:const burn-sound-period 10)

(defn- fire-proof-item? [e]
  (= "is_fire" (data/resists (:item (:stack e)))))

(defn- item-wet? [world e]
  (let [[half height] (box-of e)]
    (pos? (liquid/fluid-height
            (:chunks world) (:pos e) half height :water))))

(defn- burnt-down
  "Returns the fire left once the base tick burns it down, which the
  burning flag shows. Water puts it out."
  ^long [^long fire wet?]
  (cond (not (pos? fire)) fire
        wet? 0
        :else (dec fire)))

(defn- burn-sound [world eid e ^long i]
  (let [r (random/of-key (:tick world) eid :burn i)
        pitch (+ burn-pitch (* burn-pitch-spread r))]
    (out/all (out/sound :generic/burn (:pos e) burn-volume pitch))))

(defn- heard?
  "Returns true when a lava hurt of item e that leaves health plays
  the burn sound."
  [e ^double health]
  (or (<= health 0.0)
      (= :experience-orb (:type e))
      (zero? (rem (inc (long (or (:age e) 0))) burn-sound-period))))

(defn- hurt-deltas-of
  "Returns the deltas of hurts [amount source] of item or orb eid, e,
  in order, up to the one that takes the last of its health."
  [world eid e hurts]
  (let [ev (when (= :item (:type e))
             (signal/game-event :entity-damage (:pos e) nil))]
    (loop [h (double (:health e)) i 0 hs hurts ds []]
      (if-let [[d src] (when (pos? h) (first hs))]
        (let [h (- h (double d))]
          (recur h (inc i) (rest hs)
                 (cond-> (into (conj ds [:damage eid d src]) ev)
                   (and (identical? src in-lava) (heard? e h))
                   (conj (burn-sound world eid e i)))))
        [ds (not (pos? h))]))))

(defn- held-stacks [stack]
  (let [cs (:components stack)]
    (->> (concat (:container cs) (:bundle-contents cs))
         (map stack/of-template)
         (filter #(pos? (long (:count % 0)))))))

(defn- spill-deltas
  "Returns the stacks a destroyed item entity e lets out of the shulker
  box or bundle it holds, each where e was."
  [world eid e]
  (map-indexed
    (fn [i s]
      (let [r #(random/of-key [(:tick world) eid :spill i %])]
        [:spawn-entity
         (entity/item (:pos e)
                      [(- (* 0.2 (r :x)) 0.1) 0.2 (- (* 0.2 (r :z)) 0.1)]
                      s)]))
    (held-stacks (:stack e))))

(defn- gone-deltas [world eid e]
  (cons [:remove-entity eid] (spill-deltas world eid e)))

(def ^:private ^:table cactus-cells
  (delay (block/state-table :boolean #(= :cactus (block/type-of %)))))

(def ^:private ^:table loose-harm
  (delay (block/state-table
           :boolean
           #(or (burns? %) (cauldron/lava? %)
                (= :cactus (block/type-of %))))))

(defn stirred?
  "Returns true when item or orb e lies in fire, lava or a cactus."
  [world e]
  (boolean (some-inside? (:chunks world) e @loose-harm)))

(def ^:private ^:table pots
  (delay (block/state-table :boolean cauldron/layered?)))

(defn by-pot?
  "Returns true when item or orb e lies in a cauldron of water or
  snow."
  [world e]
  (boolean (some-inside? (:chunks world) e @pots)))

(def ^:private cactus {:type :cactus})

(defn- pricks
  "Returns a hurt for each cactus cell item e lies in."
  [world e]
  (let [chunks (:chunks world)]
    (when (some-inside? chunks e @cactus-cells)
      (for [c (inside-cells e)
            :when (aget ^booleans @cactus-cells (cell-state chunks c))]
        [1.0 cactus]))))

(defn- item-hurts
  "Returns the fire of item e once the tick burns it down and the
  blocks met light it, its hurts [amount source] in the order it
  takes them and the changes of the cauldrons it lowers."
  [world e wet? met]
  (let [fire (long (or (:fire e) 0))
        due? (and (pos? fire) (not wet?)
                  (zero? (rem fire fire-damage-period))
                  (not (in-lava? world e)))
        [r hurts lows] ((stepped-fire fire-lit identity false true)
                        [(burnt-down fire wet?) [] [] false] met)]
    [r (concat (when due? [[1.0 on-fire]]) (pricks world e) hurts) lows]))

(defn- item-fire
  "Returns the deltas of the fire of item e, eid, and its hurts."
  [world eid e]
  (let [fire (long (or (:fire e) 0))
        [half height] (box-of e)
        p (:pos e)
        met (inside/met (:chunks world) [[p p nil]] half height
                        @burners)
        wet? (and (or (pos? fire) (boolean (seq met)))
                  (item-wet? world e))
        [r hurts lows] (item-hurts world e wet? met)]
    [(concat (burning-flag eid e (burnt-down fire wet?))
             (lowered-deltas world eid e lows)
             (when (and (not wet?) (not= r fire))
               [[:merge-entity eid {:fire r}]])
             (when (and (pos? (burnt-down fire wet?)) (<= (long r) 0))
               [(put-out-sound world eid e)])
             (douse-deltas eid e fire wet?))
     hurts]))

(defn item-deltas
  "Returns the deltas of the fire, lava and cactus item or orb eid
  lies in. It goes once its hurts of the tick take all its health."
  [world eid e]
  (let [[fire-ds hurts] (if (fire-proof-item? e)
                          [(when (pos? (long (or (:fire e) 0)))
                             [[:merge-entity eid {:fire 0}]])
                           (pricks world e)]
                          (item-fire world eid e))
        [ds dead?] (hurt-deltas-of world eid e hurts)]
    (concat fire-ds ds (when dead? (gone-deltas world eid e)))))

(def ^:private ^:const safe-fall 3.0)

(defn- ground-state [world pos]
  (chunk/at
    (:chunks world)
    [(long (Math/floor (v/x pos)))
     (long (Math/floor (- (v/y pos) 0.2)))
     (long (Math/floor (v/z pos)))]))

(defn- landing-particles [world e ^double fall]
  (let [power (Math/floor (+ (- fall safe-fall) 1.0e-6))
        pos (:pos e)
        st (ground-state world pos)]
    (when (and (pos? power) (not (block/air? st)))
      (let [scale (min (+ 0.2 (/ power 15.0)) 2.5)
            at [(v/x pos) (v/y pos) (v/z pos)]
            n (long (* 150.0 scale))]
        [(out/all (out/particles :block st at n 0.15))]))))

(defn landing-deltas
  [world eid e]
  (when-let [fall (:landed e)]
    (concat [[:merge-entity eid {:landed nil}]]
            (landing-particles world e (double fall)))))

(defn- loading? [world e]
  (and (entity/player? e)
       (not (player/client-loaded? e (inc (long (:tick world)))))))

(defn- void-deltas [world eid e]
  (when (and (pos? (double (:health e)))
             (< (v/y (:pos e)) (chunk/void-y world))
             (not (loading? world e)))
    (damage-deltas world eid e void-damage out-of-world)))

(def ^:private ^:table panic-causes
  (delay (set (data/tag-values "damage_type" "panic_causes"))))

(defn- panics? [e]
  (contains? @panic-causes (:hurt-cause e)))

(defn- panic-until [world e]
  (when (panics? e) (+ (long (:tick world)) panic-ticks)))

(defn- panicked [world e]
  (cond-> {:love-until nil :panic-until (panic-until world e)}
          (:hurt-cause e) (assoc :hurt-cause nil)))

(def ^:private ^:table drop-tables (delay (data/entity-drops)))

(defn- killed-by-player? [world e]
  (let [until (:hurt-by-player-until e)]
    (boolean (and until (< (long (:tick world)) (long until))))))

(defn- on-fire? [e]
  (or (pos? (long (or (:fire e) 0))) (boolean (:burning? e))))

(defn- worn
  "Returns the stacks that player e wears and holds, by equipment
  slot. Any other entity has none."
  [e]
  (when (entity/player? e) (player/equipment e)))

(defn- loot-view
  "Returns entity e as loot predicates see it, or nil for no e."
  [e]
  (when e
    (cond-> {:type (:type e) :living? (entity/living? e)
             :on-fire? (on-fire? e) :equipment (worn e)}
      (mobs/mob-type? (:type e)) (merge (mobs/loot-entity e)))))

(defn- credited-player
  "Returns [eid player] of the player who hurt e recently enough to
  count as its killer, while it is in the world."
  [world e]
  (when (killed-by-player? world e)
    (let [u (:last-hurt-by-player e)]
      (some (fn [[_ p :as entry]] (when (= u (:uuid p)) entry))
            (level/player-entries world)))))

(def ^:private ^:const mob-memory 100)

(defn- kill-credit
  "Returns [eid entity] of the one the death of e counts for. It is
  the player who hurt e recently, else the living one that last hurt
  it, if not too long ago."
  [world e]
  (or (credited-player world e)
      (let [[src at] (:last-hurt e)
            oid (:cause src)
            o (get-in world [:entities oid])]
        (when (and o (entity/living-attacker? src)
                   (<= (- (long (:tick world)) (long at)) mob-memory))
          [oid o]))))

(defn- luck ^double [p]
  (if p
    (attribute/value p (:effects p) :luck)
    0.0))

(defn- loot-ctx [world e]
  (let [src (:killed-by e)
        p (second (credited-player world e))]
    {:entity (loot-view e)
     :attacker (loot-view (:attacker src))
     :direct-attacker (loot-view (:direct-attacker src))
     :attacking-player (loot-view p)
     :damage-type (:type src)
     :direct? (= (:cause src) (:direct src))
     :luck (luck p)}))

(defn- loot-stacks [world eid e]
  (loot/drops @drop-tables (:type e) (loot-ctx world e)
              #(random/of-key (:tick world) eid %)))

(defn- drops-loot?
  "Returns true when e drops loot. A baby does not, nor any body while
  the mob drops rule is off."
  [world e]
  (and (get @drop-tables (:type e)) (mobs/drops-loot? world e)))

(defn- drop-deltas [world eid e]
  (when (drops-loot? world e)
    (let [t (:tick world)
          spawn (fn [i s]
                  (let [v (entity/pop-velocity [t eid :loot i])]
                    [:spawn-entity (entity/item (:pos e) v s)]))]
      (map-indexed spawn (loot-stacks world eid e)))))

(defn- keeps? [world] (get-in world [:rules :keep-inventory] false))

(defn- mob-reward [world eid e]
  (when (and (killed-by-player? world e) (mobs/drops-loot? world e)
             (not (mobs/flag? e :no-xp?)))
    (let [r (random/of-key (:tick world) eid :xp-reward)]
      (inc (long (* 3.0 r))))))

(defn- player-reward [world e]
  (xp/death-reward e (keeps? world) (game-mode/spectator? e)))

(defn- death-orbs
  "Returns the orbs e leaves where it died. Players drop some of
  their levels, animals a few points when a player killed them."
  [world eid e]
  (let [n (if (entity/player? e)
            (player-reward world e)
            (mob-reward world eid e))]
    (when (and n (pos? (long n)))
      [[:xp-award (vec (:pos e)) (long n) [:death eid]]])))

(defn- hushed
  "Returns the tick from which mob e hurt at tick t counts towards its
  next ambient sound. A late hurt, after the base tick of the mob,
  counts from one tick later."
  ^long [e ^long t late?]
  (+ t (mobs/ambient-interval e) (if late? 1 0)))

(defn- hurt-marks [world e health src late?]
  (cond-> {:health-sent health}
    src (assoc :struck-by nil)
    (:hurts e) (assoc :hurts nil)
    (:shell-sounds e) (assoc :shell-sounds nil)
    (not (entity/player? e)) (merge (panicked world e))
    (and src (pos? health) (mobs/mob-type? (:type e)))
    (assoc :say-tick (hushed e (:tick world) late?))))

(defn- voice [world eid e snd]
  (let [p (mobs/voice-pitch e (sound-pitch world eid e))
        m (out/sound snd (:pos e) (mobs/volume e) p)]
    (if (entity/player? e) (out/except eid m) (out/all m))))

(defn- struck-deltas
  "Returns the effects of a full hit from src on entity eid. Its
  viewers and the entity itself see the damage, and they hear the hurt
  sound, or the death sound when it killed."
  [world eid e src]
  (let [snd (hurt-sound e src)
        ev (out/damage-event
             eid (:type src) (:cause src) (:direct src) (:pos src))]
    (cond-> [(out/all ev)]
      (entity/player? e) (conj (out/to eid ev))
      snd (conj (voice world eid e snd)))))

(defn- marked-deltas
  "Returns the delta that marks the hurts of entity eid as shown,
  the game events of those that took health and the rolls of the
  shell of an armadillo, in this order."
  [world eid e health src late?]
  (let [ev (signal/game-event :entity-damage (:pos e) eid)]
    (concat
      [[:merge-entity eid (hurt-marks world e health src late?)]]
      (mapcat (fn [_] ev) (range (long (or (:hurts e) 0))))
      (shell/shell-deltas eid e (:tick world)))))

(def ^:private death-order
  (concat (range 36 45) (range 9 36) [45 8 7 6 5]))

(defn- vanishes? [s]
  (contains? (get-in s [:components :enchantments]) :vanishing-curse))

(defn- inventory-deltas
  "Returns the deltas of player eid, e, who dies and drops all it
  carries but what vanishes."
  [world eid e]
  (when-not (or (keeps? world) (game-mode/spectator? e))
    (let [inv (:inventory e)
          drops (remove vanishes? (keep inv death-order))]
      (cons [:merge-entity eid {:inventory (apply dissoc inv death-order)}]
            (map-indexed (fn [i s]
                           [:spawn-entity
                            (item/dropped world eid s true [:death i])])
                         drops)))))

(def ^:private ^:table death-kinds
  (delay (into {}
               (map (fn [[id m]] [(data/kebab id)
                                  [(get m "message_id")
                                   (get m "death_message_type")]]))
               (data/pack "damage_type"))))

(defn- killer [world src]
  (or (:attacker src) (get-in world [:entities (:cause src)])))

(defn- shown [world eid e]
  (entity/display-name eid e (team/of-entity (:teams world) eid e)))

(defn death-text
  "Returns the cause of the death of entity eid of world from src as
  chat shows it."
  [world eid e src]
  (let [[id kind] (get @death-kinds (:type src) ["generic" nil])
        v (shown world eid e)
        k (killer world src)
        [bid by] (if k [(:cause src) k] (credited-player world e))
        key (str "death.attack." id)]
    (cond
      (and (= "fall_variants" kind) (nil? by))
      {:translate "death.fell.accident.generic" :with [v]}
      k {:translate key :with [v (shown world bid k)]}
      by {:translate (str key ".player")
          :with [v (shown world bid by)]}
      :else {:translate key :with [v]})))

(defn- death-told
  "Returns the effect of death line msg of player eid of world for the
  players its team lets hear it."
  [world eid e msg]
  (let [t (team/of-entity (:teams world) eid e)]
    (case (if (entity/player? e) (:death-message t :always) :always)
      :always [(out/all msg)]
      :hide-for-other-teams [(out/team-except (:name t) eid msg)]
      :hide-for-own-team [(out/team-others (:name t) msg)]
      :never [])))

(defn- told-deltas [world eid e]
  (let [src (or (:struck-by e) {:type :generic})
        text (death-text world eid e src)]
    (if (get-in world [:rules :show-death-messages] true)
      (into [(out/to eid (out/combat-kill eid text))]
            (death-told world eid e (out/system-chat text)))
      [(out/to eid (out/combat-kill eid {:text ""}))])))

(defn- kill-deltas
  "Returns the stats of the death of e. A dead player counts who
  killed it, a player with the credit counts the kill, and a player
  who struck the blow counts the kind of mob it killed."
  [world eid e]
  (let [[kid k] (kill-credit world e)
        src (:killed-by e)
        player? (entity/player? e)]
    (concat
      (when (and k player?)
        [[:award eid (keyword "killed-by" (name (:type k))) 1]])
      (when (and (entity/player? k) (not= kid eid))
        [[:award kid (if player? :custom/player-kills :custom/mob-kills) 1]])
      (when (and (not player?) (entity/player? (:attacker src)))
        [[:award (:cause src) (keyword "killed" (name (:type e))) 1]]))))

(defn- player-died-deltas [world eid e]
  (when (entity/player? e)
    (concat
      (told-deltas world eid e)
      (inventory-deltas world eid e)
      (kill-deltas world eid e)
      [[:award eid :custom/deaths 1]
       [:merge-entity eid
        {:fire 0 :burning? false :ticks-frozen 0
         :last-death [(:dim world :overworld) (v/cell (:pos e))]
         :stats (assoc (:stats e) :custom/time-since-death 0
                       :custom/time-since-rest 0)}]])))

(defn- died-deltas [world eid e]
  (concat (signal/game-event :entity-die (:pos e) eid)
          [(out/all (out/status eid :death))]
          (if (entity/player? e)
            (player-died-deltas world eid e)
            (kill-deltas world eid e))
          (drop-deltas world eid e) (death-orbs world eid e)))

(defn- lost-deltas [world eid e health]
  (when-not (pos? (double health)) (died-deltas world eid e)))

(defn- armor-broken-deltas
  "Returns the deltas that show the armor player eid, e, broke in its
  hurts, and count each break."
  [eid e]
  (when-let [bs (seq (:broken e))]
    (concat
      [[:merge-entity eid {:broken nil}]]
      (mapcat (fn [[k item]]
                (let [fx (out/status eid (keyword (str "break-" (name k))))]
                  [(out/all fx) (out/to eid fx)
                   [:award eid (keyword "broken" (name item)) 1]]))
              bs))))

(defn report-deltas
  "Returns the deltas that show the hurts of entity eid since they
  were last shown. They are the game events of its hurts, the
  effects of its full hit, and on death its drops. A hurt is late
  when it came after the base tick of eid in this tick."
  ([world eid e] (report-deltas world eid e false))
  ([world eid e late?]
   (let [health (double (:health e))
         src (:struck-by e)
         lost? (< health (double (or (:health-sent e) health)))]
     (when (or src lost? (:hurts e) (:broken e))
       (concat
         (marked-deltas world eid e health src late?)
         (armor-broken-deltas eid e)
         (when src (struck-deltas world eid e src))
         (when lost? (lost-deltas world eid e health)))))))

(defn- gone-fx
  "Returns the effects of a body that goes. It loses its scores
  before it leaves its team."
  [world eid e]
  (let [nm (str (entity/uuid-of eid e))]
    (concat
      (filter #(= :fx (first %))
              (scoreboard/reset-all (:scoreboard world) nm))
      (when-let [t (team/of-member (:teams world) nm)]
        [(out/all (out/team :leave t [nm]))]))))

(defn timer-deltas
  "Returns the deltas of the death countdown of entity eid of world.
  A body that goes loses its scores and leaves its team."
  [world eid e]
  (let [dead? (not (pos? (double (:health e))))
        death (when dead? (inc (long (or (:death-time e) 0))))
        gone? (and death (>= (long death) mobs/death-ticks)
                   (not (entity/player? e)))]
    (concat
      (when death [[:merge-entity eid {:death-time death}]])
      (when gone? (concat [(out/all (out/status eid :poof))
                           [:remove-entity eid]]
                          (gone-fx world eid e))))))

(defn removal-deltas
  "Returns the deltas that take dead player eid off its level once
  its countdown ends. It puffs for its viewers and for itself, they
  lose its body, and it loses all it saw and every chunk."
  [world eid e]
  (when (and (not (entity/removed? e))
             (entity/removed? (update e :death-time (fnil inc 0))))
    (concat
      [(out/all (out/status eid :poof))
       (out/to eid (out/status eid :poof))]
      (for [[oid o] (:entities world)
            :when (contains? (:tracking o) eid)]
        [:tracking oid [] [eid]])
      [[:tracking eid [] (vec (:tracking e))]
       [:chunks-sent eid [] (vec (sort (:sent-chunks e))) nil]
       [:merge-entity eid {:chunk-pos nil :chunk-view nil
                           :chunks-pending? false}]])))

(defn- own-apply [world eid e d]
  (if-let [g (and (= eid (nth d 1 nil))
                  (get delta/entity-apply (nth d 0)))]
    (g (:tick world) e d)
    e))

(defn hurt-now
  "Returns entity eid after its own deltas of ds."
  [world eid e ds]
  (reduce (fn [e' d] (own-apply world eid e' d)) e ds))

(defn- player-burn-deltas
  "Returns the deltas of the hurt from the fire player eid burns in.
  Outside lava it takes one hurt every second."
  [world eid e ^long fire]
  (when (and (zero? (rem fire fire-damage-period))
             (not (in-lava? world e)))
    (damage-deltas world eid e 1.0 on-fire)))

(defn- burn-deltas [world eid e]
  (let [fire (long (or (:fire e) 0))]
    (when (pos? fire)
      (if (entity/player? e)
        (player-burn-deltas world eid e fire)
        (burn-tick-deltas world eid e fire)))))

(defn burnt-deltas
  "Returns the deltas of the fire living entity eid burns in,
  followed by those of the void."
  [world eid e]
  (into (vec (burn-deltas world eid e)) (void-deltas world eid e)))

(defn rest-deltas
  "Returns the delta that counts down the hurt resistance of entity
  eid, none when it has none left."
  [eid e]
  (when (pos? (long (or (:hurt-resist e) 0)))
    [[:rest eid]]))

(def ^:private ^:table harmful
  (delay (let [n (data/block-state-count) a (boolean-array n)]
           (dotimes [st n]
             (when (case (block/type-of st)
                     :cactus true
                     :campfire (= :true (:lit (block/props-of st)))
                     :sweet-berry-bush
                     (pos? (block/prop-long st :age))
                     false)
               (aset a st true)))
           a)))

(defn- on-magma? [world e]
  (and (:on-ground e) (not (:sneaking? e))
       (= :magma (block/type-of
                   (motion/below-state (:chunks world) (:pos e)
                                       (:support e) 0.2)))))

(defn- moving? [e]
  (let [[x _ z] (or (:client-vel e) [0.0 0.0 0.0])]
    (or (>= (Math/abs (double x)) 0.003)
        (>= (Math/abs (double z)) 0.003))))

(defn- cell-harm [e st]
  (case (block/type-of st)
    :cactus [1.0 {:type :cactus}]
    :campfire [(double (:fire-damage (data/info (block/block-of st)) 1))
               {:type :campfire}]
    :sweet-berry-bush (when (moving? e) [1.0 {:type :sweet-berry-bush}])
    nil))

(defn touch-deltas
  "Returns the deltas of the blocks that hurt living entity eid, e.
  The magma it stands on comes before the blocks it is in."
  [world eid e]
  (let [chunks (:chunks world)
        inside (when (some-inside? chunks e @harmful)
                 (keep #(cell-harm e (cell-state chunks %))
                       (inside-cells e)))]
    (concat
      (when (on-magma? world e)
        (damage-deltas world eid e 1.0 {:type :hot-floor}))
      (mapcat (fn [[n src]] (damage-deltas world eid e n src)) inside))))

(defn- touching? [world e]
  (or (on-magma? world e) (some-inside? (:chunks world) e @harmful)))

(def ^:const max-air 300)

(defn- air ^long [e] (long (:air e max-air)))

(defn- wall-cells [e]
  (let [p (:pos e)
        w (/ (double (unchecked-float (* 2.0 (double (first (entity/box e)))
                                         0.8)))
             2.0)
        ey (+ (v/y p) (entity/eye-height e))
        r (fn [c d] (range (num/floor (- c d)) (inc (num/floor (+ c d)))))]
    (for [x (r (v/x p) w) y (r ey 5.0E-7) z (r (v/z p) w)]
      [[x y z] [(- (v/x p) w) (- ey 5.0E-7) (- (v/z p) w)
                (+ (v/x p) w) (+ ey 5.0E-7) (+ (v/z p) w)]])))

(defn- meets? [[x y z] [ax ay az bx by bz] [a b c d f g]]
  (let [s (fn [o v] (+ (double o) (/ (double v) 16.0)))]
    (and (< (s x a) bx) (> (s x d) ax) (< (s y b) by) (> (s y f) ay)
         (< (s z c) bz) (> (s z g) az))))

(defn- in-wall? [world e]
  (and (not (game-mode/spectator? e)) (not (:sleeping e))
       (some (fn [[cell box]]
               (let [st (chunk/at (:chunks world) cell)]
                 (and (pos? st) (block/suffocating? st)
                      (some #(meets? cell box %)
                            (block/collision-boxes st)))))
             (wall-cells e))))

(defn wall-deltas
  "Returns the deltas of living entity eid, e, choking in a block."
  [world eid e]
  (when (in-wall? world e)
    (damage-deltas world eid e 1.0 {:type :in-wall})))

(def ^:private ^:table water-breathers
  (delay (set (data/tag-values "entity_type" "can_breathe_under_water"))))

(defn- breathes? [e]
  (let [fx (:effects e)]
    (or (contains? @water-breathers (:type e))
        (contains? fx :water-breathing) (contains? fx :conduit-power)
        (contains? fx :breath-of-the-nautilus)
        (and (entity/player? e) (game-mode/invulnerable? e)))))

(defn- refills? [e]
  (let [fx (:effects e)]
    (or (not (contains? fx :breath-of-the-nautilus))
        (contains? fx :water-breathing) (contains? fx :conduit-power))))

(defn- eye-in-water? [world e]
  (if (entity/player? e)
    (:eye-in-water? e)
    (liquid/eye-in-water? (:chunks world) (:pos e) (entity/eye-height e))))

(defn- drowned [world eid e]
  (concat [[:merge-entity eid {:air 0}]
           (out/all (out/status eid :drown-bubbles))]
          (when (entity/player? e)
            [(out/to eid (out/status eid :drown-bubbles))])
          (damage-deltas world eid e 2.0 {:type :drown})))

(defn- refilled [eid e]
  (when (< (air e) max-air)
    [[:merge-entity eid {:air (min max-air (+ (air e) 4))}]]))

(defn air-deltas
  "Returns the deltas of the air of living entity eid, e. It runs out
  with the eyes in water and comes back out of it."
  [world eid e]
  (if (eye-in-water? world e)
    (cond
      (not (breathes? e))
      (let [a (dec (air e))]
        (if (<= a -20)
          (drowned world eid e)
          [[:merge-entity eid {:air a}]]))
      (refills? e) (refilled eid e))
    (refilled eid e)))

(defn base-deltas
  "Returns the deltas of living entity eid at the start of its tick.
  The fire it burns in and the void come before the block it chokes
  in, its air and the countdown of its hurt resistance."
  [world eid e]
  (-> (vec (burnt-deltas world eid e))
      (into (wall-deltas world eid e))
      (into (air-deltas world eid e))
      (into (touch-deltas world eid e))
      (conj [:rest eid])))

(defn based?
  "Returns true when living entity e has work in the start of its
  base tick, which hurt resistance, fire, the void, a block at the
  eyes or water gives."
  [world e]
  (or (pos? (long (or (:hurt-resist e) 0)))
      (pos? (long (or (:fire e) 0)))
      (< (air e) max-air)
      (and (:health e)
           (< (v/y (:pos e)) (chunk/void-y world)))
      (eye-in-water? world e)
      (in-wall? world e)
      (touching? world e)))
