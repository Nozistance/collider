(ns collider.game.entity.hurt
  "Damage, fire, the void and death of entities, and the deltas
  that show them."
  (:require [collider.data :as data]
            [collider.game.attribute :as attribute]
            [collider.game.changes :as changes]
            [collider.game.delta :as delta]
            [collider.game.entity :as entity]
            [collider.game.experience :as xp]
            [collider.game.loot :as loot]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.game.slots :as slots]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.env.difficulty :as difficulty]
            [collider.world.env.weather :as weather]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ^:private ^:const void-damage 4.0)

(def ^:private ^:const panic-ticks 40)

(def ^:private ^:const voice-pitch-spread 0.2)

(def ^:private ^:const baby-voice-pitch 1.5)

(def ^:private ^:const adult-voice-pitch 1.0)

(def ^:private ^:const fluid-margin 0.001)

(def ^:private ^:const fire-damage-period 20)

(def ^:private ^:const ticks-per-second 20)

(defn- hurt-sound [e]
  (if (entity/player? e)
    (cond
      (not (pos? (double (:health e)))) :player/death
      (pos? (long (or (:fire e) 0))) :player/hurt-on-fire
      :else :player/hurt)
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
  (let [t (:type src) rule (get @rule-of t)]
    (or (and rule (not (get-in world [:rules rule] true)))
        (and (entity/player? (cause-of world src))
             (not (get-in world [:rules :pvp] true)))
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
  (if (entity/player? e)
    (when-not (spared? world e src)
      (let [n (double amount)
            n (if (scales? world src) (by-difficulty world n) n)]
        (when-not (zero? n) n)))
    amount))

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

(def ^:private ^:const sunk-shrink-xz 0.1)

(def ^:private ^:const sunk-shrink-y 0.4)

(def ^:private ^:const fire-bit 1)

(def ^:private ^:const lava-bit 2)

(def ^:private ^:const sunk-bit 4)

(defn- any-bit? [^long flags ^long mask]
  (not (zero? (bit-and flags mask))))

(defn- floor-lo ^long [^double a]
  (long (Math/floor (+ a fluid-margin))))

(defn- floor-hi ^long [^double a]
  (long (Math/floor (+ (- a fluid-margin) 1.0))))

(defn- span
  ^longs [p ^double half ^double height [sxz sy]]
  (let [px (v/x p) py (v/y p) pz (v/z p)
        sy (double sy)
        h (- half (double sxz))]
    (doto (long-array 6)
      (aset 0 (floor-lo (- px h))) (aset 1 (floor-hi (+ px h)))
      (aset 2 (max chunk/min-y (floor-lo (+ py sy))))
      (aset 3 (min (inc chunk/max-y) (floor-hi (- (+ py height) sy))))
      (aset 4 (floor-lo (- pz h))) (aset 5 (floor-hi (+ pz h))))))

(defn- burn-bit ^long [st]
  (cond (block/lava? st) lava-bit (block/fire? st) fire-bit :else 0))

(def ^:private ^:table burn-bits
  (delay
    (let [a (byte-array (data/block-state-count))]
      (dotimes [st (alength a)] (aset a st (byte (burn-bit st))))
      a)))

(defn probe
  "Returns the flags of the burning blocks entity e touches.
  Zero means it touches none."
  (^long [world e] (probe world e (box-of e)))
  (^long [world e [half height]]
   (let [p (:pos e)
         outer (span p (double half) (double height)
                     [fluid-margin fluid-margin])
         inner (span p (double half) (double height)
                     [sunk-shrink-xz sunk-shrink-y])]
     (phys/burns (:chunks world) @burn-bits outer inner))))

(defn- burning-flag [eid e ^long fire sunk?]
  (let [lit? (boolean (or (pos? fire) sunk?))]
    (when (not= lit? (boolean (:burning? e)))
      [[:merge-entity eid {:burning? lit?}]])))

(def ^:private on-fire {:type :on-fire})

(def ^:private in-fire {:type :in-fire})

(def ^:private in-lava {:type :lava})

(def ^:private out-of-world {:type :out-of-world})

(defn- burn-tick-deltas [eid fire wet? in-lava?]
  (when (and (pos? fire) (not wet?))
    (let [due? (and (zero? (rem fire fire-damage-period))
                    (not in-lava?))]
      (cond-> [[:merge-entity eid {:fire (dec fire)}]]
              due? (conj [:damage eid 1.0 on-fire])))))

(defn- fire-ticks ^long [fire seconds]
  (max (long fire) (* ticks-per-second (long seconds))))

(defn- ignite-deltas [eid fire wet? damage seconds src]
  (cond-> [[:damage eid (double damage) src]]
          (not wet?)
          (conj [:merge-entity eid
                 {:fire (fire-ticks fire seconds)}])))

(defn- fizz-cell [e]
  (mapv (fn [c] (long (Math/floor (double c))))
        [(v/x (:pos e)) (v/y (:pos e)) (v/z (:pos e))]))

(defn- douse-deltas [eid e fire wet?]
  (when (and wet? (pos? (long fire)))
    (cons [:merge-entity eid {:fire 0 :burning? false}]
          [(out/all (out/fizz (fizz-cell e)))])))

(defn- lit-by-deltas [eid e fire wet? flags]
  (let [touch (any-bit? flags (bit-or fire-bit lava-bit))
        sunk? (any-bit? flags sunk-bit)
        ignite (fn [d s src] (ignite-deltas eid fire wet? d s src))]
    (concat (burning-flag eid e fire sunk?)
            (when touch (ignite 1.0 fire-seconds in-fire))
            (when sunk? (ignite lava-damage lava-seconds in-lava))
            (douse-deltas eid e fire wet?))))

(defn- lit-deltas [eid e fire wet? flags]
  (when (or (pos? (long fire)) (pos? (long flags)) (:burning? e))
    (lit-by-deltas eid e fire wet? flags)))

(def ^:private ^:const fire-rest -20)

(def ^:private ^:const inside-margin 1.0E-5)

(def ^:private ^:const fluid-slack 1.0E-7)

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

(defn- in-fluid? [chunks e cls c]
  (when-let [top (liquid/surface chunks cls c)]
    (< (v/y (:pos e)) (- (double top) fluid-slack))))

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

(defn- all-air? [chunks e]
  (let [p (:pos e)
        [half height] (box-of e)
        h (double half) x (v/x p) y (v/y p) z (v/z p)
        top (+ y (double height))]
    (not (phys/some-cell?
           chunks @occupied
           (lo-cell (- x h)) (lo-cell y) (lo-cell (- z h))
           (inc (hi-cell (+ x h))) (inc (hi-cell top))
           (inc (hi-cell (+ z h)))))))

(def ^:private calm
  {:fire? false :lava? false :water? false :snow []})

(defn- touched [world e]
  (let [chunks (:chunks world)
        cs (inside-cells e)
        of (fn [pred] (boolean (some pred cs)))]
    {:fire? (of #(block/fire? (cell-state chunks %)))
     :lava? (of #(in-fluid? chunks e :lava %))
     :water? (of #(in-fluid? chunks e :water %))
     :snow (filterv #(= :powder-snow
                        (block/type-of (cell-state chunks %)))
                    cs)}))

(defn- contact [world e]
  (if (all-air? (:chunks world) e)
    calm
    (touched world e)))

(defn- rained-on? [world e]
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

(defn- lit-by ^long [cap ^long f c ^long added]
  (let [f (if (:fire? c) (fire-touched cap f added) f)]
    (if (:lava? c)
      (long (cap (max f (* ticks-per-second lava-seconds))))
      f)))

(defn- player-fire [world eid e c]
  (let [cap (capper e)
        f0 (long (or (:fire e) 0))
        f1 (if (pos? f0) (long (cap (dec f0))) f0)
        lit (lit-by cap f1 c (added-ticks world eid))
        wet? (or (:water? c) (seq (:snow c)) (rained-on? world e))
        f (if wet? (min 0 lit) lit)]
    [f0 f1 lit (if (and (<= f 0) (<= f f1)) fire-rest f)]))

(defn- melt-effect [chunks c]
  (out/all (out/break-effect c (cell-state chunks c))))

(defn- melt-deltas [world cells]
  (when (seq cells)
    (concat (changes/set-deltas world (mapv (fn [c] [c 0]) cells))
            (map #(melt-effect (:chunks world) %) cells))))

(defn- put-out-sound [world eid e]
  (let [t (long (:tick world))
        r (- (random/of-longs t eid (hash :put-out1))
             (random/of-longs t eid (hash :put-out2)))]
    (out/all (out/sound :generic/extinguish-fire (:pos e) 0.7
                        (+ 1.6 (* 0.4 r)) :players))))

(defn- contact-hurts
  [world eid e c]
  (concat (when (:fire? c) (damage-deltas world eid e 1.0 in-fire))
          (when (:lava? c)
            (damage-deltas world eid e lava-damage in-lava))))

(defn- player-fire-deltas [world eid e]
  (let [c (contact world e)
        [f0 f1 lit f] (player-fire world eid e c)]
    (concat (contact-hurts world eid e c)
            (burning-flag eid e f1 false)
            (when (not= f f0) [[:merge-entity eid {:fire f}]])
            (when (pos? (long lit))
              (delta/authored
                (melt-deltas world (:snow c))
                (delta/entity-author eid e)))
            (when (and (pos? (long f1)) (<= (long f) 0))
              [(put-out-sound world eid e)]))))

(defn- cool? [world e half height]
  (phys/cool? (:chunks world) @burn-bits (:pos e) half height))

(defn fire-deltas
  "Returns the deltas of the fire and lava entity eid touches,
  wet as wet? tells or as it is."
  ([world eid e] (fire-deltas world eid e (:wet? e)))
  ([world eid e wet?]
   (if (entity/player? e)
     (player-fire-deltas world eid e)
     (let [fire (long (or (:fire e) 0))
           [half height :as box] (box-of e)]
       (when-not (and (zero? fire) (not (:burning? e))
                      (cool? world e half height))
         (lit-deltas eid e fire (boolean wet?)
                     (probe world e box)))))))

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

(defn- item-fire-deltas [world eid e ^long flags]
  (let [fire (long (or (:fire e) 0))
        touched? (or (pos? fire) (pos? flags))
        wet? (boolean (and touched? (item-wet? world e)))
        lava? (pos? (bit-and flags lava-bit))
        ignite (fn [d s] (ignite-deltas eid fire wet? d s nil))]
    (concat (burning-flag eid e fire false)
            (burn-tick-deltas eid fire wet? lava?)
            (when (pos? (bit-and flags fire-bit))
              (ignite 1.0 fire-seconds))
            (when lava? (ignite lava-damage lava-seconds))
            (douse-deltas eid e fire wet?))))

(defn- damage-sum ^double [deltas]
  (reduce (fn [^double s d]
            (if (= :damage (nth d 0)) (+ s (double (nth d 2))) s))
          0.0 deltas))

(defn- burn-sound-deltas [world eid e ^double health]
  (when (or (<= (- health lava-damage) 0.0)
            (= :experience-orb (:type e))
            (zero? (rem (inc (long (or (:age e) 0)))
                        burn-sound-period)))
    (let [r (random/of-key (:tick world) eid :burn)
          pitch (+ burn-pitch (* burn-pitch-spread r))]
      [(out/all
         (out/sound :generic/burn (:pos e) burn-volume pitch))])))

(defn- item-burn-deltas [world eid e ^long flags]
  (let [ds (item-fire-deltas world eid e flags)
        health (double (:health e))]
    (concat ds
            (when (pos? (bit-and flags lava-bit))
              (burn-sound-deltas world eid e health))
            (when (>= (damage-sum ds) health)
              [[:remove-entity eid]]))))

(defn- unlit? [e ^long flags]
  (and (zero? flags) (not (pos? (long (or (:fire e) 0))))
       (not (:burning? e))))

(defn item-deltas
  "Returns the deltas of the fire and lava item or orb eid lies in."
  [world eid e]
  (if (fire-proof-item? e)
    (when (pos? (long (or (:fire e) 0)))
      [[:merge-entity eid {:fire 0}]])
    (let [flags (probe world e)]
      (if (unlit? e flags)
        (when (>= 0.0 (double (:health e))) [[:remove-entity eid]])
        (item-burn-deltas world eid e flags)))))

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

(def ^:private ^:const player-kill-memory 100)

(def ^:private ^:table drop-tables (delay (data/entity-drops)))

(defn- killed-by-player? [world e]
  (let [at (:hurt-by-player e)]
    (boolean (and at (< (- (long (:tick world)) (long at))
                        player-kill-memory)))))

(defn- on-fire? [e]
  (or (pos? (long (or (:fire e) 0))) (boolean (:burning? e))))

(defn- worn
  "Returns the stacks that player e wears and holds, by equipment
  slot. Any other entity has none."
  [e]
  (when (entity/player? e)
    (let [at (assoc slots/armor :mainhand (player/hand-slot e :main)
                    :offhand slots/offhand)]
      (into {}
            (keep (fn [[k i]]
                    (when-let [s (get-in e [:inventory i])] [k s])))
            at))))

(defn- loot-view
  "Returns entity e as loot predicates see it, or nil for no e."
  [e]
  (when e
    (cond-> {:type (:type e) :living? (entity/living? e)
             :on-fire? (on-fire? e) :equipment (worn e)}
      (mobs/mob-type? (:type e)) (merge (mobs/loot-entity e)))))

(defn- attacking-player
  "Returns the player who hurt mob e recently enough to count as its
  killer, while it is in the world."
  [world e]
  (when (killed-by-player? world e)
    (let [p (get-in world [:entities (:last-hurt-by-player e)])]
      (when (entity/player? p) p))))

(defn- luck ^double [p]
  (if p (attribute/value p (:effects p) :luck) 0.0))

(defn- loot-ctx [world e]
  (let [src (:killed-by e)
        p (attacking-player world e)]
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
  (and (get @drop-tables (:type e))
       (not (mobs/baby? e))
       (get-in world [:rules :mob-drops] true)))

(defn- drop-deltas [world eid e]
  (when (drops-loot? world e)
    (let [t (:tick world)
          spawn (fn [i s]
                  (let [v (entity/pop-velocity [t eid :loot i])]
                    [:spawn-entity (entity/item (:pos e) v s)]))]
      (map-indexed spawn (loot-stacks world eid e)))))

(defn- keeps? [world] (get-in world [:rules :keep-inventory] false))

(defn- mob-reward [world eid e]
  (when (and (killed-by-player? world e) (not (mobs/baby? e))
             (get-in world [:rules :mob-drops] true))
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

(def ^:private ^:const say-interval 120)

(defn- hushed
  "Returns the tick from which a mob hurt at tick t counts towards its
  next ambient sound. A late hurt, after the base tick of the mob,
  counts from one tick later."
  ^long [^long t late?]
  (+ t say-interval (if late? 1 0)))

(defn- hurt-marks [world e health src late?]
  (cond-> {:health-sent health}
    src (assoc :struck-by nil)
    (not (entity/player? e)) (merge (panicked world e))
    (and src (pos? health) (mobs/mob-type? (:type e)))
    (assoc :say-tick (hushed (:tick world) late?))))

(defn- voice [world eid e snd]
  (out/all (out/sound snd (:pos e) 1.0 (sound-pitch world eid e))))

(defn- struck-deltas
  "Returns the effects of a full hit from src on entity eid. Its
  viewers and the entity itself see the damage, and they hear the hurt
  sound, or the death sound when it killed."
  [world eid e src]
  (let [snd (hurt-sound e)
        ev (out/damage-event
             eid (:type src) (:cause src) (:direct src) (:pos src))]
    (cond-> [(out/all ev)]
      (entity/player? e) (conj (out/to eid ev))
      snd (conj (voice world eid e snd)))))

(defn- died-deltas [world eid e]
  (concat [(out/all (out/status eid :death))]
          (drop-deltas world eid e) (death-orbs world eid e)))

(defn- lost-deltas [world eid e health]
  (concat (when-not (pos? (double health)) (died-deltas world eid e))
          (when (entity/player? e)
            [(out/to eid (out/health health))])))

(defn report-deltas
  "Returns the deltas that show the hurts of entity eid since they
  were last shown. They are the effects of its full hit, and on
  death its drops. A hurt is late when it came after the base tick
  of eid in this tick."
  ([world eid e] (report-deltas world eid e false))
  ([world eid e late?]
   (let [health (double (:health e))
         src (:struck-by e)
         lost? (< health (double (or (:health-sent e) health)))]
     (when (or src lost?)
       (concat
         [[:merge-entity eid (hurt-marks world e health src late?)]]
         (when src (struck-deltas world eid e src))
         (when lost? (lost-deltas world eid e health)))))))

(defn timer-deltas
  "Returns the deltas of the death countdown of entity eid."
  [eid e]
  (let [dead? (not (pos? (double (:health e))))
        death (when dead? (inc (long (or (:death-time e) 0))))
        gone? (and death (>= (long death) mobs/death-ticks)
                   (not (entity/player? e)))]
    (concat
      (when death [[:merge-entity eid {:death-time death}]])
      (when gone? [[:remove-entity eid]]))))

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
             (not (any-bit? (probe world e) lava-bit)))
    (damage-deltas world eid e 1.0 on-fire)))

(defn- burn-deltas [world eid e]
  (let [fire (long (or (:fire e) 0))]
    (when (pos? fire)
      (if (entity/player? e)
        (player-burn-deltas world eid e fire)
        (let [lava? (any-bit? (probe world e) lava-bit)]
          (burn-tick-deltas eid fire (boolean (:wet? e)) lava?))))))

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

(defn base-deltas
  "Returns the deltas of living entity eid at the start of its tick.
  The fire it burns in and the void come before the countdown of its
  hurt resistance."
  [world eid e]
  (conj (burnt-deltas world eid e) [:rest eid]))

(defn based?
  "Returns true when living entity e has work in the start of its
  base tick, which hurt resistance, fire or the void gives."
  [world e]
  (or (pos? (long (or (:hurt-resist e) 0)))
      (pos? (long (or (:fire e) 0)))
      (and (:health e)
           (< (v/y (:pos e)) (chunk/void-y world)))))
