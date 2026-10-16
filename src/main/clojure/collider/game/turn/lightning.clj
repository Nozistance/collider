(ns collider.game.turn.lightning
  "The turn of a lightning bolt. It sets fire, powers a rod, cleans
  copper and hits the bodies around it while it flashes."
  (:require [collider.cell :as cell]
            [collider.game.apply :as apply]
            [collider.game.areas :as areas]
            [collider.game.deltas :as deltas]
            [collider.game.changes :as changes]
            [collider.game.entity :as entity]
            [collider.game.entity.hurt :as hurt]
            [collider.game.entity.size :as size]
            [collider.game.mob.spec :as spec]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.block.oxidation :as oxidation]
            [collider.world.blocks.fire :as fire]
            [collider.world.blocks.lightning-rod :as lightning-rod]
            [collider.world.blocks.support :as support]
            [collider.world.env.difficulty :as difficulty]))

(set! *warn-on-reflection* true)

(def ^:private ^:const normal 2)

(def ^:private ^:const reach 3.0)

(def ^:private ^:const damage 5.0)

(def ^:private ^:const lit-ticks 160)

(def ^:private unhurt
  #{:armor-stand :painting :item-frame :glow-item-frame :leash-knot})

(defn- roll [world eid & ks]
  (random/of-key [(:tick world) eid ks]))

(defn- fire-at [world q]
  (let [st (fire/state-for (:chunks world) q)]
    (when (and (block/air-type? (changes/block-at world q))
               (support/supported? (:chunks world) q st))
      [q st])))

(defn- shifted [p offs]
  (mapv + p (mapv #(dec (random/below % 3)) offs)))

(defn- fire-deltas
  "Returns the deltas of the fires bolt eid at pos sets, in its cell
  and in n cells around it."
  [world eid pos n]
  (let [p (v/cell pos)
        r #(roll world eid :fire %1 %2)
        near (for [i (range n)] (shifted p (map #(r i %) [0 1 2])))]
    (when (areas/fire-spreads-at? world p)
      (when-let [cs (seq (keep #(fire-at world %) (cons p near)))]
        (changes/change-deltas world (vec cs))))))

(defn- first-stage [st]
  (loop [s st] (if-let [p (oxidation/scraped s)] (recur p) s)))

(defn- state-at [world cur q] (get cur q (changes/block-at world q)))

(defn- step [world eid at cur k]
  (let [r #(roll world eid :copper k %1 %2)
        cands (for [i (range 10)]
                (shifted at (map #(r i %) [0 1 2])))]
    (first (filter #(oxidation/ages? (state-at world cur %)) cands))))

(defn- spark [q]
  (out/all (out/level-event :particles-electric-spark q -1)))

(defn- scraped-at [world cur q]
  (if-let [prev (oxidation/scraped (state-at world cur q))]
    (assoc cur q prev)
    cur))

(defn- walked [world eid start [cur fx] w]
  (let [n (inc (random/below (roll world eid :steps w) 8))]
    (loop [at start i 0 cur cur fx fx]
      (if-let [q (when (< i n) (step world eid at cur [w i]))]
        (recur q (inc i) (scraped-at world cur q) (conj fx (spark q)))
        [cur fx]))))

(defn- copper
  "Returns the cells cur with the copper at the struck cell p cleaned,
  a first stage for it and a few random walks that scrape, and the
  sparks fx after them."
  [world eid p [cur fx]]
  (let [st (state-at world cur p)
        ages? (oxidation/ages? st)]
    (if (or ages? (oxidation/unwaxed st))
      (let [cur (cond-> cur ages? (assoc p (first-stage st)))
            walks (+ 3 (random/below (roll world eid :walks) 3))]
        (reduce #(walked world eid p %1 %2) [cur fx] (range walks)))
      [cur fx])))

(defn- rod
  "Returns the cells and the effects of a bolt that powers the rod at
  the struck cell p, none when p holds no rod."
  [world p]
  (let [st (changes/block-at world p)
        due (+ (long (:tick world)) lightning-rod/powered-ticks)
        spark #(out/level-event :particles-electric-spark p
                                (lightning-rod/axis-id st))]
    (if (lightning-rod/rod? st)
      [{p (lightning-rod/powered st true)}
       [[:schedule-ticks {due [(cell/pack p)]}] (out/all (spark))]]
      [{} []])))

(defn- struck-deltas [world eid p]
  (let [[cur fx] (copper world eid p (rod world p))]
    (concat (when (seq cur) (changes/change-deltas world (vec cur)))
            fx)))

(defn- struck-cell [[x y z]]
  (v/cell [x (- (double y) 1.0E-6) z]))

(defn- in-box? [[bx by bz] e]
  (let [[h ht] (or (size/box e) [0.0 0.0]) [x y z] (:pos e)
        h (double h) ht (double ht)]
    (and (< (- (double x) h) (+ (double bx) reach))
         (> (+ (double x) h) (- (double bx) reach))
         (< (double y) (+ (double by) 6.0 reach))
         (> (+ (double y) ht) (- (double by) reach))
         (< (- (double z) h) (+ (double bz) reach))
         (> (+ (double z) h) (- (double bz) reach)))))

(defn- thunder-hit [world eid e bid]
  (if-let [f (:thunder-hit (spec/of (:type e)))]
    (f eid e bid)
    (when-not (contains? unhurt (:type e))
      (let [f (inc (long (or (:fire e) 0)))
            lit (if (zero? f)
                  {:fire lit-ticks :ticks-frozen 0}
                  {:fire f})]
        (cons [:merge-entity eid lit]
              (hurt/damage-deltas world eid e damage
                                  {:type :lightning-bolt}))))))

(defn- hits [world bid b]
  (for [[eid e] (:entities world)
        :when (and (not= eid bid) (entity/alive? e)
                   (not (entity/removed? e)) (in-box? (:pos b) e))
        d (thunder-hit world eid e bid)]
    d))

(defn- next-life [world eid life flashes]
  (cond (>= life 0) [life flashes nil]
        (zero? flashes) [life flashes :gone]
        (< life (- (random/below (roll world eid :flash flashes) 10)))
        [1 (dec flashes) :flash]
        :else [life flashes nil]))

(defn- first-deltas [world eid b]
  (concat (when (>= (difficulty/id world) normal)
            (fire-deltas world eid (:pos b) 4))
          (struck-deltas world eid (struck-cell (:pos b)))))

(defn- end-deltas [world eid b life flashes how]
  (if (= :gone how)
    [[:remove-entity eid]]
    (concat [[:merge-entity eid {:life life :flashes flashes}]]
            (when (>= (long life) 0) (hits world eid b)))))

(defn- turn [world [eid b]]
  (let [life (long (:life b))
        [life' flashes how] (next-life world eid (dec life)
                                       (long (:flashes b)))]
    (concat (when (= 2 life) (first-deltas world eid b))
            (when (= :flash how) (fire-deltas world eid (:pos b) 0))
            (end-deltas world eid b life' flashes how))))

(defn turns
  "Returns the deltas of every lightning bolt of level world in one
  tick, after the turns ds of the other entities. A bolt sees the
  bodies as their turns left them."
  [world ds]
  (when-let [bolts (seq (areas/active-of-types world
                                               [:lightning-bolt]))]
    (let [seen (apply/entities world (deltas/as-vec ds))]
      (mapcat #(turn seen %) bolts))))
