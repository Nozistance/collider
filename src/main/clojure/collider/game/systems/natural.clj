(ns collider.game.systems.natural
  "Natural spawning of mobs around the players, as NaturalSpawner
  runs it from ServerChunkCache.tickChunks. The rolls are keyed, so
  each chunk plans its spawns from the start of the tick on its own;
  the mob caps and the mobs spawned before then decide the plans in
  the shuffled order of the chunks."
  (:require [collider.data.long-map :as lm]
            [collider.game.deltas :as deltas]
            [collider.game.entity.size :as size]
            [collider.game.mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.game.areas :as areas]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.biome :as biome]
            [collider.world.env.difficulty :as difficulty]
            [collider.world.phys :as phys]
            [collider.world.space.spawn :as spawn]))

(set! *warn-on-reflection* true)

(def ^:private ^:const persistent-period 400)

(def ^:private ^:const reach 8)

(def ^:private ^:const magic 289)

(def ^:private ^:const near-sq 16384.0)

(def ^:private ^:const min-sq 576.0)

(def ^:private ^:const cluster 4)

(def ^:private ^:const baby-chance 0.05)

(defn- roll ^double [^long t ^long cid salt ^long i]
  (random/of-longs t cid (long (hash salt)) i))

(defn- below ^long [^double r ^long n] (long (* r n)))

(defn- spawners [w]
  (into [] (keep #(let [e (get-in w [:entities %])]
                    (when (and e (not (game-mode/spectator? e)))
                      [% e])))
        (vals (:players w))))

(defn- center-sq ^double [^long cid p]
  (let [[cx cz] (chunk/id->pos cid)
        dx (- (+ (* 16.0 (long cx)) 8.0) (v/x p))
        dz (- (+ (* 16.0 (long cz)) 8.0) (v/z p))]
    (+ (* dx dx) (* dz dz))))

(defn- close? [cid e] (< (center-sq cid (:pos e)) near-sq))

(defn- counted [w]
  (into (lm/long-set)
        (mapcat #(let [[cx cz] (chunk/id->pos %)]
                   (chunk/around-ids cx cz reach)))
        (areas/player-chunks w)))

(defn- near [ctx ^long cid]
  (if (contains? (:counted ctx) cid)
    (into [] (keep #(when (close? cid (nth % 1)) (nth % 0)))
          (:players ctx))
    []))

(defn- category-of [e] (:category (spawn/facts (:type e))))

(defn- add-mob [counts near-ids cat]
  (reduce #(update-in %1 [:local %2 cat] (fnil inc 0))
          (update-in counts [:global cat] (fnil inc 0)) near-ids))

(defn- mob-counts [w ctx]
  (reduce (fn [c e]
            (let [cat (category-of e) cid (chunk/pos-chunk (:pos e))]
              (if (and cat (contains? (:chunks w) cid))
                (add-mob c (near ctx cid) cat)
                c)))
          {} (vals (:entities w))))

(defn- cap ^long [[_ f]] (long (:max f)))

(defn- global-ok? [ctx [cat :as c]]
  (< (long (get-in @(:counts ctx) [:global cat] 0))
     (quot (* (cap c) (count (:counted ctx))) magic)))

(defn- local-ok? [ctx counts cid [cat :as c]]
  (some #(< (long (get-in counts [:local % cat] 0)) (cap c))
        (near ctx cid)))

(defn- spawners-of [ctx cat] (get-in ctx [:biome :spawners cat]))

(defn- biome-of [w] (biome/at (:dim w :overworld) nil))

(defn- creatable? [w cat]
  (some #(mobs/mob-type? (:type %))
        (get-in (biome-of w) [:spawners cat])))

(defn- enemies? [w]
  (and (get-in w [:rules :spawn-mobs] true)
       (get-in w [:rules :spawn-monsters] true)))

(defn- wanted? [w [cat f]]
  (and (not= :misc cat) (or (enemies? w) (:friendly f))
       (or (zero? (rem (long (:tick w)) persistent-period))
           (not (:persistent f)))
       (creatable? w cat)))

(defn- at ^long [ctx x y z]
  (chunk/chunks-get-block (:chunks ctx) x y z))

(defn- start [ctx cid cat]
  (let [t (long (:t ctx)) [cx cz] (chunk/id->pos cid)
        r #(roll t cid [:start cat %] 0)
        x (+ (* 16 (long cx)) (below (r :x) 16))
        z (+ (* 16 (long cz)) (below (r :z) 16))
        lo (long (:min-y ctx))
        top (inc (spawn/surface-top (:chunks ctx) x z))]
    [x (+ lo (below (r :y) (inc (- top lo)))) z]))

(defn- opens? [ctx [x y z]]
  (and (> (long y) (long (:min-y ctx)))
       (not (block/conductor? (at ctx x y z)))))

(defn- nearest-sq [ctx x y z]
  (let [p [x y z]]
    (reduce (fn [best [_ e]]
              (let [d (v/dist-sq (:pos e) p)]
                (if (or (nil? best) (< d (double best))) d best)))
            nil (:players ctx))))

(defn- by-spawn? [ctx ^long x ^long y ^long z]
  (when-let [[sx sy sz] (:spawn ctx)]
    (let [dx (- (long sx) x) dy (- (+ (long sy) 0.5) y)
          dz (- (long sz) z)]
      (< (+ (* dx dx) (* dy dy) (* dz dz)) min-sq))))

(defn- chunk-of ^long [x z]
  (chunk/pos->id (bit-shift-right (long x) 4)
                 (bit-shift-right (long z) 4)))

(defn- right-distance? [ctx cid d2 x y z]
  (let [c (chunk-of x z)]
    (and (> (double d2) min-sq) (not (by-spawn? ctx x y z))
         (or (= c (long cid)) (contains? (:active ctx) c)))))

(defn- pick [ctx cid cat salt ll]
  (let [es (spawners-of ctx cat)
        total (long (reduce + 0 (map :weight es)))]
    (when (pos? total)
      (loop [[e & more] es
             r (below (roll (:t ctx) cid salt ll) total)]
        (let [w (long (:weight e))]
          (if (< r w) e (recur more (- r w))))))))

(defn- counted-kind [ctx cid cat g ll]
  (when-let [e (pick ctx cid cat [cat g :pick] ll)]
    (let [lo (long (:min e)) span (inc (- (long (:max e)) lo))
          r (roll (:t ctx) cid [cat g :count] ll)]
      [(get (:kinds ctx) (:type e)) (+ lo (below r span))])))

(defn- box-at [k x y z]
  (spawn/spawn-box k (+ (long x) 0.5) y (+ (long z) 0.5)))

(defn- fits? [ctx k d2 x y z]
  (let [cs (:chunks ctx)
        far (long (get-in ctx [:despawn (:category k)]))]
    (and (or (:far k) (<= (double d2) (double (* far far))))
         (:summon k)
         (spawn/position-ok? cs k x y z)
         (spawn/rules-ok? cs k (:peaceful? ctx) x y z)
         (spawn/mob-box-free? cs (box-at k x y z)))))

(defn- mob-box [k x y z] (box-at (dissoc k :scale) x y z))

(defn- box-of [e]
  (when-let [[half height] (size/box e)]
    (let [p (:pos e) h (double half)]
      [(- (v/x p) h) (v/y p) (- (v/z p) h)
       (+ (v/x p) h) (+ (v/y p) (double height)) (+ (v/z p) h)])))

(defn- blocks-building? [e]
  (case (:type e)
    (:tnt :falling-block) true
    :player (not (game-mode/spectator? e))
    (mobs/mob-type? (:type e))))

(defn- blockers [w]
  (reduce (fn [m e]
            (if-let [b (when (blocks-building? e) (box-of e))]
              (update m (chunk/pos-chunk (:pos e)) (fnil conj []) b)
              m))
          (lm/long-map) (vals (:entities w))))

(defn- blocked? [ctx box]
  (let [cx (bit-shift-right (long (Math/floor (double (box 0)))) 4)
        cz (bit-shift-right (long (Math/floor (double (box 2)))) 4)
        hits? #(some (fn [b] (phys/joined? box b)) %)]
    (some #(hits? (get @(:blockers ctx) %))
          (chunk/around-ids cx cz 1))))

(defn- clear? [ctx k x y z]
  (let [h (/ (double (float (:width k))) 2.0)
        at [(+ (long x) 0.5) y (+ (long z) 0.5)]]
    (and (phys/dry? (:chunks ctx) at h (double (float (:height k))))
         (not (blocked? ctx (mob-box k x y z))))))

(defn- attempt [ctx k d2 [x y z]]
  (when (and (fits? ctx k d2 x y z) (clear? ctx k x y z))
    {:kind k :pos [(+ (long x) 0.5) (double y) (+ (long z) 0.5)]
     :box (mob-box k x y z)}))

(defn- stepped [ctx cid cat g ll [x y z]]
  (let [r #(below (roll (:t ctx) cid [cat g %] ll) 6)]
    [(+ (long x) (- (r :x1) (r :x2))) y
     (+ (long z) (- (r :z1) (r :z2)))]))

(defn- chosen [ctx cid cat g ll s]
  (if-let [[k n] (when-not (:k s) (counted-kind ctx cid cat g ll))]
    (if k (assoc s :k k :n n) (assoc s :stop ::abort))
    (if (:k s) s (assoc s :stop ::none))))

(defn- tried [ctx cid g ll d2 s]
  (if-let [tr (attempt ctx (:k s) d2 (:p s))]
    (update s :acc conj (assoc tr :g g :ll ll))
    s))

(defn- visit [ctx cid cat g ll s]
  (let [[x y z :as p] (stepped ctx cid cat g ll (:p s))
        s (assoc s :p p)
        d2 (nearest-sq ctx (+ (long x) 0.5) y (+ (long z) 0.5))]
    (if (and d2 (right-distance? ctx cid d2 x y z))
      (let [s (chosen ctx cid cat g ll s)]
        (if (:stop s) s (tried ctx cid g ll d2 s)))
      s)))

(defn- group [ctx cid cat g at0]
  (let [n (Math/ceil (* 4.0 (roll (:t ctx) cid [cat g :n] 0)))]
    (loop [ll 0 s {:p at0 :n (long n) :acc []}]
      (cond (= ::abort (:stop s)) (conj (:acc s) ::abort)
            (or (:stop s) (>= ll (long (:n s)))) (:acc s)
            :else (recur (inc ll) (visit ctx cid cat g ll s))))))

(defn- tries [ctx cid cat at0]
  (reduce (fn [acc g]
            (let [acc (into acc (group ctx cid cat g at0))]
              (if (= ::abort (peek acc)) (reduced acc) acc)))
          [] (range 3)))

(defn- plan [ctx cats cid]
  (into [] (keep (fn [[cat :as c]]
                   (let [p (start ctx cid cat)]
                     (when (opens? ctx p)
                       [c (tries ctx cid cat p)]))))
        cats))

(defn- baby? [ctx cid cat {:keys [g ll]} size]
  (and (pos? (long size))
       (<= (roll (:t ctx) cid [cat g :baby] ll) baby-chance)))

(defn- born [ctx cid cat tr size]
  (let [{:keys [g ll kind pos]} tr t (:t ctx)
        yaw (float (* 360.0 (roll t cid [cat g :yaw] ll)))]
    (mobs/natural-mob (:type kind) pos [t cid cat g ll] t (:dim ctx)
                      yaw (baby? ctx cid cat tr size))))

(defn- added [acc ctx cat mob]
  (-> acc
      (update :counts add-mob
              (near ctx (chunk/pos-chunk (:pos mob))) cat)
      (update :boxes conj (box-of mob))
      (update :mobs conj mob)))

(defn- free-of? [acc tr]
  (not (some #(phys/joined? (:box tr) %) (:boxes acc))))

(defn- spawned [acc ctx cid cat tr]
  (let [size (long (get-in acc [:sizes (:g tr)] 0))
        mob (born ctx cid cat tr size)]
    (-> (added acc ctx cat mob)
        (assoc-in [:sizes (:g tr)] (inc size))
        (update :cluster inc))))

(defn- settle-tries [acc ctx cid cat trs]
  (loop [[tr & more] trs acc (assoc acc :cluster 0 :sizes {})]
    (cond (or (nil? tr) (= ::abort tr)) acc
          (not (free-of? acc tr)) (recur more acc)
          :else (let [acc (spawned acc ctx cid cat tr)]
                  (if (>= (long (:cluster acc)) cluster)
                    acc
                    (recur more acc))))))

(defn- settle [ctx acc [cid plans]]
  (reduce (fn [acc [[cat :as c] trs]]
            (if (local-ok? ctx (:counts acc) cid c)
              (settle-tries acc ctx cid cat trs)
              acc))
          acc plans))

(defn- spawns-in? [ctx ticking cid]
  (and (contains? ticking cid) (contains? (:active ctx) cid)
       (some #(close? cid (nth % 1)) (:players ctx))))

(defn- candidates [ctx]
  (let [ticking (:ticking ctx) t (long (:t ctx))]
    (->> (filterv #(spawns-in? ctx ticking %) (:counted ctx))
         (sort-by #(roll t % :shuffle 0))
         vec)))

(defn- kinds [ctx]
  (into {} (for [es (vals (get-in ctx [:biome :spawners]))
                 {t :type} es
                 :when (mobs/mob-type? t)]
             [t (spawn/kind
                  t (get-in mobs/types [t :spawns-on]))])))

(defn- base [w]
  (let [dim (:dim w :overworld)]
    {:t (:tick w) :dim dim :chunks (:chunks w)
     :min-y (chunk/level-min-y w) :active (areas/active-chunks w)
     :ticking (areas/ticking-chunks w)
     :counted (counted w) :players (spawners w)
     :biome (biome-of w) :peaceful? (zero? (difficulty/id w))
     :despawn (into {} (map (fn [[c f]] [c (:despawn f)]))
                    (spawn/categories))
     :spawn (when (= dim (:world-spawn-dimension w :overworld))
              (:world-spawn w))}))

(defn- context [w]
  (let [ctx (base w)
        ctx (assoc ctx :kinds (kinds ctx)
                   :blockers (delay (blockers w)))]
    (assoc ctx :counts (delay (mob-counts w ctx)))))

(def ^:private ^:const chunk-leaf 16)

(def ^:private ^:const chunk-threshold 64)

(defn- spawned-in [ctx cats]
  (let [cids (candidates ctx)
        job #(plan ctx cats %)
        plans (deltas/pmapv job cids chunk-leaf chunk-threshold)
        acc {:counts @(:counts ctx) :boxes [] :mobs []}
        pairs (map vector cids plans)]
    (:mobs (reduce #(settle ctx %1 %2) acc pairs))))

(defn- wanted [w]
  (not-empty (filterv #(wanted? w %) (spawn/categories))))

(defn- spawn-deltas [w]
  (when-let [cs (wanted w)]
    (let [ctx (context w)
          cats (filterv #(global-ok? ctx %) cs)]
      (when (seq cats)
        (mapv (fn [m] [:spawn-entity m]) (spawned-in ctx cats))))))

(defn natural-spawns
  "Returns the mobs that spawn naturally around the players of the
  level this tick, while the spawn_mobs rule holds."
  {:wake {:types #{:player}}}
  [world _]
  (deltas/of-vec
    (when (get-in world [:rules :spawn-mobs] true)
      (spawn-deltas world))))
