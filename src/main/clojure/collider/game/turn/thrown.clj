(ns collider.game.turn.thrown
  "The turns of thrown snowballs, eggs, pearls, potions and bottles
  o' enchanting, and of lingering clouds."
  (:require [collider.data.long-map :as lm]
            [collider.data :as data]
            [collider.game.apply :as apply]
            [collider.game.changes :as changes]
            [collider.game.delta :as delta]
            [collider.game.deltas :as deltas]
            [collider.game.effect :as effect]
            [collider.game.effect.account :as account]
            [collider.game.entity :as entity]
            [collider.game.entity.hurt :as hurt]
            [collider.game.entity.size :as size]
            [collider.game.mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.areas :as areas]
            [collider.game.player :as player]
            [collider.game.potion :as potion]
            [collider.game.projectile :as projectile]
            [collider.game.stack :as stack]
            [collider.game.blast :as blast]
            [collider.game.turn.arrow :as arrow]
            [collider.game.turn.overlay :as overlay]
            [collider.game.turn.rocket :as rocket]
            [collider.num :as num]
            [collider.parallel :as par]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ^:private ^:const air-drag (double (float 0.99)))

(def ^:private ^:const water-drag (double (float 0.8)))

(def ^:private gravity
  {:splash-potion 0.05 :lingering-potion 0.05
   :experience-bottle 0.07})

(def ^:private ^:const inaccuracy 0.0172275)

(def ^:const eye-drop (double (float 0.1)))

(def ^:private ^:const bottle-xp-color -13083194)

(def ^:private ^:const splash-range-sq 16.0)

(def ^:private ^:const pearl-damage 5.0)

(def ^:private ^:const cloud-min-radius 0.5)

(def ^:private ^:const cloud-period 5)

(def ^:private ^:const cloud-reapply 20)

(def throwables
  "Shoot power and pitch offset of every item thrown by hand."
  {:snowball         {:power 1.5 :offset 0.0
                      :sound :snowball/throw}
   :egg              {:power 1.5 :offset 0.0
                      :sound :egg/throw}
   :blue-egg         {:power 1.5 :offset 0.0
                      :sound :egg/throw :entity :egg}
   :brown-egg        {:power 1.5 :offset 0.0
                      :sound :egg/throw :entity :egg}
   :wind-charge      {:power 1.5 :offset 0.0
                      :sound :wind-charge/throw :drop 0.0}
   :ender-pearl      {:power 1.5 :offset 0.0
                      :sound :ender-pearl/throw}
   :splash-potion    {:power 0.5 :offset -20.0
                      :sound :splash-potion/throw}
   :lingering-potion {:power 0.5 :offset -20.0
                      :sound :lingering-potion/throw}
   :experience-bottle {:power 0.7 :offset -20.0
                       :sound :experience-bottle/throw}})

(defn- break-event [stack]
  (if (potion/has-instant-effects? stack)
    :particles-instant-potion-splash
    :particles-spell-potion-splash))

(defn- triangle ^double [world eid k ^double dev]
  (let [t (:tick world)]
    (* dev (- (random/of-key t eid [k 0])
              (random/of-key t eid [k 1])))))

(defn- f32 ^double [^double a] (double (unchecked-float a)))

(defn- f* ^double [^double a ^double b] (f32 (* a b)))

(defn- rad ^double [^double deg] (f* (f32 deg) num/deg->rad))

(defn- aim [^double yaw ^double pitch ^double off]
  (let [y (rad yaw) p (rad pitch)
        up (rad (f32 (+ (f32 pitch) off)))]
    [(f* (- (v/sin y)) (v/cos p)) (- (v/sin up))
     (f* (v/cos y) (v/cos p))]))

(defn- normalized [[x y z]]
  (let [x (double x) y (double y) z (double z)
        l (Math/sqrt (+ (* x x) (* y y) (* z z)))]
    (if (< l 1.0E-5) [0.0 0.0 0.0] [(/ x l) (/ y l) (/ z l)])))

(defn- shot-vel [world eid dir ^double power]
  (let [[x y z] (normalized dir)
        power (f32 power)]
    [(* power (+ (double x) (triangle world eid :x inaccuracy)))
     (* power (+ (double y) (triangle world eid :y inaccuracy)))
     (* power (+ (double z) (triangle world eid :z inaccuracy)))]))

(defn- carried [e vel]
  (let [m (or (:client-vel e) [0.0 0.0 0.0])]
    (v/add vel [(v/x m) (if (:on-ground e) 0.0 (v/y m)) (v/z m)])))

(defn- eye-y ^double [e] (f32 (entity/eye-height e)))

(defn shot
  "Returns the position, motion, yaw and pitch of a projectile shot by
  e of eid along dir at power, set drop below the eyes. It faces its
  own motion."
  [world eid e dir power drop]
  (let [p (:pos e)
        vel (shot-vel world eid dir (double power))
        [yaw pitch] (projectile/facing vel)]
    {:pos [(v/x p) (- (+ (v/y p) (eye-y e)) (double drop)) (v/z p)]
     :vel vel :yaw yaw :pitch pitch}))

(defn launched
  "Returns the shot of e of eid at power, aimed offset degrees up and
  set drop below the eyes. It carries the motion of e."
  [world eid e power offset drop]
  (let [dir (aim (double (:yaw e 0.0)) (double (:pitch e 0.0))
                 (double offset))]
    (update (shot world eid e dir power drop) :vel #(carried e %))))

(defn- thrown [world eid e stack]
  (let [{:keys [power offset entity drop]
         :or {drop eye-drop}} (throwables (:item stack))]
    (merge {:type (or entity (:item stack)) :owner eid :age 0
            :left-owner? false :on-ground false
            :stack (assoc stack :count 1)}
           (launched world eid e power offset drop))))

(defn- throw-sound [world eid e stack]
  (when-let [snd (:sound (throwables (:item stack)))]
    (let [r (f32 (random/of-key (:tick world) eid :throw))
          d (f32 (+ (f* r (f32 0.4)) (f32 0.8)))
          pitch (f32 (/ (f32 0.4) d))]
      [(out/all (out/sound snd (:pos e) 0.5 pitch))])))

(defn- spent-deltas [eid e stack]
  (when-not (player/infinite-materials? e)
    (let [n (dec (long (:count stack 1)))]
      [[:set-slot eid (player/hand-slot e (:use-hand e))
        (when (pos? n) (assoc stack :count n))]])))

(defn throw-deltas
  "Returns the deltas of a player throwing what its hand holds, or
  nil when the hand holds nothing to throw."
  [world eid e]
  (let [stack (player/hand-stack e (:use-hand e)) item (:item stack)]
    (when (throwables item)
      (concat (throw-sound world eid e stack)
              [[:spawn-entity (thrown world eid e stack)]
               [:award eid (keyword "used" (name item)) 1]]
              (spent-deltas eid e stack)
              (player/cooldown-deltas eid e item (:tick world))))))

(defn- submerged? [world e]
  (let [chunks (:chunks world)]
    (pos? (let [[w h] (entity/box e)]
            (liquid/fluid-height chunks (:pos e) w h :water)))))

(defn- wind? [e] (= :wind-charge (:type e)))

(defn- falling-drift [world e]
  (let [g (double (gravity (:type e) 0.03))
        vel (:vel e)
        k (if (submerged? world e) water-drag air-drag)]
    [(* k (v/x vel)) (* k (- (v/y vel) g)) (* k (v/z vel))]))

(defn- drift [world e]
  (if (wind? e)
    (:vel e)
    (falling-drift world e)))

(def ^:private ^:const rad->deg
  (double (unchecked-float (/ (float 180.0) (float Math/PI)))))

(defn- deg ^double [^double rad] (f32 (* rad rad->deg)))

(defn- turned
  "Returns the yaw and the pitch of e turned a fifth of the way
  towards its motion d, as a hurting projectile turns."
  [e d]
  (let [x (v/x d) y (v/y d) z (v/z d)
        h (Math/sqrt (+ (* x x) (* z z)))]
    (if (zero? (+ (* h h) (* y y)))
      [(:yaw e 0.0) (:pitch e 0.0)]
      (let [yaw (f32 (+ (deg (num/atan2 z x)) 90.0))
            pitch (f32 (- (deg (num/atan2 h y)) 90.0))]
        [(projectile/lerp-rotation (double (:yaw e 0.0)) yaw)
         (projectile/lerp-rotation (double (:pitch e 0.0)) pitch)]))))

(defn- moved [e at d left?]
  (let [[yaw pitch] (if (wind? e) (turned e d) (projectile/rotated e d))]
    {:pos at :vel d :left-owner? left? :yaw yaw :pitch pitch
     :age (inc (long (:age e 0)))}))

(defn- teleport-packet [at o]
  (out/teleport at (:yaw o 0.0) (:pitch o 0.0)))

(defn- pearl-deltas [world e]
  (let [oid (:owner e) o (get-in world [:entities oid]) p (:pos e)
        at [(v/x p) (v/y p) (v/z p)]]
    (when (and o (pos? (double (:health o 0.0))) (not (:sleeping o)))
      (concat [[:teleport oid at]
               (out/to oid (teleport-packet at o))]
              (when (:impulse-at o)
                [[:merge-entity oid
                  {:impulse-at nil :impulse-grace 0}]])
              (hurt/damage-deltas
                world oid o pearl-damage {:type :ender-pearl})
              [(out/all (out/sound :player/teleport p 1.0 1.0))]))))

(defn- dowse-cells [hit]
  (let [cell (:cell hit) at (mapv + cell (dir/offset (:face hit)))]
    (distinct (into [at cell] (map #(mapv + at (dir/offset %)))
                    dir/horizontal))))

(defn- fire-at? [chunks p]
  (and (chunk/in-range? (nth p 1))
       (block/fire? (chunk/at chunks p))))

(defn- break-packet [chunks p]
  (out/all (out/break-effect p (chunk/at chunks p))))

(defn- fire-out-deltas [world fires]
  (let [chunks (:chunks world)]
    (when (seq fires)
      (into (mapv #(break-packet chunks %) fires)
            (changes/set-deltas world (mapv (fn [p] [p 0]) fires))))))

(defn- dowse-deltas [world hit]
  (let [loaded? #(chunk/in-range? (nth % 1))
        cells (filterv loaded? (dowse-cells hit))
        fires (filterv #(fire-at? (:chunks world) %) cells)]
    (into (vec (fire-out-deltas world fires))
          (mapcat #(changes/dowse-deltas world %))
          (remove (set fires) cells))))

(defn- doused-deltas [world e at]
  (let [box (projectile/inflated (projectile/body-box e at) 4.0 2.0 4.0)]
    (for [[oid o] (par/keyed (:entities world))
          :when (and (projectile/hittable? o) (pos? (long (:fire o 0)))
                     (projectile/overlaps? box (projectile/target-box o))
                     (< (v/dist-sq at (:pos o)) splash-range-sq))
          d [[:merge-entity oid {:fire 0 :burning? false}]
             (hurt/put-out-sound world oid o)]]
      d)))

(defn- cloud-spec [e at]
  (let [stack (:stack e)]
    {:type     :area-effect-cloud :pos at :vel [0.0 0.0 0.0]
     :yaw      0.0 :pitch 0.0 :on-ground false :owner (:owner e)
     :radius   3.0 :stack stack :color (potion/color stack)
     :waiting? false :age 0 :duration 600 :wait-time 10
     :radius-per-tick (num/f32 (/ -3.0 600.0)) :radius-on-use -0.5
     :victims {}}))

(defn- floored [at] (mapv #(long (Math/floor (double %))) at))

(defn- magic-from [world eid e at]
  (let [o (some->> (:owner e) (get (:entities world)))]
    {:type :indirect-magic :cause (if o (:owner e) eid) :direct eid
     :from at :player? (entity/player? o) :attacker (or o e)
     :direct-attacker e}))

(defn- affected-in? [box o]
  (and (account/living? o) (pos? (double (:health o 0.0)))
       (not (game-mode/spectator? o))
       (projectile/overlaps? box (projectile/target-box o))))

(defn- reported [world oid o ds]
  (let [now (hurt/hurt-now world oid o ds)]
    (into (vec ds) (hurt/report-deltas world oid now))))

(defn- splash-timed [i ^double scale ^double s]
  (-> (effect/timed i #(long (+ (* scale (double %) s) 0.5)))
      (assoc :icon? (:visible? i))))

(defn- splash-lay [scale s src acc [k i]]
  (if (potion/instant? k)
    (account/instant acc k (:amplifier i) scale src)
    (let [i (splash-timed i scale s)]
      (if (effect/ends-within? i 20) acc (account/land acc k i)))))

(defn- splashed-deltas [world oid o stack scale src]
  (let [s (potion/duration-scale stack)
        acc (reduce #(splash-lay scale s src %1 %2)
                    (assoc (account/account oid o) :world world)
                    (potion/instances stack))]
    (reported world oid o (account/deltas acc o))))

(defn- splash-gap ^double [pb o ^double m]
  (let [b (projectile/inflated (projectile/target-box o) m m m)]
    (projectile/gap-sq pb b)))

(defn- splash-deltas [world eid e d at]
  (let [pb (projectile/body-box e at)
        area (projectile/inflated pb 4.0 2.0 4.0)
        m (projectile/margin e)
        src (magic-from world eid (assoc e :vel d) at)]
    (for [[oid o] (par/keyed (:entities world))
          :when (affected-in? area o)
          :let [g (splash-gap pb o m)]
          :when (< g splash-range-sq)
          :let [k (- 1.0 (/ (Math/sqrt g) 4.0))]
          ds (splashed-deltas world oid o (:stack e) k src)]
      ds)))

(defn- potion-deltas [world eid e d at hit]
  (let [stack (:stack e) water? (potion/water? stack)]
    (concat (when (and water? (= :block (:kind hit)))
              (dowse-deltas world hit))
            (when water? (doused-deltas world e at))
            (when (and (not water?) (potion/has-effects? stack)
                       (= :splash-potion (:type e)))
              (splash-deltas world eid e d at))
            (when (and (not water?) (potion/has-effects? stack)
                       (= :lingering-potion (:type e)))
              [[:spawn-entity (cloud-spec e at)]])
            [(out/all (out/level-event
                        (break-event stack)
                        (floored at)
                        (potion/color stack)))])))

(defn- bottle-deltas [world eid d at hit]
  (let [roll #(random/of-key (:tick world) eid [:xp %])
        n (+ 3 (long (* 5.0 (roll 0))) (long (* 5.0 (roll 1))))
        rough (if (= :block (:kind hit))
                (mapv double (dir/offset (:face hit)))
                (mapv - d))]
    [(out/all
       (out/level-event :particles-spell-potion-splash (floored at)
                        bottle-xp-color))
     [:xp-award (mapv double at) n [:bottle eid] rough]]))

(defn- thrown-source [world eid e d]
  (let [o (some->> (:owner e) (get (:entities world)))]
    {:type (if (wind? e) :wind-charge :thrown)
     :cause (when o (:owner e)) :direct eid :along d
     :player? (= :player (:type o)) :attacker o :direct-attacker e}))

(defn- hurt-deltas
  "Returns the deltas of the hurt that thrown e deals to what it hit.
  A snowball deals 3 to a blaze. The hurt knocks along the motion d
  and shows at once."
  [world eid e d hit]
  (when-let [oid (:target hit)]
    (let [o (get-in world [:entities oid])
          blaze? (and (= :snowball (:type e)) (= :blaze (:type o)))
          n (cond (wind? e) 1.0 blaze? 3.0 :else 0.0)
          src (thrown-source world eid e d)]
      (when-let [ds (hurt/damage-deltas world oid o n src)]
        (into ds (hurt/report-deltas
                   world oid (hurt/hurt-now world oid o ds)))))))

(defn- free-at [world [x y z :as pos] w sy shift]
  (let [wd (/ (+ w 1.0E-6) 2.0)
        hd (/ (+ sy 1.0E-6) 2.0)
        box [(- x wd) (- y hd) (- z wd) (+ x wd) (+ y hd) (+ z wd)]
        ctx (phys/context {:type :chicken})
        free (phys/free-position (:chunks world) box pos w sy w y
                                 ctx)]
    (some-> free (update 1 + shift))))

(defn- hatched-at
  "Returns where a baby chicken hatched at pos stands once grown from
  no size to its own, or nil when it does not fit."
  [world pos]
  (let [[half h] (size/box {:type :chicken :baby-until 0})
        w (* 2.0 (double half))
        h (double h)
        pos [(v/x pos) (v/y pos) (v/z pos)]]
    (or (free-at world pos w h (/ (- h) 2.0))
        (free-at world pos w 0.0 1.0E-6))))

(defn- hatch-deltas [world eid e d at]
  (let [t (long (:tick world))]
    (when (< (random/of-key t eid :hatch) 0.125)
      (when-let [p (hatched-at world at)]
        (let [four? (< (random/of-key t eid :hatch-four) 0.03125)
              n (if four? 4 1)
              turned (first (projectile/facing d))
              yaw (projectile/lerp-rotation (double (:yaw e 0.0)) turned)
              look (stack/component (:stack e) :chicken/variant)
              c (assoc (mobs/new-mob :chicken p look t)
                       :baby-until (+ t mobs/baby-start)
                       :yaw yaw :pitch 0.0)]
          (repeat n [:spawn-entity c]))))))

(defn- burst-deltas [world eid e at]
  (blast/deltas world {:center [(v/x at) (v/y at) (v/z at)] :power 1.2
                       :source :wind-charge :by eid
                       :src {:type :wind-charge :cause (:owner e)
                             :direct eid}}))

(defn- burst-at [at hit]
  (if (= :block (:kind hit))
    (v/add at (mapv #(* 0.25 (double %)) (dir/offset (:face hit))))
    at))

(defn- own-hit-deltas [world eid e d at hit]
  (case (:type e)
    :wind-charge
    (concat (hurt-deltas world eid e d hit)
            (burst-deltas world eid e (burst-at at hit)))
    :snowball
    (into (vec (hurt-deltas world eid e d hit))
          [(out/all (out/status eid :break))])
    :egg
    (-> (vec (hurt-deltas world eid e d hit))
        (into (hatch-deltas world eid e d at))
        (conj (out/all (out/status eid :break))))
    :ender-pearl
    (concat (hurt-deltas world eid e d hit) (pearl-deltas world e))
    :experience-bottle (bottle-deltas world eid d at hit)
    (delta/authored
      (potion-deltas world eid (assoc e :pos at) d at hit)
      (delta/entity-author eid e))))

(defn- hit-deltas [world eid e d at hit]
  (concat
    (when (= :block (:kind hit))
      (projectile/block-hit-deltas world e (:cell hit)))
    (own-hit-deltas world eid e d at hit)))

(def ^:private ^:const wind-ceiling (+ chunk/max-y 30))

(defn- wind-gone
  "Returns the deltas of wind charge e that goes before it moves. High
  above the world it bursts, and without its owner it just goes."
  [world eid e]
  (cond
    (> (long (Math/floor (double (v/y (:pos e))))) wind-ceiling)
    (into [[:remove-entity eid]] (burst-deltas world eid e (:pos e)))
    (and (:owner e) (nil? (get-in world [:entities (:owner e)])))
    [[:remove-entity eid]]))

(defn- moving-deltas [world eid e]
  (let [d (drift world e)
        left? (projectile/left-owner? world e d)
        hit (projectile/clip world eid (assoc e :left-owner? left?) d)
        at (if hit (:at hit) (v/add (:pos e) d))]
    (cond
      (< (v/y at) (chunk/void-y world)) [[:remove-entity eid]]
      hit (into [[:remove-entity eid]]
                (hit-deltas world eid e d at hit))
      :else [[:merge-entity eid (moved e at d left?)]])))

(defn- step-deltas [world eid e]
  (cond (= :firework-rocket (:type e)) (rocket/step-deltas world eid e)
        (entity/arrow? e) (arrow/step-deltas world eid e)
        (wind? e) (or (wind-gone world eid e) (moving-deltas world eid e))
        :else (moving-deltas world eid e)))

(defn- cloud-box [e ^double r]
  (projectile/box-of (:pos e) r (size/height :area-effect-cloud)))

(defn- in-cloud [world e ^double r]
  (let [box (cloud-box e r)]
    (for [[oid o] (par/keyed (:entities world))
          :when (affected-in? box o)]
      [oid o])))

(defn- kept-victims [e ^long age]
  (into {} (remove (fn [[_ at]] (>= age (long at)))) (:victims e)))

(defn- clouded-deltas [world eid e oid o]
  (let [src (magic-from world eid e (:pos e))
        acc (-> (account/account oid o) (assoc :world world)
                (account/potion-laid (:stack e) 0.5 src))]
    (reported world oid o (account/deltas acc o))))

(defn- out-of? [e r vs oid o]
  (or (contains? vs oid)
      (> (v/dist-xz-sq (:pos e) (:pos o)) (* (double r) (double r)))))

(defn- shrunk ^double [e r]
  (num/f32 (+ (double r) (double (:radius-on-use e)))))

(defn- used
  "Returns [radius victims deltas] after the cloud lays its potion on
  entity o, or reduced once it has shrunk away."
  [world eid e age [r vs ds :as acc] [oid o]]
  (if (out-of? e r vs oid o)
    acc
    (let [r (shrunk e r)
          acc [r (assoc vs oid (+ (long age) cloud-reapply))
               (into ds (clouded-deltas world eid e oid o))]]
      (cond-> acc (< r cloud-min-radius) reduced))))

(defn- cloud-contact [world eid e age r]
  (cond
    (pos? (rem (long age) cloud-period)) [r (:victims e) nil]
    (not (potion/has-effects? (:stack e))) [r {} nil]
    :else
    (reduce #(used world eid e age %1 %2)
            [r (kept-victims e age) []] (in-cloud world e r))))

(defn- cloud-merge [eid ^long age r victims]
  [[:merge-entity eid {:age age :waiting? false :radius r
                       :victims victims}]])

(defn- cloud-active [world eid e ^long age]
  (let [r (num/f32 (+ (double (:radius e))
                     (double (:radius-per-tick e))))]
    (if (< r cloud-min-radius)
      [[:remove-entity eid]]
      (let [[r' victims ds] (cloud-contact world eid e age r)]
        (into (if (< (double r') cloud-min-radius)
                [[:remove-entity eid]]
                (cloud-merge eid age r' victims))
              ds)))))

(defn- cloud-deltas [world eid e]
  (let [age (inc (long (:age e 0))) wait (long (:wait-time e))]
    (cond
      (< (v/y (:pos e)) (chunk/void-y world)) [[:remove-entity eid]]
      (>= (- age wait) (long (:duration e))) [[:remove-entity eid]]
      (< age wait) [[:merge-entity eid {:age age :waiting? true}]]
      :else (cloud-active world eid e age))))

(def ^:private flying (conj entity/thrown-types :area-effect-cloud))

(defn- cloud? [[_ e]] (= :area-effect-cloud (:type e)))

(defn stepped
  "Returns the hittable entities that ds moved in the turns of the
  tick so far, as they are after them, by eid."
  [world ds]
  (let [es (:entities world) t (:tick world)
        f (fn [m eid eds]
            (let [e (get es eid)]
              (if (and e (projectile/hittable? e))
                (assoc m eid (apply/entity t e eds))
                m)))]
    (reduce-kv f (lm/long-map) (deltas/entities-of ds))))

(defn seen-by
  "Returns entities es as turn eid sees them, moved by the turns of
  lower eid and not yet by the later ones."
  [es after ^long eid]
  (reduce-kv assoc es (lm/range after Long/MIN_VALUE (dec eid))))

(defn written
  "Returns entities m after the deltas ds of tick t."
  [t m ds]
  (let [f (fn [m d]
            (let [e (when (contains? delta/entity-apply (nth d 0))
                      (get m (nth d 1)))]
              (if e (assoc m (nth d 1) (apply/entity t e [d])) m)))]
    (reduce f m ds)))

(defn reach
  "Returns the box [x0 z0 x1 z1] that thrown e sweeps in its move
  this tick."
  [world e]
  (if (entity/arrow? e)
    (arrow/reach e)
    (let [p (:pos e) q (v/add p (drift world e))]
      [(min (v/x p) (v/x q)) (min (v/z p) (v/z q))
       (max (v/x p) (v/x q)) (max (v/z p) (v/z q))])))

(defn ridden
  "Returns [w ds], world w after the turn of thrown eid, e, among
  entities es as it sees them, and its deltas ds."
  [w es eid e]
  (let [tw (assoc (overlay/seen w eid) :entities es)
        ds (step-deltas tw eid e)]
    [(overlay/wrote w eid ds) ds]))

(defn- cloud-turn [w es eid e]
  [w (cloud-deltas (assoc (overlay/seen w eid) :entities es) eid e)])

(defn- turn [t rode [w es after acc] [eid e :as entry]]
  (if-let [ds (get rode eid)]
    [(overlay/wrote w eid ds) (written t es ds) after acc]
    (let [seen (seen-by es after eid)
          [w ds] (if (cloud? entry)
                   (cloud-turn w seen eid e)
                   (ridden w seen eid e))]
      [w (written t es ds) (written t after ds) (into acc ds)])))

(defn- written-world [[w _ _ acc]] [w acc])

(defn- in-order [es]
  (-> (into [] (remove cloud?) es) (into (filter cloud?) es)))

(defn turns
  "Returns world with the blocks that the thrown things in active
  chunks wrote, and their deltas, each in its turn after the turns
  ds of the other bodies. Clouds take their turns last. A hit meets
  bodies where they stand at its turn and sees its own blocks. Those
  in rode by id took their turns among the mobs, with its deltas."
  [world ds rode]
  (let [es (areas/active-of-types world flying)]
    (if (pos? (count es))
      (let [start [world (par/keyed (:entities world))
                   (stepped world ds) []]]
        (->> (in-order es)
             (reduce #(turn (:tick world) rode %1 %2) start)
             written-world))
      [world nil])))
