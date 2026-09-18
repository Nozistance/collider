(ns collider.game.systems.projectiles
  "Thrown snowballs, eggs, pearls and potions, and lingering clouds."
  (:require [collider.data :as data]
            [collider.game.entity :as entity]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.blocks.reach :as reach]
            [collider.game.systems.damage :as damage]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def ^:private ^:const half 0.125)

(def ^:private ^:const height 0.25)

(def ^:private ^:const air-drag 0.99)

(def ^:private ^:const water-drag 0.8)

(def ^:private ^:const gravity 0.03)

(def ^:private ^:const potion-gravity 0.05)

(def ^:private ^:const inaccuracy 0.0172275)

(def ^:private ^:const eye-drop 0.1)

(def ^:private ^:const eye-height 1.62)

(def ^:private ^:const below-world (- chunk/min-y 64.0))

(def ^:private ^:const base-potion-color -13083194)

(def ^:private ^:const splash-range-sq 16.0)

(def ^:private ^:const pearl-damage 5.0)

(def ^:private ^:const cloud-height 0.5)

(def ^:private ^:const cloud-min-radius 0.5)

(def ^:private ^:const cloud-period 5)

(def ^:private ^:const cloud-reapply 20)

(def throwables
  "Shoot power and pitch offset of every item thrown by hand."
  {:snowball         {:power 1.5 :offset 0.0
                      :sound :snowball/throw}
   :egg              {:power 1.5 :offset 0.0
                      :sound :egg/throw}
   :ender-pearl      {:power 1.5 :offset 0.0
                      :sound :ender-pearl/throw}
   :splash-potion    {:power 0.5 :offset -20.0
                      :sound :splash-potion/throw}
   :lingering-potion {:power 0.5 :offset -20.0
                      :sound :lingering-potion/throw}})

(def ^:private potion-types #{:splash-potion :lingering-potion})

(defn- contents [stack]
  (get-in stack [:components :potion-contents]))

(defn- water-potion? [stack]
  (let [c (contents stack)]
    (and (= :water (:potion c)) (empty? (:custom-effects c)))))

(defn- brewed [c]
  (get (data/potions) (:potion c)))

(defn- all-effects [c]
  (concat (brewed c) (:custom-effects c)))

(defn- visible? [e]
  (get-in e [:details :show-particles] true))

(defn- amplifier ^long [e]
  (long (or (:amplifier e) (get-in e [:details :amplifier]) 0)))

(defn- effect-color ^long [e]
  (long (:color (get (data/mob-effects) (:effect e)) 0)))

(defn- channel ^long [rows ^long shift ^long weight]
  (quot (reduce (fn [^long a e]
                  (+ a (* (inc (amplifier e))
                          (bit-and (bit-shift-right (effect-color e)
                                                    shift)
                                   0xFF))))
                0 rows)
        weight))

(defn- blend
  "Returns the amplifier-weighted mean of the effect colours.
  Returns nil when no effect is visible."
  [effects]
  (let [rows (filterv visible? effects)
        w (reduce (fn [^long a e] (+ a (inc (amplifier e)))) 0 rows)]
    (when (pos? w)
      (bit-or -16777216 (bit-shift-left (channel rows 16 w) 16)
              (bit-shift-left (channel rows 8 w) 8)
              (channel rows 0 w)))))

(defn potion-color
  "Returns the colour the client paints the splash and the cloud with."
  ^long [stack]
  (let [c (contents stack)]
    (long (or (:custom-color c) (blend (all-effects c))
              base-potion-color))))

(defn has-effects? [stack]
  (let [c (contents stack)]
    (boolean (or (seq (:custom-effects c)) (seq (brewed c))))))

(defn- instant? [e]
  (:instant? (get (data/mob-effects) (:effect e))))

(defn has-instant-effects?
  "Returns true when the potion itself acts at once. Custom effects do
  not count."
  [stack]
  (boolean (some instant? (brewed (contents stack)))))

(defn- break-event ^long [stack]
  (if (has-instant-effects? stack) 2007 2002))

(defn- triangle ^double [world eid k ^double dev]
  (let [t (:tick world)]
    (* dev (- (random/of-key t eid [k 0])
              (random/of-key t eid [k 1])))))

(defn- aim [^double yaw ^double pitch ^double off]
  (let [y (Math/toRadians yaw) p (Math/toRadians pitch)]
    [(* (- (Math/sin y)) (Math/cos p))
     (- (Math/sin (Math/toRadians (+ pitch off))))
     (* (Math/cos y) (Math/cos p))]))

(defn- normalized [[x y z]]
  (let [x (double x) y (double y) z (double z)
        l (Math/sqrt (+ (* x x) (* y y) (* z z)))]
    (if (< l 1.0E-4) [0.0 0.0 0.0] [(/ x l) (/ y l) (/ z l)])))

(defn- shot-vel [world eid dir ^double power]
  (let [[x y z] (normalized dir)]
    [(* power (+ (double x) (triangle world eid :x inaccuracy)))
     (* power (+ (double y) (triangle world eid :y inaccuracy)))
     (* power (+ (double z) (triangle world eid :z inaccuracy)))]))

(defn- carried
  "Returns vel plus the motion of the thrower. The thrower's own fall
  speed is left out while it stands on the ground."
  [e vel]
  (let [m (or (:client-vel e) [0.0 0.0 0.0])]
    (v/+ vel [(v/x m) (if (:on-ground e) 0.0 (v/y m)) (v/z m)])))

(defn- facing [vel]
  (let [x (v/x vel) y (v/y vel) z (v/z vel)
        d (Math/sqrt (+ (* x x) (* z z)))]
    [(Math/toDegrees (Math/atan2 x z))
     (Math/toDegrees (Math/atan2 y d))]))

(defn- thrown [world eid e stack]
  (let [{:keys [power offset]} (throwables (:item stack))
        p (:pos e)
        dir (aim (double (:yaw e 0.0)) (double (:pitch e 0.0))
                 (double offset))
        vel (carried e (shot-vel world eid dir (double power)))
        [yaw pitch] (facing vel)]
    {:type  (:item stack) :owner eid :age 0 :left-owner? false
     :pos   [(v/x p) (- (+ (v/y p) eye-height) eye-drop) (v/z p)]
     :vel   vel :yaw yaw :pitch pitch :on-ground false
     :stack (assoc stack :count 1)}))

(defn- throw-sound [world eid e stack]
  (when-let [snd (:sound (throwables (:item stack)))]
    (let [r (random/of-key (:tick world) eid :throw)
          pitch (/ 0.4 (+ (* 0.4 r) 0.8))]
      [(out/all (out/sound snd (:pos e) 0.5 pitch))])))

(defn- spent-deltas [eid e stack]
  (when-not (state/infinite-materials? e)
    (let [n (dec (long (:count stack 1)))]
      [[:set-slot eid (state/hand-slot e :main)
        (when (pos? n) (assoc stack :count n))]])))

(defn throw-deltas
  "Returns the deltas of a player throwing what its main hand holds."
  [world eid e]
  (let [stack (state/hand-stack e :main)]
    (when (throwables (:item stack))
      (concat (throw-sound world eid e stack)
              [[:spawn-entity (thrown world eid e stack)]
               [:award eid (keyword "used" (name (:item stack))) 1]]
              (spent-deltas eid e stack)
              (state/cooldown-deltas eid e (:item stack)
                                     (:tick world))))))

(defn- box-of [pos ^double w ^double h]
  (let [x (v/x pos) y (v/y pos) z (v/z pos)]
    [(- x w) y (- z w) (+ x w) (+ y h) (+ z w)]))

(defn- inflated [b ^double dx ^double dy ^double dz]
  [(- (double (b 0)) dx) (- (double (b 1)) dy) (- (double (b 2)) dz)
   (+ (double (b 3)) dx) (+ (double (b 4)) dy) (+ (double (b 5)) dz)])

(defn- swept [b d ^double m]
  (let [lo (fn ^double [^long i ^double c]
             (+ (double (b i)) (min c 0.0) (- m)))
        hi (fn ^double [^long i ^double c]
             (+ (double (b i)) (max c 0.0) m))
        dx (v/x d) dy (v/y d) dz (v/z d)]
    [(lo 0 dx) (lo 1 dy) (lo 2 dz) (hi 3 dx) (hi 4 dy) (hi 5 dz)]))

(defn- axis-overlaps?
  [^double lo1 ^double hi1 ^double lo2 ^double hi2]
  (and (< lo1 hi2) (> hi1 lo2)))

(defn- overlaps? [a b]
  (and (axis-overlaps? (a 0) (a 3) (b 0) (b 3))
       (axis-overlaps? (a 1) (a 4) (b 1) (b 4))
       (axis-overlaps? (a 2) (a 5) (b 2) (b 5))))

(defn- hittable? [e]
  (or (= :player (:type e)) (mobs/mob-type? (:type e))))

(defn- target-box [e]
  (if (= :player (:type e))
    (box-of (:pos e) 0.3 1.8)
    (let [{:keys [half height]} (mobs/types (:type e))]
      (box-of (:pos e) (double (or half 0.45))
              (double (or height 1.3))))))

(defn- nearer [best hit tail]
  (if (and hit (or (nil? best)
                   (< (double (hit 0)) (double (best 0)))))
    (into [(hit 0)] (cons (hit 1) tail))
    best))

(defn- cell-range [^double a ^double b]
  (range (long (Math/floor (Math/min a b)))
         (inc (long (Math/floor (Math/max a b))))))

(defn- cells [from to]
  (for [x (cell-range (v/x from) (v/x to))
        y (cell-range (v/y from) (v/y to))
        z (cell-range (v/z from) (v/z to))]
    [x y z]))

(defn- abs-box [[x y z] b]
  (let [c (fn ^double [^long i ^long o]
            (+ (double o) (/ (double (nth b i)) 16.0)))]
    [(c 0 x) (c 1 y) (c 2 z) (c 3 x) (c 4 y) (c 5 z)]))

(defn- cell-clip [chunks cell from d]
  (let [st (chunk/chunks-get-block chunks cell)]
    (when (block/solid? st)
      (reduce (fn [best b]
                (nearer best
                        (reach/box-entry from d (abs-box cell b))
                        nil))
              nil (block/collision-boxes st)))))

(defn- block-clip [world from d]
  (reduce (fn [best cell]
            (nearer best
                    (when (chunk/in-range? (nth cell 1))
                      (cell-clip (:chunks world) cell from d))
                    [cell]))
          nil (cells from (v/+ from d))))

(defn- skip? [eid e oid o]
  (or (= (long oid) (long eid))
      (not (hittable? o))
      (and (not (:left-owner? e))
           (= (long oid) (long (:owner e -1))))))

(defn- entity-clip [world eid e from d]
  (reduce (fn [best [oid o]]
            (if (skip? eid e oid o)
              best
              (nearer best
                      (reach/box-entry from d (target-box o))
                      [oid])))
          nil (sort-by key (:entities world))))

(defn- clip
  "Returns the first thing the move from the projectile meets."
  [world eid e d]
  (let [from (:pos e)
        b (block-clip world from d)
        x (entity-clip world eid e from d)]
    (cond
      (and x (or (nil? b) (<= (double (x 0)) (double (b 0)))))
      {:kind :entity :t (x 0) :target (nth x 2)}
      b {:kind :block :t (b 0) :face (b 1) :cell (nth b 2)})))

(defn- left-owner? [world e d]
  (or (boolean (:left-owner? e))
      (if-let [o (get-in world [:entities (:owner e)])]
        (not (overlaps? (swept (box-of (:pos e) half height) d 1.0)
                        (target-box o)))
        true)))

(defn- drift [world e]
  (let [g (if (potion-types (:type e)) potion-gravity gravity)
        vel (:vel e)
        wet? (pos? (liquid/fluid-height (:chunks world) (:pos e)
                                        half height :water))
        k (if wet? water-drag air-drag)]
    [(* k (v/x vel)) (* k (- (v/y vel) g)) (* k (v/z vel))]))

(defn- point [from d ^double t]
  [(+ (v/x from) (* t (v/x d))) (+ (v/y from) (* t (v/y d)))
   (+ (v/z from) (* t (v/z d)))])

(defn- wrapped ^double [^double old ^double new]
  (cond (< (- new old) -180.0) (recur (- old 360.0) new)
        (>= (- new old) 180.0) (recur (+ old 360.0) new)
        :else old))

(defn- lerp-rotation ^double [^double old ^double new]
  (let [old (wrapped old new)]
    (+ old (* 0.2 (- new old)))))

(defn- moved [e at d left?]
  (let [[yaw pitch] (facing d)]
    {:pos at :vel d :left-owner? left?
     :yaw (lerp-rotation (double (:yaw e 0.0)) yaw)
     :pitch (lerp-rotation (double (:pitch e 0.0)) pitch)
     :age (inc (long (:age e 0)))}))

(defn- pearl-deltas [world e]
  (let [oid (:owner e) o (get-in world [:entities oid]) p (:pos e)]
    (when (and o (pos? (double (:health o 0.0))) (not (:sleeping o)))
      (concat [[:teleport oid [(v/x p) (v/y p) (v/z p)]]
               (out/to oid (out/teleport [(v/x p) (v/y p) (v/z p)]
                                         (:yaw o 0.0)
                                         (:pitch o 0.0)))]
              (when-not (damage/creative-proof? o)
                [[:damage oid pearl-damage]])
              [(out/all (out/sound :player/teleport p 1.0 1.0))]))))

(defn- dowse-cells [hit]
  (let [cell (:cell hit) at (mapv + cell (dir/offset (:face hit)))]
    (distinct (into [at cell] (map #(mapv + at (dir/offset %)))
                    dir/horizontal))))

(defn- fire-at? [chunks p]
  (and (chunk/in-range? (nth p 1))
       (block/fire? (chunk/chunks-get-block chunks p))))

(defn- fire-out-deltas [world fires]
  (let [chunks (:chunks world)]
    (when (seq fires)
      (conj (mapv #(out/all (out/break-effect
                              % (chunk/chunks-get-block chunks %)))
                  fires)
            [:set-blocks (mapv (fn [p] [p 0]) fires)]))))

(defn- dowse-deltas
  "Returns the deltas of dowsing fire around the hit. Actual fire is
  destroyed, while a candle or a campfire only goes out."
  [world hit]
  (let [cells (filterv #(chunk/in-range? (nth % 1)) (dowse-cells hit))
        fires (filterv #(fire-at? (:chunks world) %) cells)]
    (into (vec (fire-out-deltas world fires))
          (mapcat #(edit/dowse-deltas world %))
          (remove (set fires) cells))))

(defn- doused-deltas
  "Returns the deltas of a water splash putting out entities it soaks."
  [world at]
  (let [box (inflated (box-of at half height) 4.0 2.0 4.0)]
    (for [[oid o] (sort-by key (:entities world))
          :when (and (hittable? o) (pos? (long (:fire o 0)))
                     (overlaps? box (target-box o))
                     (< (v/dist3-sq at (:pos o)) splash-range-sq))]
      [:merge-entity oid {:fire 0 :burning? false}])))

(defn- cloud-spec [e at]
  (let [stack (:stack e)]
    {:type     :area-effect-cloud :pos at :vel [0.0 0.0 0.0]
     :yaw      0.0 :pitch 0.0 :on-ground false :owner (:owner e)
     :radius   3.0 :stack stack :color (potion-color stack)
     :waiting? false :age 0 :duration 600 :wait-time 10
     :radius-per-tick (/ -3.0 600.0) :radius-on-use -0.5
     :victims {}}))

(defn- potion-deltas [world e at hit]
  (let [stack (:stack e) water? (water-potion? stack)]
    (concat (when (and water? (= :block (:kind hit)))
              (dowse-deltas world hit))
            (when water? (doused-deltas world at))
            (when (and (not water?) (has-effects? stack)
                       (= :lingering-potion (:type e)))
              [[:spawn-entity (cloud-spec e at)]])
            [(out/all (out/level-event
                        (break-event stack)
                        (mapv #(long (Math/floor (double %))) at)
                        (potion-color stack)))])))

(defn- hit-deltas [world eid e at hit]
  (case (:type e)
    (:snowball :egg) [(out/all (out/status eid :break))]
    :ender-pearl (pearl-deltas world e)
    (potion-deltas world (assoc e :pos at) at hit)))

(defn- step-deltas [world eid e]
  (let [d (drift world e)
        left? (left-owner? world e d)
        hit (clip world eid (assoc e :left-owner? left?) d)
        at (if hit (point (:pos e) d (:t hit)) (v/+ (:pos e) d))]
    (cond
      hit (into [[:remove-entity eid]]
                (hit-deltas world eid e at hit))
      (< (v/y at) below-world) [[:remove-entity eid]]
      :else [[:merge-entity eid (moved e at d left?)]])))

(defn- cloud-box [e ^double r]
  (box-of (:pos e) r cloud-height))

(defn- touched [world e ^double r ^long age]
  (let [box (cloud-box e r)]
    (for [[oid o] (sort-by key (:entities world))
          :when (and (hittable? o) (not (contains? (:victims e) oid))
                     (overlaps? box (target-box o))
                     (<= (v/dist-sq (:pos e) (:pos o)) (* r r)))]
      [oid (+ age cloud-reapply)])))

(defn- kept-victims [e ^long age]
  (into {} (remove (fn [[_ at]] (>= age (long at)))) (:victims e)))

(defn- cloud-contact [world e ^long age ^double r]
  (cond
    (pos? (rem age cloud-period)) [r (:victims e)]
    (not (has-effects? (:stack e))) [r {}]
    :else
    (let [left (kept-victims e age)
          taken (touched world (assoc e :victims left) r age)]
      [(+ r (* (count taken) (double (:radius-on-use e))))
       (into left taken)])))

(defn- cloud-active [world eid e ^long age]
  (let [r (+ (double (:radius e)) (double (:radius-per-tick e)))]
    (if (< r cloud-min-radius)
      [[:remove-entity eid]]
      (let [[r' victims] (cloud-contact world e age r)]
        (if (< r' cloud-min-radius)
          [[:remove-entity eid]]
          [[:merge-entity eid {:age age :waiting? false :radius r'
                               :victims victims}]])))))

(defn- cloud-deltas [world eid e]
  (let [age (inc (long (:age e 0))) wait (long (:wait-time e))]
    (cond
      (>= (- age wait) (long (:duration e))) [[:remove-entity eid]]
      (< age wait) [[:merge-entity eid {:age age :waiting? true}]]
      :else (cloud-active world eid e age))))

(defn- live? [active kinds [_ e]]
  (and (kinds (:type e)) (state/active-at? active (:pos e))))

(defn- entries [world kinds]
  (let [active (state/active-chunks world)]
    (into [] (filter (partial live? active kinds))
          (sort-by key (:entities world)))))

(defn projectiles
  "Returns the flight and the hits of the thrown things.
  Also returns the life of the lingering clouds."
  [world _d]
  (-> (mapv (fn [[eid e]] #(step-deltas world eid e))
            (entries world entity/thrown-types))
      (into (map (fn [[eid e]] #(cloud-deltas world eid e)))
            (entries world #{:area-effect-cloud}))))

(defn first-step
  "Returns the flight of the things thrown this tick.
  They move at once in the tick of their throw."
  [world _d]
  (into []
        (comp (filter (fn [[_ e]] (zero? (long (:age e 0)))))
              (mapcat (fn [[eid e]] (step-deltas world eid e))))
        (entries world entity/thrown-types)))
