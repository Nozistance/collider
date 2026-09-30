(ns collider.world.feature.trunk
  "The trunk placers of trees: the logs, and where foliage goes."
  (:require [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.feature.level :as lv]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defn attach
  "A FoliageAttachment: where foliage grows around."
  [p radius-offset double?]
  {:pos p :radius-offset radius-offset :double? double?})

(defn off [p dx dy dz]
  [(+ (long (p 0)) (long dx)) (+ (long (p 1)) (long dy))
   (+ (long (p 2)) (long dz))])

(defn- up [p n] (off p 0 n 0))

(defn valid?
  "TrunkPlacer.validTreePos, with the blocks the placer grows
  through."
  [l tp p]
  (or (lv/valid? l p)
      (boolean (some->> (:can-grow-through tp) (lv/in? (lv/at l p))))))

(defn free? [l tp p]
  (or (valid? l tp p) (block/tagged? (lv/at l p) "logs")))

(defn- axis-set [st axis]
  (if (contains? (block/props-of st) :axis) (lv/with-prop st :axis axis) st))

(defn log
  "TrunkPlacer.placeLog: a log at p when the cell takes one, the
  state turned to axis when given."
  ([l cfg p] (log l cfg p nil))
  ([l cfg p axis]
   (if (valid? l (:trunk-placer cfg) p)
     (let [st (lv/provide l (:trunk-provider cfg) p)]
       (lv/placed l :trunks p (cond-> st axis (axis-set axis))))
     l)))

(defn- log-if-free [l cfg p]
  (if (free? l (:trunk-placer cfg) p) (log l cfg p) l))

(defn below
  "TrunkPlacer.placeBelowTrunkBlock."
  [l cfg p]
  (if-let [st (lv/optional l (:below-trunk-provider cfg) p)]
    (lv/placed l :trunks p st)
    l))

(defn- straight [l cfg h o]
  (let [l (below l cfg (up o -1))]
    [(reduce #(log %1 cfg (up o %2)) l (range h))
     [(attach (up o h) 0 false)]]))

(defn- below-four [l cfg o]
  (let [b (up o -1)]
    (reduce #(below %1 cfg %2) l
            [b (off b 1 0 0) (off b 0 0 1) (off b 1 0 1)])))

(defn- giant-layer [l cfg o h y]
  (let [cells (if (< y (dec h))
                [[0 y 0] [1 y 0] [1 y 1] [0 y 1]]
                [[0 y 0]])]
    (reduce (fn [l [x y z]] (log-if-free l cfg (off o x y z))) l cells)))

(defn- giant [l cfg h o]
  (let [l (below-four l cfg o)]
    [(reduce #(giant-layer %1 cfg o h %2) l (range h))
     [(attach (up o h) 0 true)]]))

(defn- spoke ^long [^double trig ^long b]
  (long (float (+ 1.5 (float (* trig b))))))

(defn- jungle-branch [[l atts] cfg o ^long y]
  (let [angle (double (float (* (lv/next-float l) (float (* 2 Math/PI)))))
        c (v/cos angle) s (v/sin angle)
        at #(off o (spoke c %) (+ (- y 3) (quot (long %) 2)) (spoke s %))
        l (reduce #(log %1 cfg (at %2)) l (range 5))]
    [l (conj atts (attach (off o (spoke c 4) y (spoke s 4)) -2 false))]))

(defn- mega-jungle [l cfg h o]
  (let [h (long h)]
    (loop [acc (giant l cfg h o)
           y (- h 2 (lv/next-int l 4))]
      (if (> y (quot h 2))
        (let [acc (jungle-branch acc cfg o y)]
          (recur acc (- y 2 (lv/next-int (acc 0) 4))))
        acc))))

(defn- lean [x z d]
  (let [[dx _ dz] (dir/offset d)]
    [(+ (long x) (long dx)) (+ (long z) (long dz))]))

(defn- fork-trunk [l cfg h o d at steps]
  (loop [yo 0 l l [x z] [(o 0) (o 2)] n steps ey nil]
    (if (= yo h)
      [l x z ey]
      (let [move? (and (>= yo at) (pos? n))
            [x z] (if move? (lean x z d) [x z])
            p [x (+ (long (o 1)) yo) z]
            ok (valid? l (:trunk-placer cfg) p)]
        (recur (inc yo) (log l cfg p) [x z] (if move? (dec n) n)
               (if ok (inc (long (p 1))) ey))))))

(defn- fork-branch [l cfg h o d from steps]
  (loop [yo from n steps l l [x z] [(o 0) (o 2)] ey nil]
    (if (or (>= yo h) (<= n 0))
      [l (when ey (attach [x ey z] 0 false))]
      (if (>= yo 1)
        (let [[x z] (lean x z d)
              p [x (+ (long (o 1)) yo) z]
              ok (valid? l (:trunk-placer cfg) p)]
          (recur (inc yo) (dec n) (log l cfg p) [x z]
                 (if ok (inc (long (p 1))) ey)))
        (recur (inc yo) (dec n) l [x z] ey)))))

(defn- forking [l cfg h o]
  (let [h (long h)
        l (below l cfg (up o -1))
        d (lv/pick l lv/horizontal)
        at (- h (lv/next-int l 4) 1)
        steps (- 3 (lv/next-int l 3))
        [l x z ey] (fork-trunk l cfg h o d at steps)
        atts (if ey [(attach [x ey z] 1 false)] [])
        b (lv/pick l lv/horizontal)]
    (if (= b d)
      [l atts]
      (let [from (- at (lv/next-int l 2) 1)
            [l a] (fork-branch l cfg h o b from (inc (lv/next-int l 3)))]
        [l (cond-> atts a (conj a))]))))

(defn- dark-column [l cfg p]
  (if (lv/air-or-leaves? l p)
    (reduce #(log %1 cfg %2) l
            [p (off p 1 0 0) (off p 0 0 1) (off p 1 0 1)])
    l))

(defn- dark-branch [[l atts] cfg o ey [ox oz]]
  (if (and (or (neg? ox) (> ox 1) (neg? oz) (> oz 1))
           (<= (lv/next-int l 3) 0))
    (let [n (+ (lv/next-int l 3) 2)
          at #(off o ox (- ey (long (o 1)) % 1) oz)]
      [(reduce #(log %1 cfg (at %2)) l (range n))
       (conj atts (attach (off o ox (- ey (long (o 1))) oz) 0 false))])
    [l atts]))

(defn- dark-oak [l cfg h o]
  (let [h (long h)
        l (below-four l cfg o)
        d (lv/pick l lv/horizontal)
        at (- h (lv/next-int l 4))
        steps (- 2 (lv/next-int l 3))
        ey (+ (long (o 1)) h -1)
        column (fn [[l x z n] dy]
                 (let [move? (and (>= dy at) (pos? n))
                       [x z] (if move? (lean x z d) [x z])]
                   [(dark-column l cfg [x (+ (long (o 1)) dy) z]) x z
                    (if move? (dec n) n)]))
        [l x z] (reduce column [l (o 0) (o 2) steps] (range h))
        corners (for [ox (range -1 3) oz (range -1 3)] [ox oz])]
    (reduce #(dark-branch %1 cfg o ey %2)
            [l [(attach [x ey z] 0 true)]] corners)))

(defn- bent [[l atts p] cfg tp d top i]
  (let [p (if (>= (inc i) (+ top (lv/next-int l 2))) (lv/at-dir p d) p)
        l (if (valid? l tp p) (log l cfg p) l)
        atts (cond-> atts
               (>= i (long (:min-height-for-leaves tp)))
               (conj (attach p 0 false)))]
    [l atts (lv/at-dir p :up)]))

(defn- bending [l cfg h o]
  (let [tp (:trunk-placer cfg)
        d (lv/pick l lv/horizontal)
        top (dec (long h))
        l (below l cfg (up o -1))
        [l atts p] (reduce #(bent %1 cfg tp d top %2) [l [] o]
                           (range (inc top)))
        n (lv/sample l (:bend-length tp))
        out (fn [[l atts p] _]
              [(if (valid? l tp p) (log l cfg p) l)
               (conj atts (attach p 0 false)) (lv/at-dir p d)])]
    (pop (reduce out [l atts p] (range (inc n))))))

(defn- manhattan ^long [a b]
  (reduce + (map #(Math/abs (- (long %1) (long %2))) a b)))

(defn- cherry-climb [l cfg p end d vertical axis]
  (loop [l l p p]
    (let [n (manhattan p end)]
      (if (zero? n)
        [l (attach (up end 1) 0 false)]
        (let [c (/ (float (Math/abs (- (long (end 1)) (long (p 1)))))
                   (float n))
              v? (< (lv/next-float l) (double (float c)))
              p (lv/at-dir p (if v? vertical d))]
          (recur (log l cfg p (when-not v? axis)) p))))))

(defn- cherry-branch [l cfg h o d axis from middle?]
  (let [tp (:trunk-placer cfg)
        eo (:branch-end-offset-from-top tp)
        end-y (+ (dec (long h)) (lv/sample l eo))
        far? (or middle? (< end-y from))
        reach (+ (lv/sample l (:branch-horizontal-length tp)) (if far? 1 0))
        end (up (lv/toward o d reach) end-y)
        steps (if far? 2 1)
        [l p] (reduce (fn [[l p] _]
                        (let [p (lv/at-dir p d)] [(log l cfg p axis) p]))
                      [l (up o from)] (range steps))
        vertical (if (> (long (end 1)) (long (p 1))) :up :down)]
    (cherry-climb l cfg p end d vertical axis)))

(defn- cherry-starts [l tp ^long h]
  (let [s (:branch-start-offset-from-top tp)
        s2 (update s :max-inclusive dec)
        a (max 0 (+ (dec h) (lv/sample l s)))
        b (max 0 (+ (dec h) (lv/sample l s2)))]
    [a (if (>= b a) (inc b) b)]))

(defn- cherry [l cfg h o]
  (let [tp (:trunk-placer cfg) h (long h)
        l (below l cfg (up o -1))
        [a b] (cherry-starts l tp h)
        n (lv/sample l (:branch-count tp))
        top (cond (= n 3) h (>= n 2) (inc (max a b)) :else (inc a))
        l (reduce #(log %1 cfg (up o %2)) l (range top))
        atts (if (= n 3) [(attach (up o top) 0 false)] [])
        d (lv/pick l lv/horizontal)
        axis (dir/axis d)
        [l x] (cherry-branch l cfg h o d axis a (< a (dec top)))
        atts (conj atts x)]
    (if (>= n 2)
      (let [[l y] (cherry-branch l cfg h o (dir/opposite d) axis b
                                 (< b (dec top)))]
        [l (conj atts y)])
      [l atts])))

(defn- side-branch [l cfg tp h o y d from steps]
  (loop [i from n steps l l [x z] [(o 0) (o 2)] top (+ y from) atts []]
    (if (and (< i h) (pos? n))
      (if (>= i 1)
        (let [[x z] (lean x z d)
              p [x (+ y i) z]
              ok (valid? l tp p)]
          (recur (inc i) (dec n) (log l cfg p) [x z]
                 (if ok (inc (+ y i)) (+ y i))
                 (conj atts (attach p 0 false))))
        (recur (inc i) (dec n) l [x z] top atts))
      [l (if (> (- (long top) y) 1)
           (let [f [x top z]]
             (into atts [(attach f 0 false) (attach (up f -2) 0 false)]))
           atts)])))

(defn- branch-step [[l atts] cfg tp h o i]
  (let [y (+ (long (o 1)) i)
        p [(o 0) y (o 2)]
        ok (valid? l tp p)
        l (log l cfg p)
        [l atts] (if (and ok (< i (dec h))
                          (lv/chance? l
                                      (:place-branch-per-log-probability tp)))
                   (let [d (lv/pick l lv/horizontal)
                         xl (:extra-branch-length tp)
                         len (lv/sample l xl)
                         from (max 0 (- len (lv/sample l xl) 1))
                         steps (lv/sample l (:extra-branch-steps tp))
                         [l more] (side-branch l cfg tp h o y d from steps)]
                     [l (into atts more)])
                   [l atts])]
    [l (cond-> atts (= i (dec h)) (conj (attach (up p 1) 0 false)))]))

(defn- upwards [l cfg h o]
  (let [tp (:trunk-placer cfg)]
    (reduce #(branch-step %1 cfg tp h o %2) [l []] (range h))))

(declare fancy)

(defn place
  "TrunkPlacer.placeTrunk: returns [lv attachments]."
  [l cfg h o]
  ((case (:type (:trunk-placer cfg))
     :straight-trunk-placer straight
     :giant-trunk-placer giant
     :mega-jungle-trunk-placer mega-jungle
     :forking-trunk-placer forking
     :dark-oak-trunk-placer dark-oak
     :bending-trunk-placer bending
     :cherry-trunk-placer cherry
     :upwards-branching-trunk-placer upwards
     :fancy-trunk-placer fancy)
   l cfg h o))

(defn height
  "TrunkPlacer.getTreeHeight."
  ^long [l tp]
  (+ (long (:base-height tp))
     (lv/next-int l (inc (long (:height-rand-a tp))))
     (lv/next-int l (inc (long (:height-rand-b tp))))))

(defn- ffloor ^long [^double v]
  (let [i (long v)] (if (< v i) (dec i) i)))

(defn- limb-axis [s p]
  (let [xd (Math/abs (- (long (p 0)) (long (s 0))))
        zd (Math/abs (- (long (p 2)) (long (s 2))))
        m (max xd zd)]
    (cond (zero? m) :y (= xd m) :x :else :z)))

(defn- limb-cells [s e]
  (let [d (mapv - e s)
        steps (reduce max (map #(Math/abs (long %)) d))
        f (mapv #(/ (float %) (float steps)) d)
        at (fn [i c] (ffloor (float (+ 0.5 (float (* (float i) c))))))]
    (for [i (range (inc steps))]
      (mapv + s (mapv #(at i %) f)))))

(defn- limb-free? [l cfg s e]
  (or (= s e)
      (every? #(free? l (:trunk-placer cfg) %) (limb-cells s e))))

(defn- limb [l cfg s e]
  (reduce #(log %1 cfg %2 (limb-axis s %2)) l (limb-cells s e)))

(defn- tree-shape ^double [^long height ^long y]
  (if (< (float y) (float (* (float height) (float 0.3))))
    -1.0
    (let [r (float (/ height 2.0))
          adj (float (- r y))
          dist (float (Math/sqrt (float (- (* r r) (* adj adj)))))]
      (cond (zero? adj) (* r 0.5)
            (>= (Math/abs adj) r) 0.0
            :else (float (* dist 0.5))))))

(defn- cluster [[l coords] cfg o y shape top]
  (let [radius (* shape (+ (lv/next-float l) 0.328))
        angle (* (double (float (* (lv/next-float l) 2.0))) Math/PI)
        s (off o (ffloor (+ (* radius (Math/sin angle)) 0.5)) (dec y)
               (ffloor (+ (* radius (Math/cos angle)) 0.5)))]
    (if-not (limb-free? l cfg s (up s 5))
      [l coords]
      (let [dx (- (long (o 0)) (long (s 0)))
            dz (- (long (o 2)) (long (s 2)))
            reach (Math/sqrt (double (+ (* dx dx) (* dz dz))))
            bh (- (double (s 1)) (* reach 0.381))
            bt (if (> bh top) top (long bh))
            base [(o 0) bt (o 2)]]
        [l (cond-> coords (limb-free? l cfg base s) (conj [s bt]))]))))

(defn- trimmed? [^long height ^long y] (>= (double y) (* height 0.2)))

(defn- fancy [l cfg h o]
  (let [height (+ (long h) 2)
        trunk (ffloor (* height 0.618))
        l (below l cfg (up o -1))
        per-y (min 1 (ffloor (+ 1.382 (Math/pow (/ height 13.0) 2.0))))
        top (+ (long (o 1)) trunk)
        grow (fn [acc y]
               (let [shape (tree-shape height y)]
                 (if (neg? shape)
                   acc
                   (reduce (fn [acc _] (cluster acc cfg o y shape top))
                           acc (range per-y)))))
        [l coords] (reduce grow [l [[(up o (- height 5)) top]]]
                           (range (- height 5) -1 -1))
        l (limb l cfg o (up o trunk))
        base #(vector (o 0) (% 1) (o 2))
        kept (filter (fn [[_ b]] (trimmed? height (- (long b) (long (o 1)))))
                     coords)
        l (reduce (fn [l [p b]]
                    (let [bp [(o 0) b (o 2)]]
                      (if (= bp p) l (limb l cfg bp p))))
                  l kept)]
    [l (mapv (fn [[p]] (attach p 0 false)) kept)]))
