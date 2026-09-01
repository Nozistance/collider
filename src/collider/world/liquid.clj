(ns collider.world.liquid
  (:require [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def liquids
  {:water {:block :water :step 1 :delay 5  :bucket :water-bucket :infinite? true
           :push 0.014}
   :lava  {:block :lava :step 2 :delay 30 :bucket :lava-bucket :infinite? false
           :decay-jitter 4
           :mix {:source :obsidian :flowing :cobblestone :smother :stone}}})

(def ^:private base
  (into {} (map (fn [[cls {:keys [block]}]] [cls (block/state block)])) liquids))

(def ^:private class-of-block
  (into {} (map (fn [[cls {:keys [block]}]] [block cls])) liquids))

(def ^:private bucket->class
  (into {} (map (fn [[cls {:keys [bucket]}]] [bucket cls])) liquids))

(def ^:private horiz [[1 0] [-1 0] [0 1] [0 -1]])

(def ^:private water-source (block/state :water))

(defn liquid-state? [st] (block/liquid? (long st)))
(defn liquid-class
  "Class of the liquid at the state: :water or :lava. A waterlogged state counts as :water."
  [st]
  (cond
    (liquid-state? st) (class-of-block (block/block-of (long st)))
    (block/waterlogged? (long st)) :water))
(defn level
  "Level of the liquid: 0 for a source, 1..7 flowing, 8 falling. Waterlogged states are sources."
  ^long [st]
  (if (liquid-state? st) (- (long st) (long (base (liquid-class st)))) 0))
(defn liquid-state ^long [cls ^long level] (+ (long (base cls)) level))
(defn bucket->state [item] (when-let [cls (bucket->class item)] (liquid-state cls 0)))
(defn delay-of [st] (long (get-in liquids [(liquid-class st) :delay])))

(defn source-state? [st]
  (and (liquid-state? st) (zero? (level st))))

(defn mix-class? [st]
  (some? (get-in liquids [(liquid-class st) :mix])))

(defn push-of [st]
  (get-in liquids [(liquid-class st) :push]))

(defn update-delay ^long [old new tick pos]
  (let [cls (liquid-class new)
        {:keys [delay decay-jitter]} (liquids cls)
        om  (if (liquid-state? old) (level old) 0)
        nm  (level new)]
    (if (and decay-jitter
             (= cls (liquid-class old))
             (< om 8) (< nm 8) (> nm om)
             (not= 0 (mod (hash [tick pos]) 4)))
      (* (long delay) (long decay-jitter))
      (long delay))))

(defn- raw-at [chunks template x y z]
  (let [y (long y)]
    (if (chunk/in-range? y)
      (chunk/chunks-get-block chunks template x y z)
      -1)))

(defn- state-at [chunks template x y z]
  (let [st (long (raw-at chunks template x y z))]
    (if (and (pos? st) (block/waterlogged? st)) water-source st)))

(defn- shifted [chunks template [x y z] [dx dy dz]]
  (state-at chunks template
            (+ (long x) (long dx))
            (+ (long y) (long dy))
            (+ (long z) (long dz))))

(defn- air? [st] (zero? (long st)))
(defn- effective ^long [st] (let [m (level st)] (if (>= m 8) 0 m)))
(defn- enterable? [st] (or (air? st) (and (pos? (long st)) (block/replaceable? (long st)))))
(defn- other-class? [cls st]
  (let [c (liquid-class st)]
    (and (some? c) (not= c cls))))

(defn- blocks-movement? [st]
  (let [st (long st)]
    (and (pos? st)
         (not (block/liquid? st))
         (not (block/needs-support? st)))))

(defn- decay ^long [chunks template cls p]
  (let [st (state-at chunks template (p 0) (p 1) (p 2))]
    (if (and (pos? (long st)) (= cls (liquid-class st)))
      (let [m (level st)] (if (>= m 8) 0 m))
      -1)))

(defn- normalize [[x y z]]
  (let [x (double x) y (double y) z (double z)
        len (Math/sqrt (+ (* x x) (* y y) (* z z)))]
    (if (< len 1.0E-4)
      [0.0 0.0 0.0]
      [(/ x len) (/ y len) (/ z len)])))

(defn- neighbor-pull [chunks template cls i [x y z] [dx dz]]
  (let [nx (+ (long x) (long dx))
        nz (+ (long z) (long dz))
        j  (decay chunks template cls [nx y nz])]
    (cond
      (>= j 0)
      (- j (long i))
      (not (blocks-movement? (state-at chunks template nx y nz)))
      (let [j2 (decay chunks template cls [nx (dec (long y)) nz])]
        (if (>= j2 0)
          (- j2 (- (long i) 8))
          0))
      :else 0)))

(defn- walled? [chunks template [x y z]]
  (some (fn [[dx dz]]
          (let [nx (+ (long x) (long dx))
                nz (+ (long z) (long dz))]
            (or (blocks-movement? (state-at chunks template nx y nz))
                (blocks-movement? (state-at chunks template nx (inc (long y)) nz)))))
        horiz))

(defn flow-vector [chunks template [x y z :as p]]
  (let [st (state-at chunks template x y z)]
    (when-let [cls (when (pos? (long st)) (liquid-class st))]
      (let [i (decay chunks template cls p)
            [vx vz] (reduce (fn [[vx vz] [dx dz :as d]]
                              (let [k (neighbor-pull chunks template cls i p d)]
                                [(+ (double vx) (* (long dx) k))
                                 (+ (double vz) (* (long dz) k))]))
                            [0.0 0.0]
                            horiz)]
        (if (and (>= (level st) 8) (walled? chunks template p))
          (let [[nx _ nz] (normalize [vx 0.0 vz])]
            (normalize [nx -6.0 nz]))
          (normalize [vx 0.0 vz]))))))

(defn entity-push [chunks template [x y z] half height]
  (let [x (double x) y (double y) z (double z)
        half (double half) height (double height)
        cells (distinct
               (for [cx [(long (Math/floor (- x half))) (long (Math/floor (+ x half)))]
                     cy [(long (Math/floor y)) (long (Math/floor (+ y height)))]
                     cz [(long (Math/floor (- z half))) (long (Math/floor (+ z half)))]]
                 [cx cy cz]))
        pushes (keep (fn [c]
                       (let [st (chunk/chunks-get-block chunks template c)]
                         (when-let [p (push-of st)]
                           (when-let [v (flow-vector chunks template c)]
                             [p v]))))
                     cells)
        [sx sy sz] (reduce (fn [[ax ay az] [_ [bx by bz]]]
                             [(+ (double ax) (double bx))
                              (+ (double ay) (double by))
                              (+ (double az) (double bz))])
                           [0.0 0.0 0.0]
                           pushes)
        len (Math/sqrt (+ (* (double sx) (double sx))
                          (* (double sy) (double sy))
                          (* (double sz) (double sz))))]
    (if (or (empty? pushes) (< len 1.0E-4))
      [0.0 0.0 0.0]
      (let [p (double (ffirst pushes))]
        [(* (/ (double sx) len) p)
         (* (/ (double sy) len) p)
         (* (/ (double sz) len) p)]))))

(defn- flow-into? [cls st]
  (or (enterable? st)
      (let [c (liquid-class st)]
        (and (some? c) (not= c cls)
             (nil? (get-in liquids [c :mix]))))))

(defn- mix-product [mix m]
  (when mix
    (block/state (if (zero? (long m)) (:source mix) (:flowing mix)))))

(def ^:private contact-dirs [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0]])
(def ^:private convert-dirs [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 -1 0]])
(defn- touches-other? [chunks template cls pos]
  (some (fn [d] (other-class? cls (shifted chunks template pos d))) contact-dirs))

(defn- convert-neighbors [chunks template cls [x y z]]
  (into []
        (keep (fn [[dx dy dz]]
                (let [np [(+ (long x) (long dx))
                          (+ (long y) (long dy))
                          (+ (long z) (long dz))]
                      ns (state-at chunks template (np 0) (np 1) (np 2))
                      nc (liquid-class ns)]
                  (when (and (some? nc) (not= nc cls))
                    (when-let [prod (mix-product (get-in liquids [nc :mix]) (level ns))]
                      [np prod])))))
        convert-dirs))

(defn mix-wake? [chunks template pos]
  (let [st  (state-at chunks template (pos 0) (pos 1) (pos 2))
        cls (liquid-class st)]
    (boolean
     (and cls
          (get-in liquids [cls :mix])
          (touches-other? chunks template cls pos)))))

(defn- grow-frontier [pass? seen frontier]
  (into []
        (comp (mapcat (fn [[cx cz]]
                        (map (fn [[dx dz]]
                               [(+ (long cx) (long dx)) (+ (long cz) (long dz))])
                             horiz)))
              (distinct)
              (remove seen)
              (filter (fn [[cx cz]] (pass? cx cz))))
        frontier))

(defn- hole-distance [pass? drop? start seen]
  (loop [frontier [start] seen seen d 1]
    (cond
      (some (fn [[cx cz]] (drop? cx cz)) frontier) d
      (>= d 4) 99
      :else
      (let [nxt (grow-frontier pass? seen frontier)]
        (if (empty? nxt)
          99
          (recur nxt (into seen nxt) (inc d)))))))

(defn- min-cost-dirs [costs]
  (let [best (long (reduce min costs))]
    (if (>= best 99)
      horiz
      (into [] (keep-indexed (fn [i c] (when (= c best) (horiz i)))) costs))))

(defn- flow-dirs [chunks template cls [x y z]]
  (let [x (long x) y (long y) z (long z)
        open? (fn [st] (or (enterable? st) (liquid-state? st)))
        pass? (fn [cx cz]
                (let [st (state-at chunks template cx y cz)]
                  (and (open? st)
                       (not (and (= cls (liquid-class st))
                                 (zero? (level st)))))))
        drop? (fn [cx cz] (open? (state-at chunks template cx (dec y) cz)))
        cost  (fn [[dx dz]]
                (let [sx (+ x (long dx)) sz (+ z (long dz))]
                  (if (pass? sx sz)
                    (hole-distance pass? drop? [sx sz] #{[x z] [sx sz]})
                    99)))]
    (min-cost-dirs (mapv cost horiz))))

(defn- side-states [chunks template [x y z]]
  (mapv (fn [[dx dz]]
          (state-at chunks template (+ (long x) (long dx)) y (+ (long z) (long dz))))
        horiz))

(defn- mixed-state [cls mix st above sides]
  (when (and mix (some (fn [s] (other-class? cls s)) (cons above sides)))
    (mix-product mix (level st))))

(defn- recompute-level [{:keys [cls step infinite?]} st above below sides]
  (let [same? (fn [s] (= cls (liquid-class s)))
        src?  (fn [s] (and (same? s) (zero? (level s))))
        flows (filterv same? sides)]
    (cond
      (src? st) 0
      (and infinite?
           (>= (count (filterv src? sides)) 2)
           (or (src? below)
               (not (or (air? below) (liquid-state? below))))) 0
      (same? above) 8
      (seq flows)
      (let [m (+ (long step) (long (reduce min (map effective flows))))]
        (when (<= m 7) m))
      :else nil)))

(defn- arrive [{:keys [chunks template cls mix]} pos nl]
  (let [plain (liquid-state cls nl)
        state (if (and mix (touches-other? chunks template cls pos))
                (or (mix-product mix nl) plain)
                plain)]
    (cons [pos state] (convert-neighbors chunks template cls pos))))

(defn- fall-changes [env [x y z] below]
  (when (and (pos? (long y))
             (or (enterable? below) (not= 8 (level below))))
    (arrive env [x (dec (long y)) z] 8)))

(defn- spread-target? [cls ns]
  (flow-into? cls ns))

(defn- spread-changes [{:keys [chunks template cls] :as env} [x y z :as p] nl]
  (into []
        (mapcat (fn [[dx dz]]
                  (let [np [(+ (long x) (long dx)) y (+ (long z) (long dz))]
                        ns (state-at chunks template (np 0) (np 1) (np 2))]
                    (when (spread-target? cls ns)
                      (arrive env np nl)))))
        (flow-dirs chunks template cls p)))

(defn- flow-changes [{:keys [cls step mix] :as env} [x y z :as p] nm below]
  (let [nl        (if (= 8 (long nm)) 1 (+ (long nm) (long step)))
        down?     (or (enterable? below)
                      (and (= cls (liquid-class below))
                           (not (source-state? below))))
        grounded? (not (or (enterable? below) (liquid-state? below)))]
    (cond
      (and mix (other-class? cls below))
      [[[x (dec (long y)) z] (block/state (:smother mix))]]
      down? (fall-changes env p below)
      (and (< nl 8) (or (zero? (long nm)) grounded?))
      (spread-changes env p nl))))

(def ^:private column-drag {:soul-sand :false :magma :true})

(defn bubble-column? [st] (= :bubble-column (block/type-of (long st))))

(defn- column-state
  "Bubble column state that belongs above the raw state below, or nil."
  [below]
  (cond
    (bubble-column? below) below
    :else (when-let [drag (get column-drag (block/type-of (long below)))]
            (block/state :bubble-column {:drag drag}))))

(defn- water-source? [st] (= (long st) water-source))

(defn- column-changes [chunks template [x y z] col]
  (loop [y (long y) acc []]
    (let [st (long (raw-at chunks template x y z))]
      (if (or (water-source? st) (and (bubble-column? st) (not= st (long col))))
        (recur (inc y) (conj acc [[x y z] col]))
        acc))))

(defn- bubble-changes [chunks template [x y z :as p]]
  (let [raw (long (raw-at chunks template x y z))
        col (column-state (long (raw-at chunks template x (dec (long y)) z)))]
    (cond
      (and (bubble-column? raw) (nil? col)) [[p water-source]]
      (and col (or (water-source? raw) (not= raw (long col)))) (column-changes chunks template p col))))

(defn bubble-push
  "Vertical speed of an entity with its feet at pos after the bubble column there, as vanilla."
  ^double [chunks template pos ^double vy]
  (let [x (long (Math/floor (v/x pos))) y (long (Math/floor (v/y pos))) z (long (Math/floor (v/z pos)))
        st (long (raw-at chunks template x y z))]
    (if (bubble-column? st)
      (let [drag? (= :true (:drag (block/props-of st)))
            open? (zero? (long (raw-at chunks template x (inc y) z)))]
        (cond
          (and drag? open?) (max -0.9 (- vy 0.03))
          drag? (max -0.3 (- vy 0.03))
          open? (min 1.8 (+ vy 0.1))
          :else (min 0.7 (+ vy 0.06))))
      vy)))

(defn update-cell [chunks template [x y z :as p]]
  (let [st  (state-at chunks template x y z)
        cls (liquid-class st)]
    (when cls
      (let [{:keys [step infinite? mix]} (liquids cls)
            env   {:chunks chunks :template template :cls cls
                   :step (long step) :infinite? infinite? :mix mix}
            above (shifted chunks template p [0 1 0])
            below (shifted chunks template p [0 -1 0])
            sides (side-states chunks template p)]
        (if-let [mixed (mixed-state cls mix st above sides)]
          [[p mixed]]
          (if-let [nm (recompute-level env st above below sides)]
            (or (when (zero? (long nm)) (seq (bubble-changes chunks template p)))
                (into (if (not= (long nm) (level st))
                        [[p (liquid-state cls nm)]]
                        [])
                      (flow-changes env p nm below)))
            [[p 0]]))))))

(def rule
  {:name   :liquid
   :match? (fn [_chunks st _p] (some? (liquid-class st)))
   :wake   (fn [chunks tick p old self?]
             (if (mix-wake? chunks gen/flat-chunk p)
               (inc (long tick))
               (+ (long tick)
                  (if self?
                    (update-delay old (chunk/chunks-get-block chunks gen/flat-chunk p) tick p)
                    (delay-of (chunk/chunks-get-block chunks gen/flat-chunk p))))))
   :due    (fn [chunks p] (update-cell chunks gen/flat-chunk p))})
