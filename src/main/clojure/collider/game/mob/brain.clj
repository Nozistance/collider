(ns collider.game.mob.brain
  "Memories, activities and behaviours of a mob with a brain."
  (:require [clojure.string :as str]
            [collider.hash-order :as hash-order]
            [collider.random :as random])
  (:import (java.util Collection)))

(set! *warn-on-reflection* true)

(def ^:const forever Long/MAX_VALUE)

(defn- memories [e] (:memories (:brain e)))

(defn until
  "Returns the last tick a memory set at tick t for ttl ticks is
  there. A ttl of forever never ends."
  ^long [t ^long ttl]
  (if (== ttl forever) forever (+ (long t) ttl)))

(defn present?
  "Returns true when mob e holds memory k at tick t."
  [e k t]
  (if-let [m (get (memories e) k)]
    (<= (long t) (long (m 1)))
    false))

(defn recall
  "Returns the value of memory k of mob e at tick t, or nil."
  [e k t]
  (when (present? e k t) ((get (memories e) k) 0)))

(defn ttl-of
  "Returns how many ticks after t memory k of mob e stays. A memory
  that never ends or is not there gives forever."
  ^long [e k t]
  (let [m (get (memories e) k)]
    (cond (nil? m) forever
          (== 3 (count m)) (long (m 2))
          (== forever (long (m 1))) forever
          :else (- (long (m 1)) (long t)))))

(defn erase
  "Returns mob e without memory k."
  [e k]
  (if (contains? (memories e) k)
    (update-in e [:brain :memories] dissoc k)
    e))

(defn- blank? [v]
  (or (nil? v) (and (instance? Collection v) (empty? v))))

(defn- knows? [e k]
  (if-let [known (:known (:brain e))] (contains? known k) true))

(defn- held [e k v m]
  (cond (not (knows? e k)) e
        (blank? v) (erase e k)
        (= m (get (memories e) k)) e
        :else (assoc-in e [:brain :memories k] m)))

(defn remember
  "Returns mob e holding v as memory k up to tick until. A nil value
  or an empty collection erases the memory. A memory the breed of e
  does not know is left as it is. Before its first thought e holds
  any memory, and its brain keeps those its breed knows."
  [e k v until]
  (held e k v [v until]))

(defn remember-for
  "Returns mob e holding v as memory k from tick t for ttl ticks."
  [e k v t ttl]
  (remember e k v (until t ttl)))

(defn remember-ttl
  "Returns mob e holding v as memory k for ttl ticks, which its next
  brain tick starts to count."
  [e k v ttl]
  (held e k v [v forever ttl]))

(defn run-of
  "Returns what behaviour i of mob e keeps while it runs, or nil when
  it does not run. The part that stops it still sees it."
  [e i]
  (get (:running (:brain e)) i))

(defn slot
  "Returns what behaviour i of mob e keeps between its runs."
  [e i]
  (get (:slots (:brain e)) i))

(defn with-slot [e i m] (assoc-in e [:brain :slots i] m))

(defn- with-run [e i m] (assoc-in e [:brain :running i] m))

(defn- unrun [e i] (update-in e [:brain :running] dissoc i))

(defn- on-mob
  "Returns result r of a part with f applied to its mob."
  [r f]
  (if (vector? r) [(f (r 0)) (r 1)] (f r)))

(defn- then
  "Returns [e deltas] of acc after part f, or acc when f gives nil."
  [[e ds :as acc] f]
  (if-let [r (f e)]
    (if (vector? r) [(r 0) (into ds (r 1))] [r ds])
    acc))

(defn- holds? [known e k status t]
  (and (contains? known k)
       (case status
         :registered true
         :present (present? e k t)
         :absent (not (present? e k t)))))

(defn- needs-met? [known needs e t]
  (every? (fn [[k s]] (holds? known e k s t)) needs))

(defn- duration ^long [t eid i [lo hi]]
  (random/between (random/rnd t eid :duration i) lo hi))

(defn- halted [{:keys [stop]} i]
  (fn [w eid e t]
    (on-mob (if stop (stop w eid e t i) e) #(unrun % i))))

(defn- checked [{:keys [start? check]} w eid e t i]
  (cond check (check w eid e t i)
        start? [(start? w eid e t) e]
        :else [true e]))

(defn- end-of ^long [t eid i span]
  (if (= :never span)
    forever
    (+ (long t) (duration t eid i span))))

(defn- began [b i]
  (let [start (:start b)
        span (:duration b [60 60])]
    (fn [w eid e t]
      (let [[ok e'] (checked b w eid e t i)]
        (cond
          ok (let [e (with-run e' i {:end (end-of t eid i span)})]
               (if start (start w eid e t i) e))
          (identical? e e') nil
          :else [e' [] false])))))

(defn- kept
  "Returns [ok e] for behaviour b at index i going on. Its continue?
  answers ok alone or with the mob it changed asking."
  [{:keys [continue?]} w eid e t i]
  (if (and continue? (<= (long t) (long (:end (run-of e i)))))
    (let [r (continue? w eid e t i)]
      (if (vector? r) r [r e]))
    [false e]))

(defn- behaviour [{:keys [tick] :as b} i]
  (let [halt (halted b i)]
    {:try (began b i)
     :step (fn [w eid e t]
             (let [[ok e] (kept b w eid e t i)]
               (if ok
                 (if tick (tick w eid e t i) e)
                 (halt w eid e t))))
     :halt halt
     :running? #(some? (run-of % i))}))

(defn- one-shot [b i]
  (let [f (:one-shot b) same (fn [_ _ e _] e)]
    {:try (fn [w eid e t] (f w eid e t i))
     :step same :halt same :running? (constantly false)}))

(defn shuffle-order
  "Returns the indexes of weights in the order rolls put them. Roll j
  is at least 0 and below 1 and goes with weight j. A heavier weight
  tends to come earlier."
  [weights rolls]
  (let [rank (fn [j]
               (let [r (double (float (rolls j)))
                     x (double (float (/ 1.0 (weights j))))]
                 (- (Math/pow r x))))]
    (vec (sort-by rank (range (count weights))))))

(defn- each-running [kids o f acc]
  (reduce (fn [acc j]
            (let [k (kids j)]
              (if ((:running? k) (acc 0)) (then acc #(f k %)) acc)))
          acc o))

(defn- tried [kids one? w eid t acc j]
  (let [k (kids j)
        e (acc 0)
        r (when (and (not ((:running? k) e)) ((:check k) e t))
            ((:try k) w eid e t))
        acc' (if r (then acc (constantly r)) acc)]
    (if (and one? r (not (false? (get r 2)))) (reduced acc') acc')))

(defn- gate-order [g i n]
  (let [ws (mapv second (:items g))
        roll #(random/rnd %1 %2 [:shuffle %3] i)
        rolls (fn [t eid] (mapv #(roll t eid %) (range n)))]
    (if (= :shuffled (:order g))
      (fn [t eid] (shuffle-order ws (rolls t eid)))
      (constantly (vec (range n))))))

(defn- gate-halt [g kids i]
  (fn [w eid e t]
    (let [o (:order (run-of e i))
          f (fn [k e] ((:halt k) w eid e t))
          acc (each-running kids o f [(unrun e i) []])]
      (on-mob acc #(reduce erase % (:erase g))))))

(defn- gate-step [kids i halt]
  (fn [w eid e t]
    (let [o (:order (run-of e i))
          f (fn [k e] ((:step k) w eid e t))
          acc (each-running kids o f [e []])]
      (if (some #((:running? (kids %)) (acc 0)) o)
        acc
        (then acc #(halt w eid % t))))))

(defn- gate [g i kids]
  (let [order (gate-order g i (count kids))
        one? (= :run-one (:gate g))
        halt (gate-halt g kids i)]
    {:try (fn [w eid e t]
            (let [o (order t eid)]
              (reduce (partial tried kids one? w eid t)
                      [(with-run e i {:order o}) []] o)))
     :step (gate-step kids i halt)
     :halt halt
     :running? #(some? (run-of % i))}))

(declare control)

(defn- kids-of [items nxt known]
  (reduce (fn [[ks n] [kid _]]
            (let [[k n'] (control kid n (inc (long n)) known)]
              [(conj ks k) n']))
          [[] nxt] items))

(defn- gate-of [b i nxt known]
  (let [[ks n] (kids-of (:items b) nxt known)]
    [(gate b i ks) n]))

(defn- control
  "Returns [part next] for behaviour b at index i. The children of a
  gate take the indexes from nxt on."
  [b i nxt known]
  (let [[c nxt] (cond
                  (:one-shot b) [(one-shot b i) nxt]
                  (:gate b) (gate-of b i nxt known)
                  :else [(behaviour b i) nxt])
        needs (:needs b)]
    [(assoc c :check #(needs-met? known needs %1 %2)) nxt]))

(def ^:private header
  #{:requires :erase-on-stop :update :memories :default :sense
    :sensors :activities})

(defn- activities [spec]
  (or (:activities spec) (remove header (keys spec))))

(defn- pairs [xs]
  (map-indexed (fn [k x] (if (map? x) [k x] x)) xs))

(defn- activity-name [a] (str/replace (name a) \- \_))

(defn- hash-ordered [es]
  (let [acts (vec (distinct (map second es)))
        by (group-by second es)]
    (mapcat by (hash-order/computed activity-name acts))))

(defn order
  "Returns [prio activity behaviour] for each behaviour of spec in
  the order a brain visits them. Lower priorities come first. Within
  one the activities go by the hash of their names and their place in
  :activities of spec or else in spec, and the behaviours of an
  activity by their place."
  [spec]
  (let [acts (activities spec)
        es (for [a acts [p b] (pairs (spec a))] [p a b])]
    (mapcat (comp hash-ordered val)
            (sort-by key (group-by first es)))))

(defn- node-memories [b]
  (into (set (keys (:needs b)))
        (mapcat (comp node-memories first))
        (:items b)))

(defn- controls [es known]
  (first
    (reduce (fn [[cs nxt] [i [_ _ b]]]
              (let [[c nxt] (control b i nxt known)]
                [(conj cs c) nxt]))
            [[] (count es)] (map-indexed vector es))))

(defn- mask-of ^long [mems status b]
  (let [bit (zipmap mems (map #(bit-shift-left 1 %) (range)))]
    (reduce (fn [^long m [k s]] (if (= s status) (bit-or m (long (bit k))) m))
            0 (:needs b))))

(defn- node-tells [b]
  (cons (:tells? b) (mapcat (comp node-tells first) (:items b))))

(defn- tells-of [es]
  (let [fs (vec (keep identity (mapcat (comp node-tells peek) es)))]
    (when (seq fs) (fn [e t] (boolean (some #(% e t) fs))))))

(defn- info [spec es known]
  (let [acts (activities spec)]
    {:known known :update (:update spec) :sensors (:sensors spec)
     :tells? (tells-of es)
     :default (:default spec :idle) :erase (:erase-on-stop spec)
     :requires (merge (zipmap acts (repeat {})) (:requires spec))
     :order (mapv (fn [[p a b]] [p a (:id b)]) es)}))

(defn- known-of [spec es]
  (let [known (into (set (:memories spec))
                    (mapcat #(node-memories (peek %))) es)]
    (assert (<= (count known) 64) "a brain knows at most 64 memories")
    known))

(defn breed
  "Returns the brain of a breed from spec. An activity maps to its
  behaviours, a bare behaviour takes its place as priority, and the
  header keys give the rest. Its tells? holds when one of its
  behaviours tells? that a mob may write to others."
  [spec]
  (let [es (vec (order spec))
        known (known-of spec es)
        mems (vec (sort known))
        cs (controls es known)]
    (assoc (info spec es known)
           ::memories mems ::sense (:sense spec)
           ::steps (mapv (fn [[_ a b] c]
                           {:activity a :try (:try c) :step (:step c)
                            :needs (mask-of mems :present b)
                            :shuns (mask-of mems :absent b)})
                         es cs))))

(defn fresh
  "Returns the brain of a new mob of breed b, with the memories mems
  its breed knows."
  ([b] (fresh b nil))
  ([b mems]
   {:activity (:default b) :memories (select-keys mems (:known b))
    :known (:known b) :breed b}))

(defn- expiry ^long [m] (long (nth m 1)))

(defn- counted
  "Returns memory m that counts its ticks, held until it runs out, or
  nil when it already has."
  [m ^long t]
  (let [until (+ t (long (nth m 2)) -1)]
    (when-not (> t until) [(nth m 0) until])))

(defn- forgotten [e ^long t]
  (let [mems (:memories (:brain e))
        kept (reduce-kv (fn [kept k v]
                          (let [c (if (= 3 (count v)) (counted v t) v)]
                            (cond (or (nil? c) (> t (expiry c))) (dissoc kept k)
                                  (identical? c v) kept
                                  :else (assoc kept k c))))
                        mems mems)]
    (if (identical? kept mems) e (assoc-in e [:brain :memories] kept))))

(defn- present-bits ^long [b e ^long t]
  (let [mems (:memories (:brain e))]
    (reduce-kv (fn [^long m k mem]
                 (let [v (get mems mem)]
                   (if (and v (<= t (expiry v))) (bit-set m k) m)))
               0 (::memories b))))

(defn- ready? [b step e t]
  (let [m (present-bits b e t) needs (long (:needs step))]
    (and (= needs (bit-and m needs)) (zero? (bit-and m (long (:shuns step)))))))

(defn- running? [e i] (some? (get (:running (:brain e)) (long i))))

(defn- active? [e step]
  (let [a (:activity step)]
    (or (identical? :core a) (identical? a (:activity (:brain e))))))

(defn- mob-of [r] (if (vector? r) (nth r 0) r))

(defn- with-deltas [ds r] (if (vector? r) (into ds (nth r 1)) ds))

(defn- started [b w eid e t ds]
  (reduce-kv (fn [[e ds] i step]
               (if-let [r (and (active? e step) (not (running? e i))
                               (ready? b step e t)
                               ((:try step) w eid e t))]
                 [(mob-of r) (with-deltas ds r)]
                 [e ds]))
             [e ds] (::steps b)))

(defn- ticked [b w eid e t ds]
  (let [steps (::steps b)
        busy (filterv #(running? e %) (range (count steps)))]
    (reduce (fn [[e ds] i]
              (let [r ((:step (steps i)) w eid e t)]
                [(mob-of r) (with-deltas ds r)]))
            [e ds] busy)))

(defn think
  "Returns [e deltas] after one tick of the brain b of mob e. Expired
  memories go first, the sensors run, the behaviours that may start
  start, and last every running behaviour ticks or stops."
  [b world eid e t]
  (let [t (long t)
        e (forgotten e t)
        r (when-let [f (::sense b)] (f world eid e t))
        [e ds] (if (::sense b) [(mob-of r) (with-deltas [] r)] [e []])
        [e ds] (started b world eid e t ds)]
    (ticked b world eid e t ds)))

(defn- met? [b e t a]
  (when-let [req (get (:requires b) a)]
    (needs-met? (:known b) req e t)))

(defn- switched [b e a]
  (let [cur (:activity (:brain e))
        gone (concat (get (:erase b) :core) (get (:erase b) cur))]
    (if (or (= a :core) (= a cur))
      e
      (assoc-in (reduce erase e gone) [:brain :activity] a))))

(defn update-activity
  "Returns mob e with the first activity of the update list of b
  whose requirements e meets at tick t, or e when none does. Leaving
  an activity erases its memories."
  [b e t]
  (if-let [a (some #(when (met? b e t %) %) (:update b))]
    (switched b e a)
    e))

(def memory-types
  "The vanilla id of each memory a mob may hold and whether a save
  keeps it."
  {:attack-target ["attack_target" false]
   :breed-target ["breed_target" false]
   :cant-reach-walk-target-since
   ["cant_reach_walk_target_since" false]
   :danger-detected-recently ["danger_detected_recently" true]
   :gaze-cooldown-ticks ["gaze_cooldown_ticks" true]
   :hurt-by ["hurt_by" false]
   :hurt-by-entity ["hurt_by_entity" false]
   :is-in-water ["is_in_water" true]
   :is-panicking ["is_panicking" true]
   :is-tempted ["is_tempted" true]
   :long-jump-cooldown-ticks ["long_jump_cooling_down" true]
   :long-jump-mid-jump ["long_jump_mid_jump" false]
   :look-target ["look_target" false]
   :nearest-living-entities ["mobs" false]
   :nearest-players ["nearest_players" false]
   :nearest-visible-adult ["nearest_visible_adult" false]
   :nearest-visible-attackable-player
   ["nearest_visible_targetable_player" false]
   :nearest-visible-attackable-players
   ["nearest_visible_targetable_players" false]
   :nearest-visible-living-entities ["visible_mobs" false]
   :nearest-visible-player ["nearest_visible_player" false]
   :path ["path" false]
   :ram-cooldown-ticks ["ram_cooldown_ticks" true]
   :ram-target ["ram_target" false]
   :temptation-cooldown-ticks ["temptation_cooldown_ticks" true]
   :tempting-player ["tempting_player" false]
   :walk-target ["walk_target" false]})

(defn- kept? [k] (true? (get-in memory-types [k 1])))

(defn- ttl-left
  "Returns the ticks memory m has after tick t, nil when it never
  ends, or a negative number when it is gone."
  [m ^long t]
  (cond (== 3 (count m)) (m 2)
        (== forever (long (m 1))) nil
        :else (- (long (m 1)) t)))

(defn saved
  "Returns the memories of mob e a save at tick t keeps, each as its
  value with the ticks it has left when it ends."
  [e t]
  (reduce-kv (fn [acc k m]
               (let [r (ttl-left m (long t))]
                 (cond (not (kept? k)) acc
                       (nil? r) (assoc acc k [(m 0)])
                       (pos? (long r)) (assoc acc k [(m 0) r])
                       :else acc)))
             nil (memories e)))

(defn loaded
  "Returns the memories saved as ms back at tick t."
  [ms t]
  (reduce-kv (fn [acc k [v r]]
               (assoc acc k [v (if r (+ (long t) (long r)) forever)]))
             {} ms))

(defn timed?
  "Returns true when what a save keeps of the memories of e depends
  on the tick."
  [e]
  (boolean
    (some (fn [[k m]]
            (and (kept? k) (== 2 (count m))
                 (not= forever (m 1))))
          (memories e))))

(defn- later [^long x ^long dt] (if (== forever x) x (+ x dt)))

(defn- later-memory [m dt]
  (if (== 2 (count m)) [(m 0) (later (m 1) dt)] m))

(defn- later-run [r dt]
  (cond-> r
    (:end r) (update :end later dt)
    (:spawn r) (update :spawn later dt)))

(defn delayed
  "Returns brain b with each tick it waits for moved on by dt ticks."
  [b dt]
  (cond-> b
    (:memories b) (update :memories update-vals #(later-memory % dt))
    (:running b) (update :running update-vals #(later-run % dt))
    (:phase b) (update :phase (partial mapv #(later % dt)))))
