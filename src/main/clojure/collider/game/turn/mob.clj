(ns collider.game.turn.mob
  "Mob turns in a tick."
  (:require [clojure.core.reducers :as r]
            [collider.data :as data]
            [collider.game.areas :as areas]
            [collider.game.attribute :as attribute]
            [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.clock :as clock]
            [collider.game.mob.control :as control]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.nav :as nav]
            [collider.game.mob.push :as push]
            [collider.game.mob.sense :as sense]
            [collider.game.mob.spec :as spec]
            [collider.game.mob.travel :as travel]
            [collider.game.out :as out]
            [collider.game.turn.living :as living]
            [collider.game.turn.overlay :as overlay]
            [collider.par :as par]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.motion :as motion])
  (:import (clojure.lang MapEntry)
           (collider.data LongMap)
           (collider.game.mob Steer)))

(set! *warn-on-reflection* true)

(defn- think [world eid e t tempters]
  (if-let [b (:brain (spec/of (:type e)))]
    (b world eid e t tempters)
    [e nil]))

(defn- sound-pitch ^double [e ^long t ^long eid]
  (let [base (if (mobs/baby? e) 1.5 1.0)]
    (+ base (* 0.2 (- (random/of-longs t eid (hash :p1))
                      (random/of-longs t eid (hash :p2)))))))

(defn- wide-pitch ^double [^long t ^long eid kind]
  (+ 1.0 (* 0.4 (- (random/of-longs t eid (hash kind) (hash :w1))
                   (random/of-longs t eid (hash kind) (hash :w2))))))

(defn- water-vol ^double [vel3 k]
  (let [vx (v/x vel3) vy (v/y vel3) vz (v/z vel3)]
    (min 1.0 (* (Math/sqrt (+ (* vx vx 0.2) (* vy vy) (* vz vz 0.2)))
                (double k)))))

(defn- says? [^long t ^long eid ^long since]
  (< (long (* 1000.0 (random/of-longs t eid (hash :say))))
     (- t since)))

(defn- said [eid e t]
  (let [e (assoc e :say-tick (+ (long t) mobs/ambient-interval 1))]
    (if-let [say (mobs/sound-of e :say)]
      (let [p (sound-pitch e t eid)]
        [e [(out/all (out/sound say (:pos e) 1.0 p))]])
      [e nil])))

(defn- ambient
  "Returns mob e and its ambient sound at tick t. The chance to speak
  grows by a thousandth each tick since the mob was last heard or
  hurt."
  [eid e t]
  (let [st (:say-tick e)]
    (cond (says? t eid (long (or st t))) (said eid e t)
          (nil? st) [(assoc e :say-tick t) nil]
          :else [e nil])))

(defn- step-state ^long [world e]
  (let [ch (:chunks world) p (:pos e)
        st (motion/below-state ch p (:support e) 0.2)
        [x _ z] (or (:support e) (mapv #(Math/floor (double %)) p))
        y (inc (long (Math/floor (- (v/y p) 0.2))))
        up (sense/block-at world (long x) y (long z))]
    (if (or (block/tagged? up "inside_step_sound_blocks")
            (block/tagged? up "combination_step_sound_blocks"))
      up
      st)))

(defn- block-step [world e]
  (let [st (step-state world e)]
    (when-not (block/air? st)
      (let [info (data/info (block/block-of st))
            s (get (data/sounds) (:sound info))
            vol (double (float (* (float (:volume s)) (float 0.15))))]
        (out/all (out/sound (:step s) (:pos e) vol (:pitch s)))))))

(defn- step-sound-delta [world e t eid]
  (cond
    (:wet? e)
    (let [vol (water-vol (:vel e) 0.35)
          pitch (wide-pitch (long t) (long eid) :swm)]
      (out/all (out/sound :swim (:pos e) vol pitch)))
    (not (:on-ground e)) nil
    (:block-steps? (mobs/types (:type e))) (block-step world e)
    :else
    (when-let [snd (mobs/sound-of e :step)]
      (out/all (out/sound snd (:pos e) 0.15 1.0)))))

(defn- splash-delta [e t eid]
  (out/all (out/sound :splash (:pos e) (water-vol (:vel e) 0.2)
                      (wide-pitch (long t) (long eid) :spl))))

(defn- movement-sounds
  [world acc e was-wet? old-walked new-walked t eid]
  (let [acc (if (and (:wet? e) (not was-wet?))
              (conj acc (splash-delta e t eid))
              acc)]
    (if (> (long (Math/floor (double new-walked)))
           (long (Math/floor (double old-walked))))
      (if-let [d (step-sound-delta world e t eid)] (conj acc d) acc)
      acc)))

(defn- age-up
  "Returns mob e grown up at tick t when its time has come. A baby
  whose age is locked does not grow up."
  [e t]
  (let [until (:baby-until e)]
    (if (and until (>= (long t) (long until)) (not (:age-locked? e)))
      (assoc e :baby-until nil)
      e)))

(defn- brain-step [world eid e t tempters dead?]
  (let [[e1 deltas] (if dead? [e nil] (think world eid e t tempters))
        e1 (age-up e1 t)
        [e1 say-deltas] (if dead? [e1 nil] (ambient eid e1 t))]
    [e1 deltas say-deltas]))

(defn- steered [world eid e speed half]
  (if-let [f (:steer (spec/of (:type e)))]
    (f world eid e speed half)
    (let [[e nav m] (nav/aim world e)]
      (control/tick world e speed (* 2.0 (double half)) nav m))))

(defn- spent-jump [e dead?]
  (cond-> e
    (:jump e) (entity/with {:jump false})
    dead? (entity/with {:move (Steer/halted (:move e))})))

(defn- move-speed ^double [e]
  (if-let [fx (not-empty (:effects e))]
    (attribute/value e fx :movement-speed)
    (mobs/speed (:type e))))

(defn- joined-into [acc more]
  (if (zero? (count more)) acc (into acc more)))

(defn- stepped-deltas [world eid e e2 [pre ds say-ds] t]
  (let [changes (entity/mob-changes e e2)
        acc (if (pos? (count changes))
              (conj (vec pre) [:merge-entity eid changes])
              (vec pre))
        acc (-> acc (joined-into ds) (joined-into say-ds))]
    (movement-sounds world acc e2 (boolean (:wet? e))
                     (double (or (:walked e) 0.0))
                     (double (or (:walked e2) 0.0)) t eid)))

(defn- minded
  "Returns mob e after its base tick, after it thought and steered
  this tick, the head it turns, and its deltas and sounds. Other
  bodies do not move it yet."
  [world tempters eid e t]
  (let [[e pre] (living/based world eid e)
        [half _] (mobs/box-of e)
        speed (move-speed e)
        dead? (not (pos? (double (:health e))))
        e0 (spent-jump e dead?)
        [e1 ds say-ds] (brain-step world eid e0 t tempters dead?)
        e1 (if dead? e1 (steered world eid e1 speed half))
        look (when-not dead? (control/look-of world e1 t))]
    [e1 look ds say-ds pre]))

(defn- step-mob
  [world index eid e [e1 look ds say-ds pre] t cram live?]
  (let [[half height] (mobs/box-of e)
        more [look e cram live?]
        [e2 shoves hit? ls]
        (travel/moved world index eid e1 half height more)
        [e2 own] (if-let [f (:ai-step (spec/of (:type e)))]
                   (f eid e2 t)
                   [e2 nil])
        ds (stepped-deltas world eid e e2 [pre ds say-ds] t)]
    [e2 (joined-into ds own) shoves hit? ls]))

(defn- hand
  "Returns the delta that hands the shove sh to mob e in slot j, whose
  speed this tick so far is in vels, or nil when e takes no shove."
  [e ^objects vels j [eid dx dz]]
  (when (mobs/mob-type? (:type e))
    (let [j (int j)
          vel (or (aget vels j) (:vel e))
          v (v/v3 (- (v/x vel) (double dx)) (v/y vel)
                  (- (v/z vel) (double dz)))]
      (aset vels j v)
      [:merge-entity eid {:vel v}])))

(defn- takes-now? [^booleans ticking ^long i ^long j]
  (or (< j i) (not (aget ticking j))))

(defn- taker [chunks ticking slots es i sh]
  (let [j (push/slot slots (nth sh 0))]
    (when (and (>= j 0) (takes-now? ticking i j)
               (push/pushable? chunks (nth (nth es j) 1)))
      j)))

(defn- handing [chunks ticking vels slots es i shoves]
  (let [f (fn [acc sh]
            (let [j (taker chunks ticking slots es i sh)
                  d (when j (hand (nth (nth es j) 1) vels j sh))]
              (if d (conj acc d) acc)))]
    (reduce f [] shoves)))

(defn- takers
  "Returns the slot and the shove of each body that takes a shove
  of mob i now."
  [chunks ticking slots es i shoves]
  (let [f (fn [acc sh]
            (if-let [j (taker chunks ticking slots es i sh)]
              (conj (or acc []) [j sh])
              acc))]
    (reduce f nil shoves)))

(defn- live-of [chunks slots es]
  (fn [o]
    (push/pushable? chunks (nth (nth es (push/slot slots o)) 1))))

(defn- crowd [chunks index slots es eid e]
  (let [[half height] (mobs/box-of e)
        alive? (live-of chunks slots es)
        f (fn [^long n o] (if (alive? o) (inc n) n))]
    (reduce f 0 (push/touching index eid e half height))))

(defn- crammed?
  "Returns true when mob e, crowded, takes cramming damage this
  tick."
  [world index slots es eid e pos t]
  (and (push/alive? e)
       (push/cramming-draw? t eid)
       (let [e (assoc e :pos pos)]
         (push/crowded?
           world (crowd (:chunks world) index slots es eid e)))))

(def ^:private crush {:type :cramming})

(defn- cram-of
  "Returns the fn that hurts mob eid of es when it stands crammed,
  before it pushes."
  [world index slots es eid t]
  (fn [e pos]
    (when (crammed? world index slots es eid e pos t)
      (entity/hurt (assoc e :pos pos) push/cramming-damage crush t
                   eid))))

(defn- stepping? [^booleans ticking es ^long i]
  (and (aget ticking i) (mobs/mob-type? (:type (nth (nth es i) 1)))))

(defn- run-turn
  "Returns mob i of es after its step from what it thought, with its
  cramming, its deltas and the shoves it gave."
  [world index t slots es i mind]
  (let [[eid e] (nth es i)
        cram (cram-of world index slots es eid t)
        [e2 ds shoves hit? ls]
        (step-mob world index eid e mind t cram
                  (live-of (:chunks world) slots es))
        cs (when hit? [[:damage eid push/cramming-damage crush]])
        [e2 ds] (living/touched world eid e2 (:wet? e) ds ls cs)]
    [e2 ds shoves]))

(defn- live
  "Returns world as mob eid sees it in its turn, with the blocks the
  turns before it wrote this tick."
  [world ^long eid]
  (let [w (overlay/seen world eid)]
    (if (identical? w world)
      w
      (assoc w :watchers (:watchers world)))))

(defn- turn [world ticking vels tempters t index slots es i]
  (let [[eid e] (nth es i)
        world (live world eid)
        [e2 ds shoves]
        (if (stepping? ticking es i)
          (run-turn world index t slots es i
                    (minded world tempters eid e t))
          [e nil nil])
        es (assoc! es i [eid e2])
        hs (handing (:chunks world) ticking vels slots es i
                    shoves)
        from (when-not (identical? (:pos e) (:pos e2)) (:pos e))]
    [es (if (seq hs) (into (vec ds) hs) ds) from]))

(defn- reindexed [index es i from]
  (if from
    (let [[eid e] (nth es i)] (push/moved index eid from e))
    index))

(defn- island
  [{:keys [es bodies at]}]
  {:es es :index (push/grid-of bodies at)
   :ticking (push/ticks-of bodies at)
   :slots (push/slots-of bodies at)})

(defn- walk
  "Returns the deltas of the island isl stepped in the order of its
  bodies."
  [world tempters t {:keys [es index ticking slots]}]
  (let [vels (object-array (count es))]
    (loop [i 0 es (transient es) index index acc (transient [])]
      (if (= i (count es))
        (persistent! acc)
        (let [[es ds from]
              (turn world ticking vels tempters t index slots es i)]
          (recur (inc i) es (reindexed index es i from)
                 (reduce conj! acc ds)))))))

(defn- step-island [world tempters t h]
  (walk world tempters t (island h)))

(def ^:private ^:const step-reach 0.5)

(defn- mind-of
  "Returns the fn of slot i that lets mob i of the island isl think
  before it moves."
  [world tempters t {:keys [es ticking]} ^objects minds]
  (fn [i]
    (let [i (int i)]
      (when (stepping? ticking es i)
        (let [[eid e] (nth es i)
              m (minded (live world eid) tempters eid e t)]
          (aset minds i m))))))

(defn- placed! [index ^objects cur i eid e e2]
  (when-not (identical? (:pos e) (:pos e2))
    (push/moved index eid (:pos e) e2))
  (aset cur i [eid e2]))

(defn- body-of
  "Returns the fn of slot i that moves mob i of the island isl as it
  thought. It tells when ok? finds the mob out of reach."
  [world t {:keys [ticking index slots]}
   [^objects minds cur ^objects runs] out ok?]
  (fn [i]
    (let [i (int i) m (aget minds i)]
      (when m
        (let [[eid e] (nth cur i)
              w (live world eid)
              [e2 ds shoves] (run-turn w index t slots cur i m)
              ts (takers (:chunks w) ticking slots cur i shoves)]
          (when-not (ok? e e2) (aset ^booleans out 0 true))
          (placed! index cur i eid e e2)
          (aset runs i [ds ts]))))))

(defn- joiner
  "Returns the fn of slot i that adds to collector c the deltas of
  mob i, followed by the shoves it hands to the bodies before it."
  [c ^objects cur ^objects runs]
  (let [vels (object-array (alength runs))
        f (fn [_ [j sh]]
            (when-let [d (hand (nth (aget cur (int j)) 1) vels j sh)]
              (deltas/collect! c d)))]
    (fn [i]
      (when-let [run (aget runs (int i))]
        (deltas/collect-all! c (nth run 0))
        (reduce f nil (nth run 1))))))

(defn- turn-runs
  "Returns the deltas of the island isl in the order of its mobs,
  each run with the shoves it hands to the bodies before it, or nil
  when ok? finds a mob out of reach. Each mob thinks in parallel and
  moves as soon as the mobs it could meet before it moved."
  [world tempters t isl cur ok?]
  (let [n (count (:es isl)) minds (object-array n)
        runs (object-array n) out (boolean-array 1)
        c (deltas/collecting)]
    (push/turns (push/pinned (:index isl) step-reach) step-reach
                (mind-of world tempters t isl minds)
                (body-of world t isl [minds cur runs] out ok?)
                (joiner c cur runs))
    (when-not (aget out 0) (deltas/collected c))))

(defn- near-start? [e e2] (push/within? e e2 step-reach))

(defn- mob? [[_ e]] (mobs/mob-type? (:type e)))

(defn- ahead-island
  "Returns the deltas of herd h, the same as in order, and the count
  of mobs that stepped again. Mobs that cannot meet run in parallel,
  and the herd keeps the runs when ok? finds every mob in reach. Else
  it steps again in order."
  [world tempters t h ok?]
  (let [isl (island h) cur (object-array (:es isl))
        d (turn-runs world tempters t isl cur ok?)]
    (if d
      [d 0]
      [(deltas/of-vec (step-island world tempters t h))
       (count (filter mob? (:es h)))])))

(def ^:private ^:const ahead-bodies 64)

(defn- ahead? [h]
  (and (>= (count (:es h)) ahead-bodies)
       (> (par/threads) 1)))

(defn- island-deltas [world tempters t h]
  (if (ahead? h)
    (nth (ahead-island world tempters t h near-start?) 0)
    (deltas/of-vec (step-island world tempters t h))))

(def ^:private ^:const batch-bodies 32)

(defn- batched [[acc b ^long n] h]
  (let [b (conj b h) n (+ n (count (:es h)))]
    (if (>= n batch-bodies) [(conj acc b) [] 0] [acc b n])))

(defn- batches [islands]
  (let [[acc b] (reduce batched [[] [] 0] islands)]
    (cond-> acc (seq b) (conj b))))

(defn- island-batch [world tempters t batch]
  (let [f (fn [acc h]
            (->> (island-deltas world tempters t h)
                 (deltas/merge acc)))]
    (reduce f deltas/empty-deltas batch)))

(defn- biting? [active t [_ e]]
  (when-let [f (:bites? (spec/of (:type e)))]
    (and (f e t) (areas/active-at? active (:pos e)))))

(defn- ended? [active [_ e]]
  (and (mobs/mob-type? (:type e)) (mobs/death-ends? e)
       (areas/active-at? active (:pos e))))

(defn- kept! [^objects acc i entry]
  (aset acc i (conj! (aget acc i) entry)))

(defn- clocked
  "Returns entry as its mob ticks at t, thawed when it ticks again
  after a freeze. It adds to acc the change that freezes or thaws the
  mob."
  [^objects acc active t [eid e :as entry]]
  (let [on? (areas/active-at? active (:pos e))
        frozen? (clock/frozen? e)]
    (cond
      (and on? frozen?)
      (let [m (clock/thawed e t)]
        (kept! acc 3 [:merge-entity eid m])
        (MapEntry/create eid (merge e m)))
      (not (or on? frozen?))
      (do (kept! acc 3 [:merge-entity eid (clock/frozen t)]) entry)
      :else entry)))

(defn- steps? [active [_ e :as entry]]
  (and (mob? entry) (areas/active-at? active (:pos e))))

(defn- sorted! [^objects acc held active t entry]
  (cond
    (push/body? held entry)
    (do
      (push/add-body (aget acc 0) entry (steps? active entry))
      (when (biting? active t entry) (kept! acc 1 entry)))
    (ended? active entry) (kept! acc 2 entry)))

(defn- scan
  "Returns the fn that adds entry to the bodies, the biters or the
  endings in acc, and the change to the clocks of its mob."
  [held active t]
  (fn [^objects acc entry]
    (let [entry (if (mob? entry) (clocked acc active t entry) entry)]
      (sorted! acc held active t entry)
      acc)))

(defn- scanned ^objects []
  (object-array [(push/bodies) (transient []) (transient [])
                 (transient [])]))

(defn- kept-all! [^objects acc ^objects o i]
  (aset acc i (reduce conj! (aget acc i) (persistent! (aget o i)))))

(defn- scans-joined
  ([] (scanned))
  ([^objects acc ^objects o]
   (push/joined-bodies (aget acc 0) (aget o 0))
   (kept-all! acc o 1)
   (kept-all! acc o 2)
   (kept-all! acc o 3)
   acc))

(def ^:private ^:const scan-leaf 128)

(defn- scanned-all
  "Returns the scan of each entry of map m by f, in key order. The
  parts of a long map scan in parallel."
  ^objects [f m]
  (if (instance? LongMap m)
    (r/fold scan-leaf scans-joined
            (fn [acc k v] (f acc (MapEntry/create k v))) m)
    (reduce f (scanned) m)))

(defn- herd-of [b at]
  (let [es (push/entries-of b at)]
    (when (some mob? es) {:es es :bodies b :at at})))

(defn- herds
  "Returns the herds of world, which are the islands of bodies with
  a mob among them, with the mobs that bite this tick, the mobs whose
  death ends and the changes to the clocks of mobs. All four are in
  id order."
  [world active t]
  (let [f (scan (areas/loaded-zone world) active t)
        acc (scanned-all f (:entities world))
        b (aget acc 0)]
    [(into [] (keep #(herd-of b %)) (push/groups b))
     (persistent! (aget acc 1)) (persistent! (aget acc 2))
     (persistent! (aget acc 3))]))

(defn- clocks-set [world clocks]
  (let [f (fn [es [_ eid m]] (assoc es eid (merge (get es eid) m)))]
    (update world :entities #(reduce f % clocks))))

(defn- bitten [world tempters t [eid e]]
  (let [ds (nth (minded (live world eid) tempters eid e t) 2)]
    (overlay/wrote world eid ds)))

(defn- seen
  "Returns world with the bites of this tick in the overlay. A mob
  bites in its turn and sees the writes of each turn before it."
  [world tempters t biters]
  (let [world (assoc world :watchers (animal/watchers world))
        bf (fn [w m] (bitten w tempters t m))]
    (reduce bf world biters)))

(defn- endings [ends]
  (into [] (mapcat (fn [[eid e]] (living/ended eid e))) ends))

(defn- herds-deltas [world tempters t hs]
  (->> (batches hs)
       (deltas/fold-merged #(island-batch world tempters t %))))

(defn- before-turns
  "Returns world before the turns of its mobs at tick t, its herds,
  and the deltas that start or stop the clocks of the mobs."
  [world active t]
  (let [[_ _ _ clocks :as h] (herds world active t)]
    [(clocks-set world clocks) h clocks]))

(defn turns
  "Returns the deltas of the mobs in one tick, each in its turn.
  Each island of mobs steps on its own."
  [world _d]
  (let [t (long (:tick world))
        active (areas/active-chunks world)
        [world [hs biters ends] pre]
        (before-turns world active t)
        tempters (sense/holders world)
        world (seen (sense/indexed world) tempters t biters)]
    (deltas/merge
      (deltas/of-vec pre)
      (herds-deltas world tempters t hs)
      (deltas/of-vec (endings ends)))))
