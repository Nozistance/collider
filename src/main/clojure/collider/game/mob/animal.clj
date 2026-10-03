(ns collider.game.mob.animal
  "Farm animal goals and their selector."
  (:require [collider.game.entity :as entity]
            [collider.game.inventory :as inventory]
            [collider.game.mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.nav :as nav]
            [collider.game.mob.randompos :as pos]
            [collider.game.mob.sense :as sense]
            [collider.game.orb :as orb]
            [collider.game.out :as out]
            [collider.game.level :as level]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.phys :as phys]
            [collider.world.chunk :as chunk]
            [collider.world.space.sight :as sight])
  (:import (clojure.lang IFn)
           (collider.game.mob GoalSelector)))

(set! *warn-on-reflection* true)

(def ^:private ^:const stroll-interval 120)

(def ^:private ^:const stroll-idle 100)

(def ^:private ^:const stroll-swim 0.001)

(def ^:private ^:const look-chance 0.02)

(def ^:private ^:const look-ticks 40)

(def ^:private ^:const look-around-ticks 20)

(def ^:private ^:const love-ticks 600)

(def ^:private ^:const mate-ticks 60)

(def ^:private ^:const breed-cooldown 6000)

(def ^:private ^:const baby-ticks 24000)

(def ^:private ^:const breed-range-sq 64.0)

(def ^:private ^:const breed-near-sq 9.0)

(def ^:private ^:const default-look-range 6.0)

(def ^:private ^:const sight-range-sq 16384.0)

(def ^:private ^:const calm-ticks 100)

(def ^:private ^:const follow-near-sq 9.0)

(def ^:private ^:const follow-far-sq 256.0)

(def ^:private ^:const idle-reset-sq 1024.0)

(def ^:private ^:const feeding-speedup 0.1)

(def ^:private ^:const ticks-per-second 20)

(def ^:private ^:const jump-threshold 0.4)

(def ^:private ^:const float-jump-chance 0.8)

(def rnd
  "Returns a number from 0 to 1 decided by tick t, mob eid and key k."
  pos/rnd)

(def one-in?
  "Returns true with a chance of one in n."
  pos/one-in?)

(defn- reduced-delay
  "Returns ticks as passes of the goal selector, which runs on every
  second tick."
  ^long [^long ticks]
  (quot (inc ticks) 2))

(def ^:private speeds
  "The speed of each goal for each breed.
  A goal left out walks at one."
  {:cow       {:panic 2.0 :tempt 1.25 :follow 1.25}
   :mooshroom {:panic 2.0 :tempt 1.25 :follow 1.25}
   :sheep     {:panic 1.25 :tempt 1.1 :follow 1.1}
   :pig       {:panic 1.25 :tempt 1.2 :stick-tempt 1.2 :follow 1.1}
   :chicken   {:panic 1.4 :follow 1.1}
   :rabbit    {:panic 2.2 :mate 0.8 :tempt 1.0 :avoid 2.2
               :raid (double (float 0.7)) :wander 0.6}})

(def ^:private look-ranges
  "The range of the look at player goal for each breed that looks
  further than six blocks."
  {:rabbit 10.0})

(defn- look-range ^double [e]
  (double (get look-ranges (:type e) default-look-range)))

(defn goal-speed
  "Returns the speed modifier mob e walks goal k at."
  ^double [e k]
  (double (get (speeds (:type e)) k 1.0)))

(defn- roaming? [_ e _ _]
  (not (nav/done? e)))

(def ^:private ^:const water-reach 5)

(def ^:private water-scan
  (let [r water-reach]
    (vec (for [d (range (+ r 1 r 1))
               x (range (- (min r d)) (inc (min r d)))
               :let [my (min 1 (- d (Math/abs (long x))))]
               y (range (- my) (inc my))
               :let [z (- d (Math/abs (long x)) (Math/abs (long y)))]
               :when (<= z r)
               dz (if (zero? z) [0] [z (- z)])]
           [x y dz]))))

(defn- shifted [[x y z] [dx dy dz]]
  [(+ (long x) (long dx)) (+ (long y) (long dy))
   (+ (long z) (long dz))])

(defn- look-for-water [world e]
  (let [chunks (:chunks world)
        [x y z :as here] (sense/feet-cell (:pos e))]
    (when (empty? (block/collision-boxes
                    (chunk/block-state chunks x y z)))
      (first (filter #(pos/water? chunks %)
                     (map #(shifted here %) water-scan))))))

(defn- other [world oid] (get (:entities world) oid))

(defn- glance [e oid t]
  (assoc e :look {:target oid :until (+ (long t) 2)}))

(defn- panic-pos [world eid e t]
  (or (when (:burning? e) (look-for-water world e))
      (pos/default-pos world e t eid :panic 5 4)))

(defn- start-panic [world eid e t _]
  (when (mobs/panicking? e t)
    (when-let [cell (panic-pos world eid e t)]
      [(nav/move-to world (assoc e :task {:kind :panic}) cell
                    (goal-speed e :panic))
       nil])))

(defn- partner? [eid e t oid o]
  (and (not= oid eid) (= (:type e) (:type o))
       (mobs/in-love? o t) (not (mobs/panicking? o t))
       (< (v/dist-sq (:pos e) (:pos o)) breed-range-sq)))

(defn- partner-for [world eid e t]
  (let [pred #(partner? eid e t %1 %2)]
    (second (sense/nearest world (:pos e) breed-range-sq pred))))

(defn- start-mate [world eid e t _]
  (when (mobs/in-love? e t)
    (when-let [pid (partner-for world eid e t)]
      [(assoc e :task {:kind :mate :partner pid :love 0}) nil])))

(defn- mating? [world e t _]
  (let [o (other world (:partner (:task e)))]
    (and o (mobs/in-love? o t) (not (mobs/panicking? o t))
         (< (long (:love (:task e) 0)) mate-ticks))))

(defn- newborn [spec world t eid e o]
  (let [color ((:child-color spec) world t eid e o)]
    (assoc (mobs/new-mob (:type e) (:pos e) color t)
           :baby-until (+ (long t) baby-ticks)
           :arrived [(* 2 (long t)) (* 2 (long eid))])))

(defn- breeding-orb
  "Returns the orb of 1 to 7 points that breeding drops at mob e."
  [eid e t]
  (let [roll #(random/of-key t eid [:breed-orb %])
        value (inc (long (* 7.0 (double (roll :value)))))]
    (orb/make (:pos e) value roll)))

(defn- bred [spec world eid pid e o t]
  (let [cooled {:love-until 0
                :breed-ready-at (+ (long t) breed-cooldown)}]
    [(assoc e :task nil)
     (cond-> [[:spawn-entity (newborn spec world t eid e o)]
              [:merge-entity eid cooled]
              [:merge-entity pid (assoc cooled :task nil)]
              (out/all (out/status eid :love))
              (out/all (out/status pid :love))]
       (get-in world [:rules :mob-drops] true)
       (conj [:spawn-entity (breeding-orb eid e t)]))]))

(defn- mate-tick [spec world eid e t _]
  (let [pid (:partner (:task e))
        o (other world pid)
        speed (goal-speed e :mate)
        e (nav/move-to-entity world (glance e pid t) o speed)
        e (update-in e [:task :love] (fn [n] (inc (long n))))
        due (reduced-delay mate-ticks)]
    (if (and (>= (long (:love (:task e))) due)
             (< (v/dist-sq (:pos e) (:pos o)) breed-near-sq)
             (< (long eid) (long pid)))
      (bred spec world eid pid e o t)
      [e nil])))

(defn- tempt-range ^double [e]
  (mobs/attribute (:type e) :tempt-range))

(defn- tempting [e lure? [pid items p]]
  (when (and (some lure? items)
             (sense/in-range? (:pos e) p (tempt-range e)))
    [(v/dist-sq (:pos e) (:pos p)) pid]))

(defn- tempter [e lures tempters]
  (when (seq tempters)
    (let [lure? (lures (:type e))]
      (->> tempters
           (keep #(tempting e lure? %))
           (sort-by first)
           first
           second))))

(def ^:private ^:const tempt-stop-sq 6.25)

(defn- tempt-tick [kind lures]
  (fn [_ world _ e t tempters]
    (let [pid (tempter e lures tempters)
          o (other world pid)
          e (glance (assoc-in e [:task :player] pid) pid t)]
      [(cond
         (nil? o) e
         (< (v/dist-sq (:pos e) (:pos o)) tempt-stop-sq) (nav/stop e)
         :else (nav/move-to-entity world e o (goal-speed e kind)))
       nil])))

(defn tempt
  "Returns the goal to follow the nearest player who holds what lures
  gives for the breed. Under key calm it calms down for a while when
  it lets go."
  [kind calm lures]
  {:kind kind :flags #{:move :look}
   :start (fn [_ _ e t tempters]
            (when (>= (long t) (long (or (get e calm) 0)))
              (when-let [pid (tempter e lures tempters)]
                [(assoc e :task {:kind kind :player pid}) nil])))
   :tick (tempt-tick kind lures)
   :continue? (fn [_ e _ tempters] (some? (tempter e lures tempters)))
   :stop (fn [e t]
           (let [until (+ (long t) calm-ticks)]
             (nav/stop (assoc e :task nil calm until))))})

(defn- parent? [eid e p oid o]
  (let [q (:pos o)
        d (fn [f] (Math/abs (- (double (f q)) (double (f p)))))]
    (and (not= oid eid) (= (:type e) (:type o)) (not (mobs/baby? o))
         (<= (double (d v/y)) 4.0) (<= (double (d v/x)) 8.0)
         (<= (double (d v/z)) 8.0))))

(defn- parent-for [world eid e]
  (let [p (:pos e)
        pred #(parent? eid e p %1 %2)]
    (when-let [[d2 oid] (sense/nearest world p follow-far-sq pred)]
      (when (>= (double d2) follow-near-sq) oid))))

(def ^:private ^:const follow-repath 20)

(defn- start-follow [world eid e t _]
  (when (mobs/baby? e)
    (when-let [oid (parent-for world eid e)]
      [(assoc e :follow oid :follow-at t) nil])))

(defn- follow-tick [_ world _ e t _]
  (let [o (other world (:follow e))]
    [(if (and o (>= (long t) (long (:follow-at e 0))))
       (assoc (nav/move-to-entity world e o (goal-speed e :follow))
         :follow-at (+ (long t) follow-repath))
       e)
     nil]))

(defn- unfollowed [e _]
  (assoc e :follow nil))

(defn- following? [world e _ _]
  (let [o (other world (:follow e))]
    (and (mobs/baby? e) o
         (<= follow-near-sq (v/dist-sq (:pos e) (:pos o))
             follow-far-sq))))

(defn- stroll-pos [world eid e t]
  (cond
    (:wet? e) (or (pos/land-pos world e t eid :stroll-wet 15 7)
                  (pos/default-pos world e t eid :stroll 10 7))
    (>= (rnd t eid :swim) stroll-swim)
    (pos/land-pos world e t eid :stroll-land 10 7)
    :else (pos/default-pos world e t eid :stroll 10 7)))

(defn- start-wander [world eid e t _]
  (when (and (< (long (or (:no-action e) 0)) stroll-idle)
             (one-in? t eid :stroll (reduced-delay stroll-interval)))
    (when-let [cell (stroll-pos world eid e t)]
      [(nav/move-to world (assoc e :task {:kind :wander}) cell
                    (goal-speed e :wander))
       nil])))

(defn- look-goal [e t]
  (let [l (:look e)]
    (when (and (= :look-player (:kind l))
               (> (long (:until l)) (long t)))
      l)))

(defn- eye-of [e ^double h]
  (let [p (:pos e)] [(v/x p) (+ (v/y p) h) (v/z p)]))

(defn in-sight?
  "Returns true when mob e sees the eyes of o from its own."
  [world e o]
  (let [from (eye-of e (mobs/eye-height e))
        to (eye-of o (entity/eye-height o))]
    (and (<= (v/dist-sq from to) sight-range-sq)
         (sight/clear? (:chunks world) from to))))

(defn- noticed? [world e o]
  (and (game-mode/seen? o)
       (sense/in-range? (:pos e) o (look-range e))
       (in-sight? world e o)))

(defn- nearer [world e eye best [pid p]]
  (let [d2 (v/dist-sq eye (:pos p))]
    (if (and (or (nil? best) (< d2 (double (best 0))))
             (noticed? world e p))
      [d2 pid p]
      best)))

(defn- player-to-look-at
  "Returns the player nearest the eyes of mob e among those it
  notices, with its distance squared and its id."
  [world e]
  (let [eye (eye-of e (mobs/eye-height e))]
    (reduce #(nearer world e eye %1 %2) nil
            (level/player-entries world))))

(defn- look-until [t eid]
  (+ (long t) look-ticks
     (long (* look-ticks (rnd t eid :look-time)))))

(defn- start-look-player [world eid e t _]
  (when (< (rnd t eid :look) look-chance)
    (when-let [[_ pid] (player-to-look-at world e)]
      [(assoc e :look {:kind :look-player :target pid
                       :until (look-until t eid)})
       nil])))

(defn- looking? [world e _ _]
  (let [o (other world (:target (:look e)))]
    (and o (<= (v/dist-sq (:pos e) (:pos o))
               (let [r (look-range e)] (* r r))))))

(defn- start-look-around [_ eid e t _]
  (when (< (rnd t eid :around) look-chance)
    (let [n (* look-around-ticks (rnd t eid :around-time))
          yaw (- (* 360.0 (rnd t eid :around-yaw)) 180.0)]
      [(assoc e :task {:kind :look-around
                       :until (+ (long t) look-around-ticks (long n))}
              :look {:yaw yaw :until Long/MAX_VALUE})
       nil])))

(defn- looking-around? [_ e t _]
  (>= (long (:until (:task e))) (long t)))

(defn- afloat? [world e _ _]
  (let [[half height] (mobs/box-of e)
        chunks (:chunks world)
        h (if (phys/dry? chunks (:pos e) half height)
            0.0
            (liquid/fluid-height chunks (:pos e) half height :water))]
    (or (> h jump-threshold) (boolean (:in-lava? e)))))

(defn- start-float [world eid e t tempters]
  (when (afloat? world e t tempters) [(assoc e :float? true) nil]))

(defn- float-tick [_ _ eid e t _]
  [(cond-> e (< (rnd t eid :float) float-jump-chance)
           (assoc :jump true))
   nil])

(def goals
  "The goals every farm animal has, highest priority first."
  [{:kind     :float :flags #{:jump} :every-tick? true
    :start    start-float :continue? afloat? :tick float-tick
    :running? (fn [e _] (boolean (:float? e)))
    :stop     (fn [e _] (assoc e :float? nil))}
   {:kind :panic :flags #{:move} :start start-panic
    :continue? roaming?}
   {:kind :mate :flags #{:move :look} :start start-mate
    :continue? mating? :tick mate-tick}
   (tempt :tempt :tempt-cooldown-until mobs/food)
   {:kind :follow :flags #{} :start start-follow :continue? following?
    :running? (fn [e _] (some? (:follow e))) :tick follow-tick
    :stop unfollowed}
   {:kind :wander :flags #{:move} :start start-wander
    :continue? roaming?
    :stop (fn [e _] (nav/stop (assoc e :task nil)))}
   {:kind :look-player :flags #{:look} :start start-look-player
    :continue? looking?
    :running? (fn [e t] (some? (look-goal e t)))
    :stop (fn [e _] (assoc e :look nil))}
   {:kind :look-around :flags #{:move :look} :start start-look-around
    :continue? looking-around?
    :stop (fn [e _] (assoc e :task nil :look nil))}])

(defn goal
  [k]
  (first (filter #(= k (:kind %)) goals)))

(def ^:private watcher? (complement game-mode/spectator?))

(defn watchers
  "Returns the players that keep mobs from idling, as sense/watchers
  gives them, for the key :watchers of the world a brain sees."
  [world]
  (sense/watchers world watcher?))

(defn- watched? [world e]
  (if-let [ws (:watchers world)]
    (sense/watched? ws (:pos e) idle-reset-sq)
    (sense/player-within? world (:pos e) idle-reset-sq watcher?)))

(defn- idle-count [world e]
  (if (watched? world e) 0 (long (or (:no-action e) 0))))

(def ^:private flag-bits
  {:move 1 :look 2 :jump 4 :target 8})

(defn- mask-of ^long [flags]
  (reduce (fn [m f]
            (if-let [b (flag-bits f)]
              (bit-or (long m) (long b))
              (throw (ex-info "unknown goal flag" {:flag f}))))
          0 flags))

(defn- ranked [i g]
  (assoc g :prio (:prio g i) :mask (mask-of (:flags g))))

(defn- fns [gs k] (into-array IFn (map k gs)))

(defn spec
  "Returns the spec of a breed from its goals, highest priority first.
  A goal ranks by its place unless it names its priority. Each goal
  gets its flags as a bit mask too. The spec also picks the
  colour of a newborn from both parents."
  ([goals] (spec goals (fn [_ _ _ a _] (:color a))))
  ([goals child-color]
   (let [gs (vec (map-indexed ranked goals))]
     (GoalSelector.
       gs child-color (count gs) (object-array (map :kind gs))
       (long-array (map :prio gs)) (long-array (map :mask gs))
       (fns gs :running?) (fns gs :stop)
       (fns gs :start) (fns gs :continue?) (fns gs :tick)
       (boolean-array (map (comp boolean :every-tick?) gs))))))

(defn- full-pass?
  "Returns true when mob eid runs its goals on tick t. The parity of
  eid picks every second tick, and a new mob runs them twice first."
  [eid e t]
  (let [b (:born e)]
    (or (even? (+ (long t) (long eid)))
        (and (some? b) (<= (- (long t) (long b)) 1)))))

(defn brain
  "Returns the mob and its deltas after one tick of its goals.
  Goals run on every second tick, and on the first two. A goal that
  wants every tick gets every tick."
  [spec world eid e t tempters]
  (let [n (inc (long (idle-count world e)))
        e (if (== n (long (or (:no-action e) 0)))
            e
            (assoc e :no-action n))]
    (GoalSelector/think spec world eid e t tempters
                        (full-pass? eid e t))))

(defn egg-result
  "Returns what a spawn egg of the mob's own kind does to it.
  It hatches a baby bred from that one parent. The breed spec of each
  mob type comes from specs."
  [specs {:keys [world t peid p eid e hand item]}]
  (when-let [spec (specs (:type e))]
    (when (= (:type e) (mobs/egg-type item))
      {:result :success-server
       :deltas (cons [:spawn-entity (newborn spec world t eid e e)]
                     (inventory/consume-deltas peid p hand 1))})))

(defn- feedable? [e t]
  (and (not (mobs/baby? e))
       (not (mobs/in-love? e t))
       (<= (long (or (:breed-ready-at e) 0)) (long t))))

(defn- fed-growth ^long [^long remaining]
  (let [s (double (quot remaining ticks-per-second))
        seconds (long (* s feeding-speedup))]
    (- remaining (* seconds ticks-per-second))))

(defn- grown [t target e]
  (let [left (max 0 (- (long (:baby-until e)) (long t)))]
    [[:merge-entity target
      {:baby-until (+ (long t) (fed-growth left))}]]))

(defn- loved [t eid]
  [[:merge-entity eid {:love-until (+ (long t) love-ticks)}]
   (out/all (out/status eid :love))])

(defn- eaten
  "Returns the eating sound of mob e for the breeds that eat aloud."
  [t eid e]
  (when-let [snd (mobs/eating-sound e)]
    (let [r #(random/of-key t eid [:eat %])
          base (if (mobs/baby? e) 1.5 1.0)
          pitch (+ base (* 0.2 (- (double (r 1)) (double (r 2)))))]
      [(out/all (out/sound snd (:pos e) 1.0 pitch))])))

(defn feed-result
  "Returns what the breeding food of the mob does to it.
  A grown mob falls in love and a baby grows up sooner. A mob that may
  do neither leaves the food alone."
  [{:keys [world t peid p eid e hand item]}]
  (when (contains? (mobs/food (:type e)) item)
    (let [used (inventory/use-item-deltas world peid p hand)
          ate (eaten t eid e)]
      (cond
        (feedable? e t)
        {:result :success-server
         :deltas (concat used (loved t eid) ate)}
        (mobs/baby? e)
        {:result :success
         :deltas (concat used (grown t eid e) ate)}))))
