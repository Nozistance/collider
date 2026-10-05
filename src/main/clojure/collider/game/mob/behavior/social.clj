(ns collider.game.mob.behavior.social
  "Behaviours that turn a mob to others."
  (:require [collider.game.entity :as entity]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.behavior.core :as c]
            [collider.game.mob.brain :as b]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sensor :as sensor]
            [collider.num :as num]
            [collider.random :as random]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(def ^:private ^:const forever b/forever)

(defn- declined [e] [e [] false])

(defn- ticked-down
  "Returns mob e after the Ticker of behaviour i, declined unless it
  comes to zero now."
  [e eid t i interval]
  (let [n (long (:ticks (b/slot e i) 0))]
    (if (zero? n)
      (let [m (dec (c/sample t eid i :interval interval))]
        (declined (b/with-slot e i {:ticks m})))
      (let [e (b/with-slot e i {:ticks (dec n)})]
        (if (== n 1) e (declined e))))))

(defn- glance [kind max-dist interval]
  (let [d (num/f32 max-dist) r2 (num/fmul d d)]
    (fn [w eid e t i]
      (let [ok? (fn [_ o]
                  (and (= kind (:type o))
                       (<= (v/dist-sq (:pos o) (:pos e)) r2)))]
        (let [[oid e] (sensor/closest w eid e t ok?)]
          (if (nil? oid)
            (declined e)
            (let [r (ticked-down e eid t i interval)
                  tr (c/at-entity oid true)]
              (if (vector? r)
                r
                (b/remember r :look-target tr forever)))))))))

(defn set-entity-look-target-sometimes [kind max-dist interval]
  {:id :set-entity-look-target-sometimes
   :needs {:look-target :absent
           :nearest-visible-living-entities :present}
   :one-shot (glance kind max-dist interval)})

(defn- sq ^double [^double x] (* x x))

(defn- followed [e aid speed lo]
  (let [to (c/at-entity aid false false)
        wt (c/walk-target to speed (dec (long lo)))]
    (-> (b/remember e :look-target (c/at-entity aid true false)
                    forever)
        (b/remember :walk-target wt forever))))

(defn baby-follow-adult [[lo hi] speed]
  {:id :baby-follow-adult
   :needs {:nearest-visible-adult :present :look-target :registered
           :walk-target :absent}
   :one-shot
   (fn [w _ e t _]
     (let [aid (b/recall e :nearest-visible-adult t)
           o (c/other w aid)]
       (when (and (mobs/baby? e) o)
         (let [d2 (v/dist-sq (:pos e) (:pos o))]
           (when (and (< d2 (sq (inc (long hi))))
                      (>= d2 (sq (double lo))))
             (followed e aid speed lo))))))})

(defn- tempted-tick [speed-of close-of eyes?]
  (fn [w _ e t _]
    (let [pid (b/recall e :tempting-player t)
          o (c/other w pid)
          e (b/remember e :look-target (c/at-entity pid true) forever)
          close (double (close-of e))]
      (if (< (v/dist-sq (:pos e) (:pos o)) (sq close))
        (b/erase e :walk-target)
        (let [to (c/at-entity pid eyes? eyes?)]
          (b/remember e :walk-target (c/walk-target to (speed-of e) 2)
                      forever))))))

(defn- tempted? [_ _ e t _]
  (and (b/present? e :tempting-player t)
       (not (b/present? e :breed-target t))
       (not (b/present? e :is-panicking t))))

(defn- untempted [_ _ e t _]
  (reduce b/erase (c/cool e :temptation-cooldown-ticks 100 t)
          [:is-tempted :walk-target :look-target]))

(def ^:private tempt-needs
  {:look-target :registered :walk-target :registered
   :temptation-cooldown-ticks :absent :is-tempted :absent
   :tempting-player :present :breed-target :absent
   :is-panicking :absent})

(defn follow-temptation
  ([speed-of] (follow-temptation speed-of (constantly 2.5)))
  ([speed-of close-of] (follow-temptation speed-of close-of false))
  ([speed-of close-of eyes?]
   {:id :follow-temptation :duration :never :needs tempt-needs
    :continue? tempted?
    :start (fn [_ _ e _ _] (b/remember e :is-tempted true forever))
    :tick (tempted-tick speed-of close-of eyes?) :stop untempted}))

(defn- panicking? [e t] (b/present? e :is-panicking t))

(defn- mates? [eid e t oid o]
  (and (not= eid oid) (= (:type e) (:type o))
       (mobs/in-love? e t) (mobs/in-love? o t)))

(defn- partner-of [w eid e t kind]
  (let [ok? (fn [oid o]
              (and (= kind (:type o)) (mates? eid e t oid o)
                   (not (panicking? o t))))]
    (sensor/closest w eid e t ok?)))

(defn- gazed [e oid speed close]
  (let [tr (c/at-entity oid true)]
    (-> (b/remember e :look-target tr forever)
        (b/remember :walk-target (c/walk-target tr speed close)
                    forever))))

(defn- gazes [pid eid speed close]
  (let [tr (c/at-entity eid true)]
    [[:remember pid :look-target tr forever]
     [:remember pid :walk-target (c/walk-target tr speed close)
      forever]]))

(defn- love-start [kind speed close]
  (fn [w eid e t i]
    (let [[pid e] (partner-of w eid e t kind)
          at (+ (long t) 60 (random/below (c/roll t eid i :child) 50))
          e (-> (b/remember e :breed-target pid forever)
                (gazed pid speed close)
                (assoc-in [:brain :running i :spawn] at))]
      [e (into [[:remember pid :breed-target eid forever]]
               (gazes pid eid speed close))])))

(defn- loving? [kind]
  (fn [w eid e t i]
    (let [pid (b/recall e :breed-target t)
          o (c/other w pid)]
      (if (and o (= kind (:type o)) (entity/alive? o)
               (mates? eid e t pid o))
        (let [[s e] (sensor/sees w eid e t pid)]
          [(and s (<= (long t) (long (:spawn (b/run-of e i))))
                (not (panicking? e t)) (not (panicking? o t)))
           e])
        false))))

(defn- love-tick [speed close spec]
  (fn [w eid e t i]
    (let [pid (b/recall e :breed-target t)
          o (c/other w pid)
          e (gazed e pid speed close)
          ds (gazes pid eid speed close)]
      (if (and (< (v/dist-sq (:pos e) (:pos o)) 9.0)
               (>= (long t) (long (:spawn (b/run-of e i)))))
        (let [[e bred] (animal/bred spec w eid pid e o t)]
          [(b/erase e :breed-target)
           (conj (into ds bred) [:remember pid :breed-target nil 0])])
        [e ds]))))

(defn- unloved [_ _ e _ _]
  (reduce b/erase e [:breed-target :walk-target :look-target]))

(def ^:private love-needs
  {:nearest-visible-living-entities :present :breed-target :absent
   :walk-target :registered :look-target :registered
   :is-panicking :absent})

(defn animal-make-love
  "Returns the behaviour that walks to a partner of kind in love and
  breeds with it. The newborn looks as child-look says."
  ([kind] (animal-make-love kind 1.0 2))
  ([kind speed close]
   (animal-make-love kind speed close (fn [_ _ _ a _] (:variant a))))
  ([kind speed close child-look]
   {:id :animal-make-love :duration [110 110] :needs love-needs
    :check (fn [w eid e t _]
             (if (mobs/in-love? e t)
               (update (partner-of w eid e t kind) 0 some?)
               [false e]))
    :start (love-start kind speed close)
    :continue? (loving? kind)
    :tick (love-tick speed close {:child-look child-look})
    :stop unloved :tells? mobs/in-love?}))
