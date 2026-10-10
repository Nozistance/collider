(ns collider.game.entity
  "Entity constructors, saving and loading."
  (:require [collider.data :as data]
            [collider.game.entity.gen :as gen]
            [collider.game.entity.records :as types]
            [collider.game.entity.size :as size]
            [collider.game.hanging :as hanging]
            [collider.game.mob.brain :as brain]
            [collider.game.mob.mobs :as mobs]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block])
  (:import (clojure.lang PersistentHashMap)
           (collider.game.entity.records Item Mob Orb)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(def thrown-types
  #{:snowball :egg :ender-pearl :splash-potion :lingering-potion
    :experience-bottle :wind-charge})

(defn player?
  [e]
  (= :player (:type e)))

(defn removed?
  "Returns true when e is a player dead long enough to have left its
  level. It comes back when it respawns."
  [e]
  (and (player? e)
       (>= (long (or (:death-time e) 0)) mobs/death-ticks)))

(defn tick-count
  "Returns the ticks entity e has lived at the tick of world, in its
  turn. A player counts the tick it joined in."
  ^long [world e]
  (cond-> (- (long (:tick world)) (long (:born e 0)))
    (player? e) inc))

(defn living?
  "Returns true when e is a player or a mob."
  [e]
  (or (player? e) (mobs/mob-type? (:type e))))

(defn alive?
  "Returns true when e is there and has health left. A body without
  health counts as alive."
  [e]
  (and (some? e) (pos? (double (:health e 1.0)))))

(defn item
  "Returns a dropped item entity of stack at pos.
  It moves with velocity vel and cannot be picked up for delay ticks."
  ([pos vel stack] (item pos vel stack 10))
  ([pos vel stack delay]
   {:type :item :pos pos :vel vel :yaw 0.0 :pitch 0.0
    :on-ground false :stack stack :age 0 :pickup-delay delay
    :health 5.0}))

(defn pop-velocity
  "Returns the velocity a stack leaves its holder with, drawn from the
  random keys ks."
  [ks]
  (let [r (fn [k] (random/of-key (conj ks k)))]
    [(- (* 0.2 (r :vx)) 0.1) 0.2 (- (* 0.2 (r :vz)) 0.1)]))

(defn- compact [e]
  (let [x (gen/extmap e)]
    (if (and (instance? PersistentHashMap x) (<= (count x) 8))
      (gen/with-extmap e (into {} x))
      e)))

(defn- record-of [m]
  (case (:type m)
    :player (types/map->Player m)
    :item (types/map->Item m)
    :experience-orb (types/map->Orb m)
    :tnt (types/map->Tnt m)
    :falling-block (types/map->FallingBlock m)
    :area-effect-cloud (types/map->Cloud m)
    (:painting :item-frame :glow-item-frame) (types/map->Hanging m)
    (:snowball :egg :ender-pearl :splash-potion :lingering-potion
     :experience-bottle)
    (types/map->Projectile m)
    (compact (types/map->Mob m))))

(defn of
  "Returns the entity m as the record its type calls for."
  [m]
  (if (record? m)
    m
    (record-of (cond-> m
                 (:pos m) (assoc :pos (v/v3 (:pos m)))
                 (:vel m) (assoc :vel (v/v3 (:vel m)))))))

(defn uuid-of
  [eid e]
  (or (:uuid e) (UUID. (long eid) (long eid))))

(defn pose-box
  "Returns the half width and height of a player in pose."
  [pose]
  (size/type-box :player pose))

(defn pose-eye
  ^double [pose]
  (size/pose-eye :player pose))

(defn eye-height
  "Returns how far above its position the entity e looks out."
  ^double [e]
  (size/eye e))

(defn box
  "Returns the half width and the height of entity e."
  [e]
  (size/box e))

(def ^:private mob-merged (gen/merger Mob))

(def ^:private item-merged (gen/merger Item))

(def ^:private orb-merged (gen/merger Orb))

(defn merged
  [e m]
  (cond (< (count m) 2) (merge e m)
        (instance? Mob e) (mob-merged e m)
        (instance? Item e) (item-merged e m)
        (instance? Orb e) (orb-merged e m)
        :else (merge e m)))

(defn mob-changes
  "Returns the fields of mob new that a turn changed from mob old."
  [old new]
  (gen/diff-fields old new :pos :vel :yaw :pitch :on-ground :task
                   :follow :no-action :baby-until :breed-ready-at
                   :tempt-cooldown-until :say-tick
                   :stick-cooldown-until :egg-at :walked :head-yaw
                   :look :jump-cd :wet? :sheared? :nav :move :jump
                   :body :follow-at :in-lava? :float? :support
                   :no-blocks? :arrived :hop :fall :brain
                   :love-until :stuck))

(defn- plain [v] (if (v/v3? v) (vec v) v))

(defn- kind [type]
  (cond (contains? thrown-types type) :thrown
        (contains? hanging/types type) :hanging
        (#{:item :tnt :falling-block :area-effect-cloud
           :experience-orb} type) type
        :else :mob))

(def ^:private kept
  {:mob [:health :death-time :color :variant :sheared?
         :sound-variant :effects :absorption :fall :stew
         :forced-age :age-locked? :last-hurt-by-player
         :armadillo-state :screaming? :left-horn? :right-horn?]
   :item [:stack :age :pickup-delay :health]
   :experience-orb [:value :count :age :health]
   :tnt [:fuse :origin :owner]
   :falling-block [:block :time :fall :hurt :hurt-max]
   :thrown [:owner :left-owner? :stack]
   :hanging [:block-pos :facing :variant :stack :rotation]})

(def ^:private defaults
  {:mob {:death-time 0 :sheared? false}
   :item {:age 0 :pickup-delay 0 :health 5.0}
   :experience-orb {:value 0 :count 1 :age 0 :health 5.0}
   :tnt {:fuse 80}
   :falling-block {:time 0}
   :thrown {:left-owner? false}})

(def ^:private fresh
  {:mob {:task nil :no-action 0}
   :thrown {:age 0}})

(def ^:private timers
  [:love-until :baby-until :breed-ready-at :egg-at
   :hurt-by-player-until])

(def ^:private expired {:love-until 0 :breed-ready-at 0})

(defn- base [e]
  (cond-> {:type (:type e) :pos (plain (:pos e)) :vel (plain (:vel e))
           :yaw (:yaw e) :pitch (:pitch e) :on-ground (:on-ground e)}
    (seq (:tags e)) (assoc :tags (:tags e))))

(defn- left [e k ^long tick]
  (when-let [t (get e k)]
    (let [dt (- (long t) tick)] (when (pos? dt) dt))))

(defn- saved-timers [m e tick]
  (let [m (reduce (fn [m k]
                    (if-let [dt (left e k tick)] (assoc m k dt) m))
                  m timers)]
    (cond-> m
      (nil? (:hurt-by-player-until m))
      (dissoc :last-hurt-by-player))))

(defn- saved-scute [m e ^long tick]
  (if-let [at (:scute-at e)]
    (assoc m :scute-time (- (long at) tick))
    m))

(defn- saved-memories [m e tick]
  (if-let [ms (brain/saved e tick)] (assoc m :memories ms) m))

(defn- saved-clocks [m e tick]
  (-> (saved-timers m e tick)
      (saved-scute e tick)
      (saved-memories e tick)))

(defn timed?
  "Returns true when what a save keeps of e depends on the tick."
  [e]
  (or (boolean (some #(get e %) (conj timers :scute-at)))
      (brain/timed? e)))

(defn saved
  "Returns entity e as the data a save keeps at game tick tick.
  The rest starts fresh when loaded. A frozen mob keeps the time its
  clocks had when they stopped."
  [e tick]
  (let [k (kind (:type e))]
    (if (= :area-effect-cloud k)
      (-> (into {} e) (dissoc :track :victims)
          (update :pos plain) (update :vel plain))
      (cond-> (into (base e)
                    (filter (comp some? val))
                    (select-keys e (kept k)))
        (= :mob k) (saved-clocks e (long (or (:frozen-at e) tick)))
        (:carrots (:hop e)) (assoc :carrots (:carrots (:hop e)))))))

(defn- still-axis ^double [^double a]
  (if (> (Math/abs a) 10.0) 0.0 a))

(defn- loaded-base [m]
  (cond-> {:pos (or (:pos m) [0.0 0.0 0.0])
           :vel (mapv still-axis (or (:vel m) [0.0 0.0 0.0]))
           :yaw (double (or (:yaw m) 0.0))
           :pitch (double (or (:pitch m) 0.0))
           :on-ground (boolean (:on-ground m))}
    (seq (:tags m)) (assoc :tags (set (:tags m)))))

(defn- loaded-timers [e m ^long tick]
  (reduce (fn [e k]
            (if-let [dt (get m k)]
              (assoc e k (+ tick (long dt)))
              (cond-> e (contains? expired k) (assoc k (expired k)))))
          e timers))

(defn- locked-baby
  "Returns mob e loaded at tick, a baby again when its age is locked,
  however long ago it was locked."
  [e ^long tick]
  (if (and (:age-locked? e) (nil? (:baby-until e)))
    (assoc e :baby-until (+ tick mobs/baby-start))
    e))

(defn- mob-extras [e m tick]
  (let [top (mobs/max-health (:type m))]
    (-> e
        (assoc :head-yaw (:yaw e) :health-sent top
               :health (or (:health m) top))
        (update (mobs/look-key (:type m)) #(or % 0))
        (cond-> (:carrots m) (assoc :hop {:carrots (:carrots m)}))
        (cond-> (:scute-time m)
          (assoc :scute-at (+ (long tick) (long (:scute-time m)))))
        (loaded-timers m (long tick))
        (locked-baby (long tick))
        (cond-> (:memories m)
          (assoc :brain
                 {:memories (brain/loaded (:memories m) tick)})))))

(defn- kind-extras [e k m tick]
  (case k
    :mob (mob-extras e m tick)
    :falling-block (update e :block #(or % (block/state :sand)))
    :hanging (assoc e :check-at (hanging/first-check-at tick))
    e))

(defn loaded
  "Returns the entity saved as m back at game tick tick, or nil when
  the entity does not survive a load."
  [m tick]
  (let [k (kind (:type m))]
    (cond
      (= :area-effect-cloud k)
      (of (merge (dissoc m :victims) (loaded-base m)))
      (and (= :item k) (not (pos? (long (:count (:stack m) 0)))))
      nil
      :else
      (of (-> (merge (defaults k) (select-keys m (kept k)))
              (merge (loaded-base m) (fresh k) {:type (:type m)})
              (kind-extras k m tick)
              (assoc :born (long tick)))))))

(defn living-attacker?
  "Returns true when damage source src has a living entity behind it."
  [src]
  (and (some? (:cause src)) (living? (:attacker src))))

(defn display-name
  "Returns the name of entity e as chat shows it."
  [e]
  (let [k (str "entity.minecraft." (data/snake (:type e)))
        nm (cond (= :player (:type e)) (:name e)
                 (:custom-name e) (:custom-name e)
                 :else {:translate k})
        hover {:action :show-entity :id (:type e) :uuid (:uuid e)
               :name nm}]
    (cond
      (nil? (:uuid e)) nm
      (= :player (:type e))
      {:text nm :insertion nm :hover hover
       :click {:action :suggest-command
               :command (str "/tell " nm " ")}}
      :else (assoc nm :hover hover :insertion (str (:uuid e))))))

(defn- yellow-line [key e]
  {:translate key :with [(display-name e)] :color "yellow"})

(defn joined-text
  "Returns the line chat shows when player e joins."
  [e]
  (yellow-line "multiplayer.player.joined" e))

(defn left-text
  "Returns the line chat shows when player e leaves."
  [e]
  (yellow-line "multiplayer.player.left" e))
