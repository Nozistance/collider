(ns collider.game.systems.orbs
  "Experience orb motion and merging, and players taking orbs up."
  (:require [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.entity.shove :as shove]
            [collider.game.experience :as xp]
            [collider.game.mode :as game-mode]
            [collider.game.orb :as orb]
            [collider.game.out :as out]
            [collider.game.areas :as areas]
            [collider.game.level :as level]
            [collider.game.turn.overlay :as overlay]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.motion :as motion]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- active-orbs [world]
  (into (sorted-map)
        (areas/active-of-types world [:experience-orb])))

(defn- followers [world]
  (into [] (remove #(game-mode/spectator? (val %)))
        (level/player-entries world)))

(defn- lived ^long [world e]
  (- (long (:tick world)) (long (or (:born e) 0))))

(defn- roll-of [world eid]
  (let [t (:tick world)] (fn [k] (random/of-key t eid k))))

(defn- pushed [world e]
  (let [{:keys [pos vel]} e
        cs (:chunks world)
        h (orb/height)
        push (liquid/entity-push cs pos (orb/half) h vel
                                 (:dim world))]
    (v/add vel push)))

(defn- driven [world e hit? roll]
  (let [chunks (:chunks world) pos (:pos e) vel (pushed world e)
        vel (cond (orb/eye-in-water? chunks pos) (orb/swum vel)
                  (not hit?) (orb/fallen vel)
                  :else vel)]
    (if (orb/in-lava? chunks pos) (orb/tossed roll) vel)))

(defn- scanned [world eid e orbs]
  (if (= 1 (rem (lived world e) 20))
    (orb/merged eid e (seq (dissoc orbs eid)))
    [e []]))

(defn- drawn [world e vel players hit? roll]
  (let [pid (orb/followed e players)
        chunks (:chunks world)]
    (cond
      pid [pid (orb/pulled vel (:pos e) (get (into {} players) pid))
           false]
      (and hit? (orb/colliding? chunks (:pos e) vel))
      [nil (shove/shoved chunks (:pos e) (orb/height) vel
                         (roll :shove))
       true]
      :else [nil vel false])))

(defn- travelled [world e vel pid sync?]
  (let [chunks (:chunks world)
        [p w og sup] (orb/moved chunks (:pos e) vel (:stuck e))
        w (assoc w 1 (liquid/bubble-push chunks p (double (w 1))))
        st (motion/stuck-speed chunks p (orb/half) (orb/height))]
    {:pos p :vel (orb/slowed chunks p w og sup (v/y vel))
     :on-ground og :follow pid
     :needs-sync? sync? :stuck st
     :age (inc (long (:age e)))}))

(defn- gone? [e] (>= (long (:age e)) orb/lifetime))

(defn- stepped [world players orbs eid e roll]
  (let [hit? (orb/colliding? (:chunks world) (:pos e))
        vel (driven world e hit? roll)
        [e taken] (scanned world eid e orbs)
        [pid vel sync?] (drawn world e vel players hit? roll)]
    [(when-not (< (v/y (:pos e)) (chunk/void-y world))
       (merge e (travelled world e vel pid sync?)))
     taken]))

(def ^:private stepped-keys
  [:pos :vel :on-ground :follow :needs-sync? :stuck :age :count])

(defn- orb-deltas [eid e' taken]
  (into (mapv (fn [o] [:remove-entity o]) taken)
        [(if (or (nil? e') (gone? e'))
           [:remove-entity eid]
           [:merge-entity eid (select-keys e' stepped-keys)])]))

(defn- tick-all [world players start roll-for]
  (loop [ids (keys start) orbs start out []]
    (if-let [eid (first ids)]
      (if-let [e (get orbs eid)]
        (let [roll (roll-for eid)
              w (overlay/seen world eid)
              [e' taken] (stepped w players orbs eid e roll)
              orbs (cond-> (apply dissoc orbs taken)
                     e' (assoc eid e') (nil? e') (dissoc eid))
              out (into out (orb-deltas eid e' taken))]
          (recur (next ids) orbs out))
        (recur (next ids) orbs out))
      out)))

(defn orbs
  "Returns the deltas of every orb in an active chunk after a tick.
  The orbs step one by one in id order."
  {:wake {:types #{:experience-orb}}}
  [world _d]
  (let [start (active-orbs world)]
    (deltas/of-vec
      (when (seq start)
        (tick-all world (followers world) start
                  (partial roll-of world))))))

(defn- touches? [p o]
  (let [[half h] (entity/pose-box (:pose p :standing))
        pp (:pos p) op (:pos o)
        px (v/x pp) py (v/y pp) pz (v/z pp)
        ox (v/x op) oy (v/y op) oz (v/z op)
        half (double half)]
    (and (< (- (- px half) 1.0) (+ ox (orb/half)))
         (> (+ (+ px half) 1.0) (- ox (orb/half)))
         (< (- py 0.5) (+ oy (orb/height)))
         (> (+ (+ py (double h)) 0.5) oy)
         (< (- (- pz half) 1.0) (+ oz (orb/half)))
         (> (+ (+ pz half) 1.0) (- oz (orb/half))))))

(defn- ready? [world p]
  (>= (long (:tick world)) (long (or (:xp-ready-at p) 0))))

(defn- picked [world pid touching]
  (let [n (count touching)
        r (random/of-key (:tick world) pid :orb-pick)]
    (nth touching (min (dec n) (long (* r n))))))

(defn- chime [pos vol]
  (out/all (out/sound :player/levelup pos vol 1.0)))

(defn- orb-left [oid ^long left]
  (if (pos? left)
    [:merge-entity oid {:count left}]
    [:remove-entity oid]))

(defn- taken-deltas [world pid p oid o]
  (let [acc (xp/account p (lived world p))
        acc (xp/give-points acc (long (:value o)))
        ready (+ 2 (long (:tick world)))
        marks (assoc (xp/marks acc) :xp-ready-at ready)]
    (concat [(out/all (out/collect oid pid))
             [:merge-entity pid marks]
             (orb-left oid (dec (long (:count o))))]
            (map #(chime (:pos p) %) (:chimes acc)))))

(defn player-pickup
  "Returns the deltas of player p, an entry, taking up one of the orbs
  it touches."
  [world [pid p]]
  (when (and (pos? (double (:health p 20.0)))
             (not (game-mode/spectator? p))
             (ready? world p))
    (let [near #(touches? p (val %))
          touching (filterv near (active-orbs world))]
      (when (seq touching)
        (let [[oid o] (picked world pid touching)]
          (taken-deltas world pid p oid o))))))
