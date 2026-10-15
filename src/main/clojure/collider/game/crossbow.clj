(ns collider.game.crossbow
  "A crossbow charged."
  (:require [collider.game.bow :as bow]
            [collider.game.glide :as glide]
            [collider.game.inventory :as inventory]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.game.stack :as stack]
            [collider.game.turn.rocket :as rocket]
            [collider.game.turn.thrown :as thrown]
            [collider.game.using :as using]
            [collider.num :as num]
            [collider.random :as random]))

(set! *warn-on-reflection* true)

(def ^:private ^:const charge-ticks 25)

(def ^:private ^:const start-at (double (float 0.2)))

(def ^:private ^:const mid-at (double (float 0.5)))

(def ^:private ^:const rocket-drop (double (float 0.15)))

(defn- charged? [stack]
  (boolean (seq (stack/component stack :charged-projectiles))))

(defn- drawn [e]
  (let [slot (player/projectile-slot e :crossbow)
        found (if slot (get-in e [:inventory slot]) {:item :arrow})
        one (assoc found :count 1)]
    (cond
      (game-mode/instabuild? e)
      {:stack (stack/put one :intangible-projectile {})}
      slot {:stack one :slot slot :rest (stack/shrunk found 1)})))

(defn- end-sound [world eid e]
  (let [f num/f32
        r (f (random/of-key (:tick world) eid :crossbow))
        pitch (f (+ (f (/ (f 1.0) (f (+ (f (* r (f 0.5))) (f 1.0)))))
                    (f 0.2)))]
    (out/all (out/sound :crossbow/loading-end (:pos e) 1.0 pitch))))

(defn- load-deltas [world eid e hand]
  (when-let [{:keys [slot rest] :as d} (drawn e)]
    (concat
      (when slot [[:set-slot eid slot rest]])
      [[:set-slot eid (player/hand-slot e hand)
        (stack/put (player/hand-stack e hand) :charged-projectiles
                   [(stack/template (:stack d))])]
       (end-sound world eid e)])))

(defn- flag-sound [kind e]
  (out/all (out/sound kind (:pos e) 0.5 1.0)))

(defn- sound-deltas [world e p]
  (let [was (:crossbow-sounds world #{})
        flags (if (< p start-at) #{} was)
        start? (and (>= p start-at) (not (:start flags)))
        mid? (and (>= p mid-at) (not (:mid flags)))
        now (cond-> flags start? (conj :start) mid? (conj :mid))]
    (concat (when (not= now was) [[:crossbow-sounds now]])
            (when start? [(flag-sound :crossbow/loading-start e)])
            (when mid? [(flag-sound :crossbow/loading-middle e)]))))

(defn tick-deltas
  "Returns the deltas of a tick of the charge of player e of eid with
  remaining ticks of use left. The sound flags are one pair for every
  player, as vanilla keeps them on the item."
  [world eid e ^long remaining]
  (let [f num/f32
        p (f (/ (f (- using/bow-ticks remaining)) (f charge-ticks)))
        hand (get-in e [:using :hand])
        loads? (and (>= p 1.0)
                    (not (charged? (player/hand-stack e hand))))]
    (concat (sound-deltas world e p)
            (when loads? (load-deltas world eid e hand)))))

(defn started-deltas
  "Returns the deltas of the event [:use-item eid ...] or [:place eid
  ...] once it started the charge of a crossbow. The sound flags of
  every player drop."
  [world [_ eid]]
  (let [{:keys [item started remaining]}
        (get-in world [:entities eid :using])]
    (when (and (= :crossbow item) (= (:tick world) started)
               (= using/bow-ticks remaining)
               (seq (:crossbow-sounds world)))
      [[:crossbow-sounds #{}]])))

(defn release-deltas
  "Returns the deltas of the event [:release-use eid] when its player
  lets go of the crossbow it charges, one more tick of the charge."
  [world [_ eid]]
  (let [e (get-in world [:entities eid])
        {:keys [hand item remaining]} (:using e)]
    (when (and (= :crossbow item)
               (= :crossbow (:item (player/hand-stack e hand))))
      (tick-deltas world eid e (long remaining)))))

(defn- rocket? [stack] (= :firework-rocket (:item stack)))

(defn- pickup-of [ammo]
  (if (stack/component ammo :intangible-projectile)
    :creative-only
    :allowed))

(defn- shot [world eid e ammo power]
  (let [dir (glide/look (double (:pitch e 0.0)) (double (:yaw e 0.0)))
        drop (if (rocket? ammo) rocket-drop thrown/eye-drop)
        flight (thrown/shot world eid e dir power drop)]
    (if (rocket? ammo)
      (merge (rocket/rocket world eid ammo (:pos flight)) flight
             {:shot-at-angle? true})
      (assoc (bow/arrow eid ammo (pickup-of ammo) true flight)
             :hit-sound :crossbow/hit))))

(defn- fired-deltas [world eid e hand slot held charge]
  (let [ammo (first charge)
        power (if (some rocket? charge) 1.6 3.15)
        emptied (stack/put held :charged-projectiles [])
        e' (assoc-in e [:inventory slot] emptied)]
    (concat
      [(out/all (out/sound :crossbow/shoot (:pos e) 1.0 1.0))
       [:spawn-entity (shot world eid e ammo power)]
       [:set-slot eid slot emptied]]
      (inventory/hurt-item-deltas (:tick world) eid e' hand
                                  (if (rocket? ammo) 3 1))
      [[:award eid :used/crossbow 1]])))

(defn shoot-deltas
  "Returns the deltas of player eid, e, shooting the charged crossbow
  in hand, or nil when it holds no charge. The charge leaves along the
  look, an arrow wears the crossbow by one, a rocket by three."
  [world eid e]
  (let [hand (:use-hand e)
        slot (player/hand-slot e hand)
        held (player/hand-stack e hand)
        charge (map stack/of-template
                    (stack/component held :charged-projectiles))]
    (when (and (= :crossbow (:item held)) (seq charge))
      (fired-deltas world eid e hand slot held charge))))
