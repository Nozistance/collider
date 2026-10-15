(ns collider.game.bow
  "A bow drawn and let go."
  (:require [collider.game.inventory :as inventory]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.game.turn.thrown :as thrown]
            [collider.game.using :as using]
            [collider.num :as num]
            [collider.random :as random]))

(set! *warn-on-reflection* true)

(def ^:private ^:const least-power 0.1)

(defn power
  "Returns the power of a bow drawn for ticks, full at 20."
  ^double [^long ticks]
  (let [f num/f32
        p (f (/ (f ticks) (f 20.0)))]
    (min 1.0 (f (/ (f (+ (f (* p p)) (f (* p (f 2.0))))) (f 3.0))))))

(defn arrow
  "Returns an arrow of eid that carries one of ammo, flies as flight
  and may be picked up as pickup says."
  [eid ammo pickup crit? flight]
  (merge {:type :arrow :owner eid :age 0 :left-owner? false
          :on-ground false :stack (assoc ammo :count 1) :pickup pickup
          :crit? crit? :life 0 :shake 0 :in-ground? false}
         flight))

(defn- shoot-sound [world eid e ^double pow]
  (let [f num/f32
        r (f (random/of-key (:tick world) eid :bow))
        d (f (+ (f (* r (f 0.4))) (f 1.2)))
        pitch (f (+ (f (/ (f 1.0) d)) (f (* pow (f 0.5)))))]
    (out/all (out/sound :arrow/shoot (:pos e) 1.0 pitch))))

(defn- spent [stack]
  (let [n (dec (long (:count stack 1)))]
    (when (pos? n) (assoc stack :count n))))

(defn- shot-deltas [world eid e {:keys [hand remaining]}]
  (let [slot (player/projectile-slot e :bow)
        creative? (game-mode/instabuild? e)
        pow (power (- using/bow-ticks (long remaining)))]
    (when (and (or slot creative?) (>= pow least-power))
      (let [ammo (if slot
                   (get-in e [:inventory slot])
                   {:item :arrow})]
        (concat
          [[:spawn-entity
            (arrow eid ammo (if creative? :creative-only :allowed)
                   (== 1.0 pow)
                   (thrown/launched world eid e (* 3.0 pow) 0.0
                                    thrown/eye-drop))]]
          (when-not creative? [[:set-slot eid slot (spent ammo)]])
          (inventory/hurt-item-deltas (:tick world) eid e hand 1)
          [(shoot-sound world eid e pow)
           [:award eid :used/bow 1]])))))

(defn release-deltas
  "Returns the deltas of the event [:release-use eid] when its player
  lets go of the bow it draws. The bow shoots past a tenth of its
  power, an arrow found or, with infinite materials, a new one, and
  wears by one."
  [world [_ eid]]
  (let [e (get-in world [:entities eid])
        {:keys [hand item] :as u} (:using e)]
    (when (and (= :bow item)
               (= :bow (:item (player/hand-stack e hand))))
      (shot-deltas world eid e u))))
