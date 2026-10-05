(ns collider.game.systems.hanging
  "Hanging, use, breaking and support checks of paintings and item
  frames."
  (:require [collider.game.deltas :as deltas]
            [collider.game.hanging.drops :as drops]
            [collider.game.inventory :as inventory]
            [collider.game.mode :as game-mode]
            [collider.game.hanging :as hanging]
            [collider.game.apply :as apply]
            [collider.game.areas :as areas]
            [collider.game.level :as level]
            [collider.game.player :as player]
            [collider.game.reach :as reach]
            [collider.random :as random]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(defn- hung-entries [world]
  (level/of-types world hanging/types))

(defn- checked [world t [live acc] [eid e]]
  (if (hanging/survives? (:chunks world) eid e live)
    [live (conj acc [:merge-entity eid
                     {:check-at (hanging/next-check-at t)}])]
    [(dissoc live eid)
     (-> (conj acc [:remove-entity eid])
         (into (drops/dropped world eid e nil nil)))]))

(defn- turn [world active t [live acc :as s] [eid e :as entry]]
  (if (areas/active-at? active (:pos e))
    (checked world t s entry)
    [live (conj acc [:merge-entity eid
                     {:check-at (inc (long (:check-at e)))}])]))

(defn- busy? [active t [_ e]]
  (or (<= (long (:check-at e)) (long t))
      (not (areas/active-at? active (:pos e)))))

(defn- turns [world entries]
  (let [active (areas/active-chunks world)
        t (long (:tick world))
        busy (filterv #(busy? active t %) entries)
        step (fn [s entry] (turn world active t s entry))]
    (if (empty? busy)
      []
      (peek (reduce step [(into {} entries) []] busy)))))

(defn hanging-checks
  "Returns the deltas that tick the hanging entities. Once in 101 of
  its ticks each checks that its wall still holds it, and drops when
  it does not."
  {:wake {:types hanging/types}}
  [world _d]
  (deltas/of-vec (turns world (hung-entries world))))

(defn- placeable? [world item face pos]
  (if (= :painting item)
    (not= :y (dir/axis face))
    (chunk/in-level? world (nth pos 1))))

(defn- dyed [e stack]
  (if-let [v (get-in stack [:components :painting/variant])]
    (if (keyword? v) (hanging/placed (assoc e :variant v)) e)
    e))

(defn- painted [world pos face stack]
  (let [t (:tick world)
        roll (random/of-key [t pos :art])
        hung (hung-entries world)]
    (some-> (hanging/painting (:chunks world) hung pos face t roll)
            (dyed stack))))

(defn- made [world item pos face stack]
  (if (= :painting item)
    (painted world pos face stack)
    (hanging/frame item pos face (:tick world))))

(defn- hung-deltas [eid p e hand]
  (concat [(drops/sound e "place")]
          (signal/game-event :entity-place (:pos e) eid)
          [[:spawn-entity e]]
          (inventory/consume-deltas eid p hand 1)))

(defn- holds? [world e]
  (hanging/survives? (:chunks world) nil e (hung-entries world)))

(defn place-deltas
  "Returns the deltas of player eid hanging the item it holds on the
  face of the block at pos. The player is p."
  [world eid p pos face]
  (let [hand (:use-hand p :main)
        stack (player/hand-stack p hand)
        item (:item stack)
        face (dir/from-index face)
        at (dir/toward pos face)]
    (when (placeable? world item face at)
      (when-let [e (made world item at face stack)]
        (when (holds? world e)
          (hung-deltas eid p e hand))))))

(defn- gap-sq ^double [p [x0 y0 z0 x1 y1 z1]]
  (let [[ex ey ez] (reach/eye-pos p)
        g (fn ^double [^double c ^double lo ^double hi]
            (max (- lo c) (- c hi) 0.0))
        dx (g ex x0 x1) dy (g ey y0 y1) dz (g ez z0 z1)]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(def ^:private ^:const reach-buffer 3.0)

(defn- range-of ^double [p]
  (+ (player/entity-reach p) reach-buffer))

(defn- in-use-range? [p e]
  (let [r (range-of p)]
    (< (gap-sq p (hanging/box e)) (* r r))))

(defn- in-attack-range? [p e]
  (<= (Math/sqrt (gap-sq p (hanging/box e))) (range-of p)))

(defn- put-deltas [peid p eid e hand stack]
  (let [e' (assoc e :stack (assoc stack :count 1))]
    (concat [[:merge-entity eid {:stack (:stack e')}]
             (drops/sound e' "add-item")]
            (signal/game-event :block-change (:pos e) peid)
            (inventory/consume-deltas peid p hand 1))))

(defn- turn-item-deltas [peid eid e]
  (let [r (mod (inc (long (or (:rotation e) 0))) 8)]
    (concat [(drops/sound e "rotate-item")
             [:merge-entity eid {:rotation r}]]
            (signal/game-event :block-change (:pos e) peid))))

(defn- frame-use [peid p eid e hand]
  (let [stack (player/hand-stack p hand)]
    (cond (:stack e) (turn-item-deltas peid eid e)
          stack (put-deltas peid p eid e hand stack))))

(defn- target [world ev]
  (let [p (get-in world [:entities (nth ev 1)])
        e (get-in world [:entities (nth ev 2)])]
    (when (and p e (hanging/types (:type e))
               (not (game-mode/spectator? p)))
      [p e])))

(defn- use-deltas [world [_ peid eid hand :as ev]]
  (when-let [[p e] (target world ev)]
    (when (and (hanging/frames (:type e)) (in-use-range? p e))
      (frame-use peid p eid e (if (#{:off 1} hand) :off :main)))))

(defn- emptied-deltas [world peid p eid e]
  (concat [[:merge-entity eid {:stack nil}]]
          (drops/frame-drops world eid e p false)
          (signal/game-event :block-change (:pos e) peid)
          [(drops/sound e "remove-item")]))

(defn- attack-deltas [world [_ peid eid :as ev]]
  (when-let [[p e] (target world ev)]
    (when (in-attack-range? p e)
      (if (and (:stack e) (hanging/frames (:type e)))
        (emptied-deltas world peid p eid e)
        (drops/kill-deltas world eid e p peid)))))

(defn- event-deltas [world ev]
  (case (nth ev 0)
    :interact (use-deltas world ev)
    :attack (attack-deltas world ev)
    nil))

(defn- aimed? [world ev]
  (and (#{:interact :attack} (nth ev 0))
       (hanging/types (get-in world [:entities (nth ev 2) :type]))))

(defn hanging-uses
  "Returns the deltas of the players using and hitting paintings and
  item frames this tick, one after another."
  {:wake {:events #{:interact :attack}}}
  [world d]
  (let [evs (filterv #(aimed? world %) (:input d))]
    (deltas/of-vec (when (seq evs)
                     (apply/fold-events world evs event-deltas)))))
