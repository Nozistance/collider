(ns collider.game.systems.consume
  "Eating and drinking, and filling a glass bottle at water."
  (:require [collider.data :as data]
            [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.blocks.reach :as reach]
            [collider.game.systems.effects :as effects]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(def ^:private ^:const effects-interval 4)

(def ^:private ^:const effects-start 0.21875)

(defn- roll ^double [world eid salt]
  (random/of-key (:tick world) eid salt))

(defn emits?
  "Returns true when the tick with that many ticks left is heard."
  [c ^long left]
  (let [total (state/consume-ticks c)
        wait (long (* total effects-start))]
    (and (> (- total left) wait)
         (zero? (rem left effects-interval)))))

(defn- use-sound [world eid e c salt]
  (let [a (roll world eid [salt 0]) b (roll world eid [salt 1])
        drink? (= :drink (:animation c))
        volume (if (or drink? (< a 0.5)) 0.5 1.0)
        pitch (if drink? (+ 0.9 (* 0.1 a)) (+ 1.0 (* 0.2 (- b a))))]
    (out/all (out/sound (:sound c) (:pos e) volume pitch :players))))

(defn- food-sounds [world eid e c]
  (let [a (roll world eid [:food 0]) b (roll world eid [:food 1])
        c' (roll world eid [:food 2])
        pos (:pos e)
        eat (+ 1.0 (* 0.4 (- a b)))
        burp (+ 0.9 (* 0.1 c'))]
    [(out/all (out/sound (:sound c) pos 1.0 eat :neutral))
     (out/all
      (out/sound :entity.player.burp pos 0.5 burp :players))]))

(defn- effect-sounds [e c]
  (for [{:keys [type sound]} (:effects c) :when (= :play-sound type)]
    (out/all (out/sound sound (:pos e) 1.0 1.0 :players))))

(defn- on-consume [acc {:keys [type]}]
  (case type
    :clear-all-effects (effects/take-all acc)
    acc))

(defn- consume-effect-deltas [eid e c]
  (let [acc (reduce on-consume (effects/account eid e) (:effects c))]
    (when (:changed? acc) (effects/deltas acc e))))

(defn- stopped [eid]
  [[:merge-entity eid {:using-item? false :using nil}]])

(defn- extra-deltas [world eid e stack]
  (let [[changes left] (items/add-stack (:inventory e) stack)]
    (concat (for [[slot s] changes] [:set-slot eid slot s])
            (when left
              [[:spawn-entity (items/dropped world eid left)]]))))

(defn- remainder-deltas [world eid e hand stack]
  (let [left (get-in (data/items) [(:item stack) :use-remainder])]
    (when (and left (not (state/infinite-materials? e)))
      (let [over (dec (long (:count stack 1)))
            slot (state/hand-slot e hand)
            made {:item (:item left) :count (long (:count left 1))}]
        (if (pos? over)
          (cons [:set-slot eid slot (assoc stack :count over)]
                (extra-deltas world eid e made))
          [[:set-slot eid slot made]])))))

(defn- finish-deltas [world eid e]
  (let [{:keys [hand item]} (:using e)
        stack (state/hand-stack e hand)
        c (state/consumable stack)]
    (concat [(use-sound world eid e c :finish)
             [:award eid (keyword "used" (name item)) 1]]
            (when (get-in (data/items) [item :food])
              (food-sounds world eid e c))
            (effect-sounds e c)
            (consume-effect-deltas eid e c)
            (remainder-deltas world eid e hand stack)
            (state/cooldown-deltas eid e item (:tick world))
            (stopped eid))))

(defn- advanced [eid e ^long left]
  [[:merge-entity eid
    {:using (assoc (:using e) :remaining (dec left))}]])

(defn- step-deltas [world [eid _]]
  (let [e (get-in world [:entities eid])
        {:keys [hand item remaining]} (:using e)
        left (long (or remaining 0))
        c (state/consumable (state/hand-stack e hand))]
    (cond
      (nil? (:using e)) nil
      (or (nil? c) (not= item (:item (state/hand-stack e hand))))
      (stopped eid)
      :else
      (concat (when (emits? c left) [(use-sound world eid e c left)])
              (if (= 1 left)
                (finish-deltas world eid e)
                (advanced eid e left))))))

(def ^:private water-bottle
  {:item       :potion :count 1
   :components {:potion-contents
                {:potion :water :custom-color nil
                 :custom-effects [] :custom-name nil}}})

(defn- same-stack? [a b]
  (and (= (:item a) (:item b)) (= (:components a) (:components b))))

(defn- filled-deltas [world eid e made]
  (if (state/infinite-materials? e)
    (when-not (some #(same-stack? made %) (vals (:inventory e)))
      (extra-deltas world eid e made))
    (extra-deltas world eid e made)))

(defn- water-at? [world pos]
  (let [st (edit/block-at world pos)]
    (or (and (block/source-state? st)
             (block/water? st))
        (= :true (:waterlogged (block/props-of st))))))

(defn- fill-sound [eid e]
  (out/except eid (out/sound :bottle/fill (:pos e) 1.0 1.0 :neutral)))

(defn bottle-deltas
  "Returns the deltas for a player who fills a glass bottle.
  The bottle fills at the water source in view."
  [world eid e]
  (when-let [{:keys [pos]} (reach/clip world e :source-only)]
    (when (water-at? world pos)
      (concat [(fill-sound eid e)
               [:award eid :used/glass-bottle 1]]
              (filled-deltas world eid e water-bottle)))))

(defn- using-entries [world]
  (filter (fn [[_ e]] (:using e)) (:entities world)))

(defn consume [world _]
  [#(state/fold-events world (using-entries world) step-deltas)])
