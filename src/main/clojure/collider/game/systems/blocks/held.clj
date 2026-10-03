(ns collider.game.systems.blocks.held
  "Items used in the air, such as armor, the spyglass and the goat
  horn."
  (:require [collider.data :as data]
            [collider.data.pack :refer [kw]]
            [collider.game.inventory :as inventory]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.stack :as stack]
            [collider.game.player :as player]
            [collider.game.slots :as slots]
            [collider.game.using :as using]))

(set! *warn-on-reflection* true)

(defn swap-slot
  "Returns the slot a player puts item on by using it, or nil."
  [item]
  (slots/armor (data/swap-slot item)))

(defn- binds? [v]
  (contains? (get v "effects") "minecraft:prevent_armor_change"))

(def ^:private ^:table binding-enchantments
  (delay (into #{}
               (keep (fn [[id v]] (when (binds? v) (kw id))))
               (data/pack "enchantment"))))

(defn- binding? [worn]
  (some @binding-enchantments (keys (stack/enchantments worn))))

(defn- same-stack? [a b]
  (and (= (:item a) (:item b))
       (= (:components a) (:components b))
       (= (:removed a) (:removed b))))

(defn- swappable?
  "Returns true when player e swaps held for worn.
  A player in creative also takes off a worn item that binds."
  [e held worn]
  (and (or (game-mode/creative? e) (not (binding? worn)))
       (not (same-stack? held worn))))

(defn- equip-sound [e item]
  (out/all (out/sound (data/equip-sound item) (:pos e) 1.0 1.0
                      :players)))

(defn- hand-after [world eid e hand held worn]
  (let [slot (player/hand-slot e hand)]
    (if (<= (stack/size held) 1)
      [[:set-slot eid slot
        (cond worn worn (game-mode/creative? e) held)]]
      (concat (when worn (inventory/kept world eid e worn))
              (when-not (player/infinite-materials? e)
                [[:set-slot eid slot (update held :count dec)]])))))

(defn equip-deltas
  "Returns the deltas of a player who puts on the held item. The worn
  item goes to the hand."
  [world eid e hand held]
  (let [item (:item held)
        slot (swap-slot item)
        worn (get-in e [:inventory slot])]
    (when (swappable? e held worn)
      (concat
        [[:award eid (keyword "used" (name item)) 1]
         [:set-slot eid slot (assoc held :count 1)]
         (equip-sound e item)]
        (hand-after world eid e hand held worn)))))

(defn spyglass-deltas
  "Returns the deltas of a player raising a spyglass."
  [eid e]
  (let [snd (out/sound :item.spyglass.use (:pos e) 1.0 1.0 :players)]
    [(out/except eid snd)
     [:award eid :used/spyglass 1]]))

(defn- horn-cooldown [eid e group ticks t]
  [[:merge-entity eid
    {:cooldowns (assoc (:cooldowns e) group
                        (+ (long t) (long ticks)))}]
   (out/to eid (out/cooldown group ticks))])

(defn- horn-sound [eid e ins]
  (let [volume (float (/ (double (:range ins)) 16.0))
        snd (:sound ins)
        fx (out/entity-sound snd eid (:pos e) volume 1.0 :records)]
    (out/except eid fx)))

(defn horn-deltas
  "Returns the deltas of a player blowing a goat horn.
  A horn with no instrument does nothing."
  [world eid e held]
  (when-let [ins (using/instrument-of held)]
    (let [ticks (using/instrument-ticks ins)
          group (data/cooldown-group (:item held))]
      (concat [(horn-sound eid e ins)]
              (horn-cooldown eid e group ticks (:tick world))
              [[:award eid :used/goat-horn 1]]))))
