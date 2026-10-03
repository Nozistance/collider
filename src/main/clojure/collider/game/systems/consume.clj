(ns collider.game.systems.consume
  "Eating, drinking and other items held in use, and filling a
  glass bottle at water."
  (:require [collider.data :as data]
            [collider.game.bundle :as bundle]
            [collider.game.changes :as changes]
            [collider.game.effect.account :as account]
            [collider.game.inventory :as inventory]
            [collider.game.item :as item]
            [collider.world.env.dimension :as dimension]
            [collider.game.effect :as effect]
            [collider.game.entity :as entity]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.game.reach :as reach]
            [collider.game.stack :as stack]
            [collider.game.using :as using]
            [collider.num :as num]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ^:private ^:const effects-interval 4)

(def ^:private ^:const effects-start 0.21875)

(defn- roll ^double [world eid salt]
  (random/of-key (:tick world) eid salt))

(defn emits?
  "Returns true when the tick with that many ticks left is heard."
  [c ^long left]
  (let [total (using/consume-ticks c)
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
        r (roll world eid [:food 2])
        pos (:pos e)
        eat (+ 1.0 (* 0.4 (- a b)))
        burp (+ 0.9 (* 0.1 r))]
    [(out/all (out/sound (:sound c) pos 1.0 eat :neutral))
     (out/all
      (out/sound :entity.player.burp pos 0.5 burp :players))]))

(def ^:private ^:const teleport-tries 16)

(def ^:private ^:const kept-motion 504)

(defn- sounded [acc kind]
  (let [at (:pos (:e acc))]
    (update acc :ds conj
            (out/all (out/sound kind at 1.0 1.0 :players)))))

(defn- instance-of [{:keys [duration amplifier] :as t}]
  (effect/instance duration amplifier
                   (:ambient? t) (:visible? t) (:icon? t)))

(defn- landed-effects [acc {:keys [effects probability]} draw]
  (if (>= (float (draw)) (float probability))
    acc
    (reduce #(account/land %1 (:id %2) (instance-of %2))
            acc effects)))

(defn- top-y ^long [world]
  (let [t (dimension/type-of (:dim world :overworld))]
    (+ (chunk/level-min-y world) (long (:logical-height t 384)) -1)))

(defn- target [world e ^double d draw]
  (let [p (:pos e)
        at #(+ (double %) (* (- (double (draw)) 0.5) d))
        x (at (v/x p))
        y (at (v/y p))
        z (at (v/z p))
        lo (double (chunk/level-min-y world))]
    [x (if (< y lo) lo (min y (double (top-y world)))) z]))

(defn- landing [world [x y z]]
  (let [chunks (:chunks world) lo (chunk/level-min-y world)
        bx (num/floor x) bz (num/floor z)
        solid? #(block/blocks-motion?
                  (long (chunk/block-state chunks bx % bz)))]
    (loop [by (num/floor y) y (double y)]
      (cond
        (<= by lo) nil
        (solid? (dec by)) [x y z]
        :else (recur (dec by) (dec y))))))

(defn- wet? [chunks [x0 y0 z0 x1 y1 z1]]
  (let [span #(range (long (Math/floor %1)) (long (Math/ceil %2)))
        fluid? (fn [[x y z]]
                 (block/liquid-class
                   (chunk/block-state chunks x y z)))]
    (some fluid? (for [x (span x0 x1) y (span y0 y1) z (span z0 z1)]
                   [x y z]))))

(defn- fits? [world e [x y z :as p]]
  (let [[half h] (entity/pose-box (:pose e :standing))
        w (double (float half)) h (double (float h))
        chunks (:chunks world)
        box [(- x w) y (- z w) (+ x w) (+ y h) (+ z w)]]
    (and (phys/free? chunks (v/v3 p) w h 0.0 0.0 0.0)
         (not (wet? chunks box)))))

(defn- placed [acc p]
  (let [eid (:eid acc)]
    (-> (assoc-in acc [:e :pos] (v/v3 p))
        (update :ds conj [:teleport eid p]
                (out/to eid (out/teleport p 0.0 0.0 kept-motion))))))

(defn- arrived [acc]
  (let [eid (:eid acc) fx (out/status eid :teleport)]
    (-> acc
        (update :ds conj (out/all fx) (out/to eid fx))
        (sounded :item.chorus-fruit.teleport)
        (update :ds conj [:merge-entity eid {:fall 0.0}]))))

(defn- teleported
  "Returns acc after up to 16 tries to teleport the eater. A try that
  fails puts it back home."
  [world acc {:keys [diameter]} draw]
  (let [o (:pos (:e acc)) home [(v/x o) (v/y o) (v/z o)]
        loaded? #(contains? (:chunks world) (chunk/block-chunk %))]
    (loop [n 0 acc acc]
      (if (= n teleport-tries)
        acc
        (let [t (target world (:e acc) (double diameter) draw)
              p (when (loaded? t) (landing world t))]
          (cond
            (nil? p) (recur (inc n) (placed acc home))
            (fits? world (:e acc) p) (arrived (placed acc p))
            :else (recur (inc n) (placed (placed acc p) home))))))))

(defn- consumed [world draw acc {:keys [type] :as ce}]
  (case type
    :apply-effects (landed-effects acc ce draw)
    :remove-effects (reduce account/take-off acc (:effects ce))
    :clear-all-effects (account/take-all acc)
    :teleport-randomly (teleported world acc ce draw)
    :play-sound (sounded acc (:sound ce))
    acc))

(defn effect-deltas
  "Returns the deltas of the consume effects of c on player eid.
  They come in their order. Draw returns the next number from 0 to 1
  each call."
  [world eid e c draw]
  (let [acc (reduce #(consumed world draw %1 %2)
                    (account/account eid e) (:effects c))]
    (when (or (:changed? acc) (seq (:ds acc)))
      (account/deltas acc e))))

(defn- draws [world eid]
  (let [n (volatile! -1)]
    #(roll world eid [:consume-effect (vswap! n inc)])))

(defn- stopped [eid]
  [[:merge-entity eid {:using-item? false :using nil}]])

(defn- remainder-deltas [world eid e hand stack]
  (let [left (get-in (data/items) [(:item stack) :use-remainder])]
    (when (and left (not (player/infinite-materials? e)))
      (let [over (dec (long (:count stack 1)))
            slot (player/hand-slot e hand)
            made {:item (:item left) :count (long (:count left 1))}]
        (if (pos? over)
          (cons [:set-slot eid slot (assoc stack :count over)]
                (inventory/kept world eid e made))
          [[:set-slot eid slot made]])))))

(defn- finish-deltas [world eid e]
  (let [{:keys [hand item]} (:using e)
        stack (player/hand-stack e hand)
        c (player/consumable stack)]
    (concat [(use-sound world eid e c :finish)
             [:award eid (keyword "used" (name item)) 1]]
            (when (get-in (data/items) [item :food])
              (food-sounds world eid e c))
            (effect-deltas world eid e c (draws world eid))
            (remainder-deltas world eid e hand stack)
            (player/cooldown-deltas eid e item (:tick world))
            (stopped eid))))

(defn- advanced [eid e ^long left]
  [[:merge-entity eid
    {:using (assoc (:using e) :remaining (dec left))}]])

(defn- spyglass-stop [eid e]
  (let [snd (out/sound :item.spyglass.stop-using (:pos e) 1.0 1.0
                       :players)]
    [(out/except eid snd)]))

(defn- release-deltas [{:keys [eid item] :as u}]
  (when (= :spyglass item) (spyglass-stop eid u)))

(defn- held-deltas [eid e item ^long left]
  (if (= 1 left)
    (concat (when (= :spyglass item) (spyglass-stop eid e))
            (stopped eid))
    (advanced eid e left)))

(def ^:private ^:const first-throw-wait 10)

(defn- drops? [stack ^long left]
  (let [n (long (using/ticks stack))]
    (or (= left n)
        (and (< left (- n first-throw-wait)) (even? left)))))

(defn- bundle-sound [world eid kind pos salt]
  (let [p (+ 0.8 (* 0.4 (roll world eid [:bundle salt])))]
    (out/sound kind pos 0.8 p :players)))

(defn- dropped-deltas [world eid e hand b s]
  (let [pos (:pos e) centre (v/centre (v/cell pos))
        one :item.bundle.remove-one
        all :item.bundle.drop-contents]
    (concat
      [(out/except eid (bundle-sound world eid one pos 0))
       [:set-slot eid (player/hand-slot e hand) b]]
      (item/thrown-deltas world eid [s])
      [(out/all (bundle-sound world eid all centre 1))
       [:award eid (keyword "used" (name (:item b))) 1]])))

(defn- bundle-drop-deltas
  [world eid e hand stack left]
  (when (and (bundle/bundle? stack) (drops? stack left))
    (let [[b s] (bundle/remove-one stack)]
      (when s (dropped-deltas world eid e hand b s)))))

(defn- eaten-deltas [world eid e c left]
  (concat (when (emits? c left) [(use-sound world eid e c left)])
          (if (= 1 (long left))
            (finish-deltas world eid e)
            (advanced eid e left))))

(defn- step-deltas [world [eid _]]
  (let [e (get-in world [:entities eid])
        {:keys [hand item remaining]} (:using e)
        left (long (or remaining 0))
        stack (player/hand-stack e hand)
        c (player/consumable stack)]
    (cond
      (nil? (:using e)) nil
      (not= item (:item stack)) (stopped eid)
      c (eaten-deltas world eid e c left)
      :else (concat (bundle-drop-deltas world eid e hand stack left)
                    (held-deltas eid e item left)))))

(defn- held? [e made]
  (some #(inventory/same-stack? made %) (vals (:inventory e))))

(defn- filled-deltas [world eid e made]
  (when-not (and (player/infinite-materials? e) (held? e made))
    (inventory/kept world eid e made)))

(defn- water-source? [world pos]
  (let [st (changes/block-at world pos)]
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
    (when (water-source? world pos)
      (concat [(fill-sound eid e)
               [:award eid :used/glass-bottle 1]]
              (filled-deltas world eid e stack/water-bottle)))))

(defn player-deltas
  "Returns the deltas of the item the player entry p holds in use
  this tick, and of the uses it let go."
  [world p]
  (let [eid (key p)
        mine #(when (= eid (:eid %)) (release-deltas %))]
    (concat (mapcat mine (get-in world [:input :releases]))
            (when (:using (val p)) (step-deltas world p)))))
