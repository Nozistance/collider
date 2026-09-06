(ns collider.game.systems.blocks
  (:require [clojure.string :as str]
            [collider.rnd :as rnd]
            [collider.game.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.sense :as sense]
            [collider.game.tnt :as tnt]
            [collider.game.systems.daynight :as daynight]
            [collider.game.systems.sleep :as sleep]
            [collider.vec :as v]
            [collider.world.bed :as bed]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.connect :as connect]
            [collider.world.gen :as gen]
            [collider.world.fire :as fire]
            [collider.world.liquid :as liquid]
            [collider.world.support :as support]))

(set! *warn-on-reflection* true)

(defn- place-sound [item]
  (let [n (name item)]
    (cond
      (some #(str/ends-with? n %) ["-planks" "-log" "-wood" "-fence" "-fence-gate" "-door" "-trapdoor" "-sign" "bookshelf" "crafting-table" "chest" "ladder"]) :place/wood
      (#{"grass-block" "short-grass" "tnt" "sponge" "vine" "moss-block"} n) :place/grass
      (some #(str/ends-with? n %) ["-leaves"]) :place/grass
      (#{"dirt" "gravel" "farmland" "clay" "coarse-dirt" "rooted-dirt" "mud"} n) :place/gravel
      (some #(str/ends-with? n %) ["sand" "soul-sand"]) :place/sand
      (some #(str/ends-with? n %) ["-wool" "-carpet" "-bed"]) :place/cloth
      (some #(str/includes? n %) ["glass" "ice" "glowstone" "sea-lantern"]) :place/glass
      (#{"snow" "snow-block" "powder-snow"} n) :place/snow
      :else :place/stone)))

(defn- block-at ^long [world pos]
  (chunk/chunks-get-block (:chunks world) gen/flat-chunk pos))

(defn- box-hits-player? [[x1 y1 z1 x2 y2 z2] [px py pz]]
  (let [px (double px) py (double py) pz (double pz)]
    (and (> (+ px 0.3) (double x1)) (< (- px 0.3) (double x2))
         (> (+ py 1.8) (double y1)) (< py (double y2))
         (> (+ pz 0.3) (double z1)) (< (- pz 0.3) (double z2)))))

(defn- intersects-player? [world [x y z] state]
  (let [boxes (map (fn [[a b c d e f]]
                     [(+ (long x) (/ (double a) 16.0)) (+ (long y) (/ (double b) 16.0)) (+ (long z) (/ (double c) 16.0))
                      (+ (long x) (/ (double d) 16.0)) (+ (long y) (/ (double e) 16.0)) (+ (long z) (/ (double f) 16.0))])
                   (block/collision-boxes state))]
    (some (fn [[_ e]]
            (when (= :player (:type e))
              (some #(box-hits-player? % (:pos e)) boxes)))
          (:entities world))))

(defn- own-change
  "The true block for one player, as the block update vanilla sends when a
   placement or a dig is refused."
  [world eid pos]
  (out/to eid (out/blocks-changed (chunk/block-chunk pos) [[pos (block-at world pos)]])))

(defn- reject-deltas [world eid pos pos']
  (cond-> [(own-change world eid pos)]
    pos' (conj (own-change world eid pos'))))

(defn- change-deltas
  "One edit of the player: the changes and what they pull along (fence
   connections, door halves), applied as one group so no half-state is seen."
  [world changes]
  (let [chunks' (chunk/chunks-set-blocks (:chunks world) gen/flat-chunk changes)
        all     (into (vec changes) (connect/derived-changes chunks' (map first changes)))
        chunks'' (chunk/chunks-set-blocks chunks' gen/flat-chunk all)
        mixed   (liquid/mix-changes chunks'' gen/flat-chunk (map first all))]
    (into [[:set-blocks (into all mixed)]]
          (map (fn [[p _]] (out/all (out/fizz p))))
          mixed)))

(defn- placed-deltas
  ([world eid pos state item] (placed-deltas world eid [[pos state]] item))
  ([world eid changes item]
   (conj (change-deltas world changes)
         (out/except eid (out/sound (place-sound item) (ffirst changes) 1.0 0.8)))))

(def ^:private openable-types #{:door :trapdoor :fence-gate})

(defn- by-hand? [state]
  (not (str/starts-with? (name (block/block-of state)) "iron-")))

(defn- open-sound
  "Vanilla sound event of a door, trapdoor or gate opening or closing."
  [state open?]
  (let [n (name (block/block-of state))
        wood (cond
               (str/starts-with? n "copper") "copper"
               (or (str/starts-with? n "crimson") (str/starts-with? n "warped")) "nether-wood"
               (str/starts-with? n "bamboo") "bamboo"
               (str/starts-with? n "cherry") "cherry"
               :else "wooden")
        kind (case (block/type-of state)
               :door (if (#{"bamboo" "cherry"} wood) "wooden-door" (str wood "-door"))
               :trapdoor (if (#{"bamboo" "cherry"} wood) "wooden-trapdoor" (str wood "-trapdoor"))
               :fence-gate (if (#{"wooden" "copper"} wood) "fence-gate" (str wood "-wood-fence-gate")))]
    (keyword (str "block." kind "." (if open? "open" "close")))))

(defn- toggled [world eid pos state]
  (let [props (block/props-of state)
        self (block/block-of state)
        open? (= :true (:open props))]
    (case (block/type-of state)
      :door (let [st' (block/state self (assoc props :open (if open? :false :true)))
                  other (mapv + pos (if (= :lower (:half props)) [0 1 0] [0 -1 0]))
                  ost (block-at world other)]
              (cond-> [[pos st']]
                (= self (block/block-of ost))
                (conj [other (block/state self (assoc (block/props-of ost) :open (if open? :false :true)))])))
      :trapdoor [[pos (block/state self (assoc props :open (if open? :false :true)))]]
      :fence-gate (let [dir (block/player-direction (get-in world [:entities eid :yaw] 0.0))
                        facing (if (and (not open?) (= (:facing props) (block/opposite-facing dir))) dir (:facing props))]
                    [[pos (block/state self (assoc props :open (if open? :false :true) :facing facing))]]))))

(defn- toggle-deltas [world eid pos state]
  (let [changes (toggled world eid pos state)
        open? (= :true (:open (block/props-of (second (first changes)))))
        t (:tick world)
        pitch (+ 0.9 (* 0.1 (rnd/rnd [t pos :door])))]
    (conj (change-deltas world changes)
          (out/except eid (out/sound (open-sound state open?) pos 1.0 pitch)))))

(defn- bed-head-effect
  "Breaking the foot of a bed in creative shows the head breaking too
   (vanilla BedBlock.playerWillDestroy)."
  [world eid pos old]
  (when (and (= :bed (block/type-of old)) (= :foot (:part (block/props-of old))))
    (let [head-pos (mapv + pos (connect/bed-partner-offset old))
          head     (block-at world head-pos)]
      (when (and (= (block/block-of old) (block/block-of head))
                 (= :head (:part (block/props-of head))))
        (out/except eid (out/break-effect head-pos head))))))

(defn- dig-deltas
  "Breaking a block in creative: the cell is emptied and everyone else sees
   it break; fire goes out with its sound for all (vanilla
   BaseFireBlock.playerWillDestroy)."
  [world [eid status pos _face]]
  (let [old (block-at world pos)]
    (when (or (= 0 status) (= 2 status))
      (if (pos? old)
        (cond-> (conj (change-deltas world [[pos (block/emptied old)]])
                      (out/except eid (out/break-effect pos old)))
          (fire/fire-state? old) (conj (out/all (out/extinguish pos)))
          (bed-head-effect world eid pos old) (conj (bed-head-effect world eid pos old)))
        [(own-change world eid pos)]))))

(defn- snow-layers ^long [st]
  (Long/parseLong (name (:layers (block/props-of st)))))

(defn- replaceable?
  "Whether placing item may take the cell: air, liquids, fire, plants, snow
   (one layer for any block, up to seven for more snow, as SnowLayerBlock)."
  ([world pos] (replaceable? world pos nil))
  ([world pos item]
   (let [cur (block-at world pos)]
     (cond
       (zero? cur) true
       (liquid/liquid-state? cur) true
       (fire/fire-state? cur) true
       (= :snow-layer (block/type-of cur)) (let [n (snow-layers cur)]
                                            (if (= item :snow) (< n 8) (= n 1)))
       :else (block/replaceable? cur)))))


(defn- stacked-snow [world pos' state item]
  (let [cur (block-at world pos')]
    (if (and (= item :snow) (= :snow-layer (block/type-of cur)))
      (block/state :snow {:layers (keyword (str (min 8 (inc (snow-layers cur)))))})
      state)))

(defn- flint-deltas
  "Fire in the air cell past the clicked face when fire can stand there
   (vanilla BaseFireBlock.canBePlacedAt); the click sound is the client's own."
  [world [eid pos face]]
  (when-let [off (block/face-offsets face)]
    (if (and (tnt/tnt-state? (block-at world pos))
             (get-in world [:rules :tnt-explodes] true)
             (not (get-in world [:entities eid :sneaking?]))
             (not ((tnt/primed-origins world) pos)))
      [[:spawn-entity (tnt/primed pos [(:tick world) pos])]
       (out/all (out/sound :tnt/primed pos 1.0 1.0))]
      (let [[_ y' _ :as pos'] (mapv + pos off)
            st (fire/fire-state 0)]
        (when (and (chunk/in-range? y')
                   (zero? (block-at world pos'))
                   (support/supported? (:chunks world) gen/flat-chunk pos' st))
          [[:set-blocks [[pos' st]]]
           (out/except eid (out/sound :fire/ignite pos' 1.0 (+ 0.8 (* 0.4 (rnd/rnd [(:tick world) pos' :flint])))))])))))

(defn- slab-merge [world pos pos' face item]
  (let [clicked (block-at world pos)
        part    (block/slab-part clicked)
        lower?  (and (= 1 (long face)) (= :bottom part))
        upper?  (and (= 0 (long face)) (= :top part))]
    (cond
      (and (block/same-slab? clicked item) (or lower? upper?))
      [pos (block/double-slab item)]
      (and pos' (block/same-slab? (block-at world pos') item)
           (not= :double (block/slab-part (block-at world pos'))))
      [pos' (block/double-slab item)])))

(defn- waterlogged [world pos' state]
  (if (and (= :water (liquid/liquid-class (block-at world pos')))
           (contains? (block/props-of state) :waterlogged))
    (block/state (block/block-of state) (assoc (block/props-of state) :waterlogged :true))
    state))

(defn- kelp-place-ok? [world [x y z :as pos']]
  (let [cur (block-at world pos')]
    (and (= :water (liquid/liquid-class cur))
         (contains? #{0 8} (liquid/level cur))
         (pos? (long y))
         (support/supported? (:chunks world) gen/flat-chunk pos' (block/state :kelp)))))

(declare door-place-deltas bed-place-deltas)

(defn- solid-place-deltas [world [eid pos face item cursor]]
  (when-let [off (block/face-offsets face)]
    (when-let [state (block/placement item face (get-in world [:entities eid :yaw] 0.0) (nth cursor 1)
                                      (replaceable? world pos))]
      (let [[_ y' _ :as target] (if (replaceable? world pos item) pos (mapv + pos off))
            pos'   (when (chunk/in-range? y') target)
            state  (or (when (and pos' (not (#{:door :bed} (block/type-of state)))) (connect/reshape (:chunks world) pos' state)) state)
            state  (if pos' (waterlogged world pos' state) state)
            state  (if pos' (stacked-snow world pos' state item) state)
            merged (slab-merge world pos pos' face item)]
        (cond
          merged
          (let [[mp ms] merged]
            (if (intersects-player? world mp ms)
              (reject-deltas world eid pos pos')
              (placed-deltas world eid mp ms item)))
          (nil? pos') nil
          (not (replaceable? world pos' item))
          (reject-deltas world eid pos pos')
          (and (= :kelp (block/type-of state)) (not (kelp-place-ok? world pos')))
          (reject-deltas world eid pos pos')
          (intersects-player? world pos' state)
          (reject-deltas world eid pos pos')
          (= :door (block/type-of state))
          (door-place-deltas world eid pos pos' state item cursor)
          (= :bed (block/type-of state))
          (bed-place-deltas world eid pos pos' state item)
          :else
          (placed-deltas world eid pos' state item))))))

(defn- door-place-deltas [world eid pos pos' state item [cx _ cz]]
  (let [above (mapv + pos' [0 1 0])
        hinge (connect/door-hinge (:chunks world) pos' (block/facing-of state) cx cz)
        lower (block/state (block/block-of state) (assoc (block/props-of state) :hinge hinge))
        upper (block/state (block/block-of state) (assoc (block/props-of lower) :half :upper))]
    (if (and (chunk/in-range? (above 1))
             (replaceable? world above)
             (not (intersects-player? world above upper))
             (block/face-sturdy? (block-at world (mapv + pos' [0 -1 0])) :up))
      (placed-deltas world eid [[pos' lower] [above upper]] item)
      (reject-deltas world eid pos pos'))))

(defn- look-dir [e]
  (let [yaw   (Math/toRadians (double (:yaw e)))
        pitch (Math/toRadians (double (:pitch e)))]
    [(- (* (Math/sin yaw) (Math/cos pitch)))
     (- (Math/sin pitch))
     (* (Math/cos yaw) (Math/cos pitch))]))

(defn- ray-cell [[ex ey ez] [dx dy dz] t]
  [(long (Math/floor (+ (double ex) (* (double dx) (double t)))))
   (long (Math/floor (+ (double ey) (* (double dy) (double t)))))
   (long (Math/floor (+ (double ez) (* (double dz) (double t)))))])

(defn- pour-target [world eid]
  (when-let [e (get-in world [:entities eid])]
    (let [[px py pz] (:pos e)
          eye [(double px) (+ (double py) 1.62) (double pz)]
          dir (look-dir e)]
      (loop [t 0.0 prev nil]
        (when (<= t 5.0)
          (let [[_ by _ :as pos] (ray-cell eye dir t)
                st (if (chunk/in-range? by) (block-at world pos) 0)]
            (if (and (pos? st) (not (liquid/liquid-state? st)))
              [pos prev]
              (recur (+ t 0.1) pos))))))))

(defn- waterloggable? [st]
  (= :false (:waterlogged (block/props-of st))))

(defn- with-water [st logged?]
  (block/state (block/block-of st) (assoc (block/props-of st) :waterlogged (if logged? :true :false))))

(defn- add
  "Empties a bucket: water into a block that can hold it (a fence, a slab),
   otherwise the liquid into the cell in front of the hit block, as BucketItem."
  [world [eid _ _] state]
  (when-let [[hit [_ y' _ :as pos']] (pour-target world eid)]
    (let [hit-st (block-at world hit)]
      (cond
        (and (= :water (liquid/liquid-class state)) (waterloggable? hit-st))
        (change-deltas world [[hit (with-water hit-st true)]])
        (and pos' (chunk/in-range? y'))
        (let [cur (block-at world pos')]
          (when (or (zero? cur) (liquid/liquid-state? cur))
            (change-deltas world [[pos' state]])))))))

(defn- scoop-target [world eid]
  (when-let [e (get-in world [:entities eid])]
    (let [[px py pz] (:pos e)
          eye [(double px) (+ (double py) 1.62) (double pz)]
          dir (look-dir e)]
      (loop [t 0.0]
        (when (<= t 5.0)
          (let [[_ by _ :as pos] (ray-cell eye dir t)
                st (if (chunk/in-range? by) (block-at world pos) 0)]
            (cond
              (liquid/source-state? st) [:source pos]
              (liquid/liquid-state? st) (recur (+ t 0.1))
              (= :true (:waterlogged (block/props-of st))) [:waterlogged pos]
              (pos? st) nil
              :else (recur (+ t 0.1)))))))))

(defn- scoop-deltas
  "Fills a bucket from a source, or takes the water out of a waterlogged block."
  [world eid]
  (when-let [[kind pos] (scoop-target world eid)]
    (case kind
      :source [[:set-blocks [[pos 0]]]]
      :waterlogged (change-deltas world [[pos (with-water (block-at world pos) false)]]))))

(def ^:private armor-slot
  {"helmet" 5 "chestplate" 6 "leggings" 7 "boots" 8})

(defn- armor-slot-of [item]
  (some (fn [[suffix slot]] (when (str/ends-with? (name item) suffix) slot)) armor-slot))

(defn- equip-armor-deltas [world eid item slot]
  (let [e (get-in world [:entities eid])]
    (when (and e (nil? (get-in e [:inventory slot])))
      (let [held  (+ 36 (long (or (:held-slot e) 0)))
            stack (or (get-in e [:inventory held])
                      {:item item :count 1})]
        [[:set-slot eid slot stack]
         [:set-slot eid held nil]]))))

(defn- spawn-egg-deltas [world [_ pos face item]]
  (when-let [off (block/face-offsets face)]
    (when-let [mob (mobs/egg-type item)]
      (let [[x y z] (mapv + pos off)
            t (:tick world)
            at [(+ (long x) 0.5) (double y) (+ (long z) 0.5)]
            pitch (+ 1.0 (* 0.2 (- (rnd/rnd [t pos :p1]) (rnd/rnd [t pos :p2]))))]
        (when (chunk/in-range? y)
          (cons [:spawn-entity (mobs/egg-mob mob at [t pos] t)]
                (when-let [say (mobs/say-sound mob)]
                  [(out/all (out/sound say at 1.0 pitch))])))))))

(defn- opens? [world eid pos item use-item?]
  (let [cur (block-at world pos)]
    (and (not use-item?)
         (contains? openable-types (block/type-of cur))
         (by-hand? cur)
         (not (and item (get-in world [:entities eid :sneaking?]))))))

(defn- bed-place-deltas
  "The foot in the clicked cell, the head one cell along the player's look;
   nothing if the head cell is taken (vanilla BedBlock.getStateForPlacement)."
  [world eid pos pos' state item]
  (let [head-pos (mapv + pos' (connect/bed-partner-offset state))
        head     (block/state (block/block-of state) (assoc (block/props-of state) :part :head))]
    (if (and (replaceable? world head-pos item)
             (not (intersects-player? world head-pos head)))
      (placed-deltas world eid [[pos' state] [head-pos head]] item)
      (reject-deltas world eid pos pos'))))

(defn- uses-bed? [world eid pos item use-item?]
  (and (not use-item?)
       (= :bed (block/type-of (block-at world pos)))
       (not (and item (get-in world [:entities eid :sneaking?])))))

(defn- bed-in-range? [world eid head]
  (let [p  (get-in world [:entities eid :pos])
        st (block-at world head)
        foot (mapv + head (connect/bed-partner-offset st))]
    (some (fn [[x y z]]
            (and (<= (Math/abs (- (v/x p) (+ (double x) 0.5))) 3.0)
                 (<= (Math/abs (- (v/y p) (double y))) 2.0)
                 (<= (Math/abs (- (v/z p) (+ (double z) 0.5))) 3.0)))
          [head foot])))

(defn- bed-blocked? [world head]
  (let [st   (block-at world head)
        above (mapv + head [0 1 0])]
    (or (block/full-cube? (block-at world above))
        (block/full-cube? (block-at world (mapv + above (connect/bed-partner-offset st)))))))

(defn- spawn-deltas [world eid head]
  (when (not= head (get-in world [:entities eid :spawn]))
    [[:merge-entity eid {:spawn head}]
     (out/to eid (out/system-chat [{:translate "block.minecraft.set_spawn"}]))]))

(defn- sleep-deltas
  "Using a bed: sets the respawn point, then lies down in it or says why not
   (vanilla BedBlock.useWithoutItem and ServerPlayer.startSleepInBed)."
  [world eid pos]
  (let [head (bed/head-pos (:chunks world) pos)
        st   (when head (block-at world head))
        say  (fn [k] [(out/to eid (out/overlay [{:translate k}]))])]
    (cond
      (nil? head) nil
      (= :true (:occupied (block/props-of st))) (say "block.minecraft.bed.occupied")
      (get-in world [:entities eid :sleeping]) nil
      (not (bed-in-range? world eid head)) (say "block.minecraft.bed.too_far_away")
      (bed-blocked? world head) (say "block.minecraft.bed.obstructed")
      (not (daynight/dark? (:time-of-day world 0)))
      (concat (spawn-deltas world eid head) (say "block.minecraft.bed.no_sleep"))
      :else
      (let [[x y z] head
            lie [(+ (long x) 0.5) (+ (long y) 0.6875) (+ (long z) 0.5)]]
        (concat (spawn-deltas world eid head)
                (change-deltas world [[head (block/state (block/block-of st) (assoc (block/props-of st) :occupied :true))]])
                [[:merge-entity eid {:sleeping {:pos head :since (:tick world)} :pos (v/v3 lie)
                                     :vel [0.0 0.0 0.0] :client-vel [0.0 0.0 0.0] :leave-bed? nil}]
                 (sleep/announcement world (inc (count (sleep/sleepers world))))])))))

(defn- place-deltas [world [eid pos face item cursor]]
  (let [item      (or item (sense/held-of (get-in world [:entities eid])))
        args      [eid pos face item cursor]
        use-item? (= 255 (bit-and (long face) 0xFF))
        pour      (liquid/bucket->state item)]
    (cond
      (opens? world eid pos item use-item?) (toggle-deltas world eid pos (block-at world pos))
      (uses-bed? world eid pos item use-item?) (sleep-deltas world eid pos)
      (nil? item)                  nil
      pour                         (when use-item? (add world args pour))
      (= :flint-and-steel item)    (when-not use-item? (flint-deltas world args))
      (= :bucket item)             (when use-item? (scoop-deltas world eid))
      (mobs/egg-type item)         (when-not use-item? (spawn-egg-deltas world args))
      (armor-slot-of item)         (when use-item?
                                     (equip-armor-deltas world eid item (armor-slot-of item)))
      :else                        (solid-place-deltas world args))))

(defn- ack-deltas [events]
  (let [latest (reduce (fn [m [tag eid & args]]
                         (if-let [sq (case tag :dig (nth args 3 nil) :place (nth args 4 nil) nil)]
                           (update m eid (fnil max -1) (long sq))
                           m))
                       {} events)]
    (map (fn [[eid sq]] (out/to eid (out/block-ack sq))) latest)))

(defn- with-edits
  "World as the next event of the tick sees it: the block changes so far applied."
  [world deltas]
  (let [changes (into [] (mapcat (fn [[tag recs]] (when (= tag :set-blocks) recs))) deltas)]
    (if (empty? changes)
      world
      (update world :chunks chunk/chunks-set-blocks gen/flat-chunk changes))))

(defn- block-edits-deltas [world events]
  (let [[_ edits] (reduce (fn [[w acc] [tag & args]]
                            (let [ds (case tag
                                       :dig   (dig-deltas w args)
                                       :place (place-deltas w args)
                                       nil)]
                              [(with-edits w ds) (into acc ds)]))
                          [world []]
                          events)]
    (into edits (ack-deltas events))))

(defn block-edits [world events]
  [#(block-edits-deltas world events)])
