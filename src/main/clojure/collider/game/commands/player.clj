(ns collider.game.commands.player
  "Commands on players, such as give, clear, effect and title."
  (:require [collider.data :as data]
            [collider.game.command.args.item :as item-args]
            [collider.game.command.reader :as cmd-reader]
            [collider.game.command.selector :as sel]
            [collider.game.commands.pos :as pos]
            [collider.game.commands.reply
             :refer [dimension-id entity-name fail say say*]]
            [collider.game.effect :as effect]
            [collider.game.effect.account :as account]
            [collider.game.experience :as xp]
            [collider.game.inventory :as inventory]
            [collider.game.item :as item]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.stack :as stack]
            [collider.random :as random]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(def ^:private rarity-colors
  {:common "white" :uncommon "yellow" :rare "aqua"
   :epic "light_purple"})

(defn- item-name [item]
  {:translate "chat.square_brackets"
   :with [{:text "" :extra [(data/item-title item)]}]
   :color (rarity-colors (data/rarity item))
   :hover {:action :show-item :id item}})

(defn- given-deltas [world [id dim e] proto n]
  (let [lv (sel/level-view world dim)
        inv (or (:inventory e) {})
        stack (assoc proto :count n)
        [changes left] (inventory/add-stack inv stack)
        set (fn [[slot s]] [:set-slot id slot s])
        spilled (when left
                  [[:spawn-entity (item/dropped lv id left true 0)]])]
    (sel/in-level world dim (concat (map set changes) spilled))))

(defn- given [world eid [_ _ e :as x] proto n]
  (concat (given-deltas world x proto n)
          (say eid "commands.give.success.single"
               n (item-name (:item proto)) (entity-name e))))

(defn- give-limit [proto]
  (* 100 (long (or (stack/component proto :max-stack-size) 1))))

(defn- give-deltas [world eid [s input n]]
  (let [xs (sel/player-selected world eid s)
        proto (item-args/item-stack input 1)
        most (when-not (cmd-reader/error? proto) (give-limit proto))]
    (cond
      (empty? xs) (fail eid "argument.entity.notfound.player")
      (cmd-reader/error? proto)
      (apply fail eid (:key proto) (:args proto))
      (> (long n) (long most))
      (fail eid "commands.give.failed.toomanyitems" most
            (item-name (:item proto)))
      :else (mapcat #(given world eid % proto n) xs))))

(def ^:private inventory-order
  "Menu slots in the order a clear visits them. The hotbar comes
  first, followed by the main slots, the armour from feet to head,
  the off hand and the crafting grid."
  (vec (concat (range 36 45) (range 9 36) [8 7 6 5 45] [1 2 3 4])))

(defn- taken ^long [pred ^long limit ^long counted stack]
  (let [n (stack/size stack)]
    (cond
      (not (and stack (item-args/matches? pred stack))) 0
      (zero? limit) n
      (neg? (- limit counted)) n
      :else (min (- limit counted) n))))

(defn- shrunk [stack ^long k]
  (when (< k (stack/size stack)) (update stack :count - k)))

(defn- clear-step [pred limit]
  (fn [[n changes] [slot stack]]
    (let [k (taken pred limit n stack)]
      [(+ (long n) k)
       (cond-> changes
         (and (pos? k) (not (zero? (long limit))))
         (conj [slot (shrunk stack k)]))])))

(defn- cleared
  "Returns how many items pred matches on player e, and the slot
  changes that take at most limit of them. A limit of 0 only counts,
  and -1 takes all."
  [e pred limit]
  (let [inv (:inventory e)
        stacks (conj (mapv (fn [s] [s (get inv s)]) inventory-order)
                     [:carried (:carried e)])]
    (reduce (clear-step (or pred {:type nil :tests []}) limit)
            [0 []] stacks)))

(defn- clear-change [id [slot stack]]
  (if (= :carried slot)
    [:merge-entity id {:carried stack}]
    [:set-slot id slot stack]))

(defn- clear-report [eid xs n limit]
  (let [kind (if (zero? (long limit)) "test" "success")]
    (if (= 1 (count xs))
      (say eid (str "commands.clear." kind ".single") n
           (entity-name (nth (first xs) 2)))
      (say eid (str "commands.clear." kind ".multiple") n
           (count xs)))))

(defn- clear-failure [eid xs]
  (if (= 1 (count xs))
    (fail eid "clear.failed.single" (:name (nth (first xs) 2)))
    (fail eid "clear.failed.multiple" (count xs))))

(defn- cleared-deltas [world [id dim [_ cs]]]
  (sel/in-level world dim (map #(clear-change id %) cs)))

(defn- players-or-self [world eid s]
  (if s (sel/player-selected world eid s) (sel/self world eid)))

(defn- clear-deltas [world eid [s pred limit]]
  (let [xs (players-or-self world eid s)
        limit (long (or limit -1))
        rs (map (fn [[id dim e]] [id dim (cleared e pred limit)]) xs)
        n (reduce + (map #(first (nth % 2)) rs))]
    (cond
      (empty? xs) (fail eid "argument.entity.notfound.player")
      (zero? (long n)) (clear-failure eid xs)
      :else (concat (mapcat #(cleared-deltas world %) rs)
                    (clear-report eid xs n limit)))))

(defn- chimes [e acc]
  (for [vol (:chimes acc)]
    (out/all (out/sound :player/levelup (:pos e) vol 1.0))))

(defn- xp-changed [world [id dim e] f]
  (let [acc (f (sel/xp-of world e))
        fields (xp/player-fields acc)
        ds (cons [:merge-entity id fields] (chimes e acc))]
    (sel/in-level world dim ds)))

(defn- xp-unit [unit] (or unit "points"))

(defn- xp-report [eid xs op unit amount]
  (let [base (str "commands.experience." op "." unit ".success.")]
    (if (= 1 (count xs))
      (say eid (str base "single") amount
           (entity-name (nth (first xs) 2)))
      (say eid (str base "multiple") amount (count xs)))))

(defn- xp-add-deltas [world eid [s amount unit]]
  (let [xs (sel/player-selected world eid s)
        unit (xp-unit unit)
        f (if (= "levels" unit) xp/give-levels xp/give-points)
        g (fn [a] (f a amount))]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.player")
      (concat (mapcat #(xp-changed world % g) xs)
              (xp-report eid xs "add" unit amount)))))

(defn- settable? [world unit amount [_ _ e]]
  (or (= "levels" unit)
      (< (long amount) (xp/needed (:level (sel/xp-of world e))))))

(defn- xp-set-deltas [world eid [s amount unit]]
  (let [xs (sel/player-selected world eid s)
        unit (xp-unit unit)
        f (if (= "levels" unit) xp/set-levels xp/set-points)
        g (fn [a] (f a amount))
        ok (filter #(settable? world unit amount %) xs)]
    (cond
      (empty? xs) (fail eid "argument.entity.notfound.player")
      (empty? ok) (fail eid "commands.experience.set.points.invalid")
      :else (concat (mapcat #(xp-changed world % g) ok)
                    (xp-report eid xs "set" unit amount)))))

(defn- xp-query-deltas [world eid [s unit]]
  (if-let [[_ _ e] (first (sel/player-selected world eid s))]
    (let [acc (sel/xp-of world e)
          n (if (= "levels" unit) (:level acc) (xp/points acc))]
      (say eid (str "commands.experience.query." unit)
           (entity-name e) n))
    (fail eid "argument.entity.notfound.player")))

(defn- effect-title [k]
  {:translate (str "effect.minecraft." (data/snake k))})

(defn- given-ticks ^long [k secs]
  (cond
    (nil? secs) (if (effect/instant? k) 1 600)
    (= :infinite secs) effect/infinite
    (effect/instant? k) (long secs)
    :else (* 20 (long secs))))

(defn- effect-changed [world [id dim e] f]
  (when (account/living? e)
    (let [acc (f (account/account id e))]
      (when (:landed? acc)
        (or (sel/in-level world dim (account/deltas acc e)) [])))))

(defn- effect-report [eid xs n key-of with]
  (if (= 1 (count xs))
    (apply say eid (key-of "single")
           (with (entity-name (nth (first xs) 2))))
    (apply say eid (key-of "multiple") (with n))))

(defn- effect-run [world eid xs f fail-key key-of with]
  (let [dss (keep #(effect-changed world % f) xs)]
    (if (empty? dss)
      (fail eid fail-key)
      (concat (apply concat dss)
              (effect-report eid xs (count xs) key-of with)))))

(defn- effect-give-deltas [world eid [s k secs amp hide]]
  (let [xs (sel/selected world eid s)
        d (given-ticks k secs)
        i (effect/instance d (or amp 0) false (not hide))
        key-of #(str "commands.effect.give.success." %)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.entity")
      (effect-run world eid xs #(account/land % k i)
                  "commands.effect.give.failed" key-of
                  (fn [who] [(effect-title k) who (quot d 20)])))))

(defn- clear-parts [k]
  (if k
    ["specific" #(account/take-off % k) #(vector (effect-title k) %)]
    ["everything" account/take-all vector]))

(defn- effect-clear-deltas [world eid [s k]]
  (let [xs (if s (sel/selected world eid s) (sel/self world eid))
        [what f with] (clear-parts k)
        base (str "commands.effect.clear." what)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.entity")
      (effect-run world eid xs f (str base ".failed")
                  #(str base ".success." %) with))))

(defn mode-name
  "Returns the name of game mode mode as chat shows it."
  [mode]
  {:translate (str "gameMode." (name mode))})

(defn- told-of-mode [world id mode]
  (when (get-in world [:rules :send-command-feedback] true)
    [(out/to id (out/system-chat
                  {:translate "gameMode.changed"
                   :with [(mode-name mode)]}))]))

(defn- mode-report
  [world eid [id _ e] mode]
  (if (= id eid)
    (say eid "commands.gamemode.success.self" (mode-name mode))
    (concat (told-of-mode world id mode)
            (say eid "commands.gamemode.success.other"
                 (entity-name e) (mode-name mode)))))

(defn mode-change
  "Returns the deltas that put the selected player x in game mode
  mode."
  [world [id dim e] mode]
  (game-mode/change (sel/level-view world dim) id e mode))

(defn mode-set
  "Returns the deltas and replies of player eid putting the selected
  player x in game mode mode, nil when it is in that mode."
  [world eid mode [_ dim :as x]]
  (when-let [ds (seq (mode-change world x mode))]
    (concat (sel/in-level world dim ds)
            (mode-report world eid x mode))))

(defn- gamemode-deltas [world eid [mode s]]
  (let [xs (sel/player-selected world eid s)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.player")
      (mapcat #(mode-set world eid mode %) xs))))

(defn- spawn-report [eid xs with]
  (if (= 1 (count xs))
    (say* eid "commands.spawnpoint.success.single"
          (conj with (entity-name (nth (first xs) 2))))
    (say* eid "commands.spawnpoint.success.multiple"
          (conj with (count xs)))))

(defn- spawnpoint-set [world eid xs at [yaw pitch]]
  (let [dim (sel/source-dim world)
        spawn {:dimension dim :pos at :yaw (double yaw)
               :pitch (double pitch)}
        forced {:forced-spawn spawn}
        set (fn [[id d]]
              (sel/in-level world d [[:merge-entity id forced]]))
        with (conj (mapv str at) (str yaw) (str pitch)
                   (dimension-id dim))]
    (concat (mapcat set xs) (spawn-report eid xs with))))

(defn- spawnpoint-deltas [world eid [s x y z yaw pitch]]
  (let [xs (sel/player-selected world eid s)]
    (cond
      (empty? xs) (fail eid "argument.entity.notfound.player")
      (pos/out-of-bounds? [x y z])
      (fail eid "argument.pos.outofbounds")
      :else (let [at (pos/block-under world [x y z])
                  turn (pos/turn-of world eid yaw pitch)]
              (spawnpoint-set world eid xs at turn)))))

(defn- sound-range-sq ^double [volume]
  (let [v (float volume)
        r (float (if (> v (float 1.0)) (* (float 16.0) v) 16.0))]
    (double (* r r))))

(defn- heard
  "Returns the position and volume player e hears a sound at, nil
  when it is out of range."
  [e at volume min-volume]
  (let [p (:pos e) [px py pz] [(v/x p) (v/y p) (v/z p)]
        d (mapv - at [px py pz])
        sq (reduce + (map * d d))]
    (cond
      (<= sq (sound-range-sq volume)) [at volume]
      (<= (double min-volume) 0.0) nil
      :else (let [n (Math/sqrt sq)]
              [(mapv #(+ %1 (* (/ %2 n) 2.0)) [px py pz] d)
               min-volume]))))

(defn- sound-listeners [world eid s]
  (let [dim (sel/source-dim world)
        xs (players-or-self world eid s)]
    (filter #(= dim (nth % 1)) xs)))

(defn- sound-report [eid played id]
  (if (= 1 (count played))
    (say eid "commands.playsound.success.single" id
         (entity-name (nth (first played) 2)))
    (say eid "commands.playsound.success.multiple" id
         (count played))))

(defn- playsound-result [world eid s id played]
  (cond
    (and s (empty? (sel/player-selected world eid s)))
    (fail eid "argument.entity.notfound.player")
    (empty? played) (fail eid "commands.playsound.failed")
    :else (concat (map second played)
                  (sound-report eid (map first played) id))))

(defn- player-hears [at volume least sound]
  (fn [[pid _ e :as x]]
    (when-let [[p v] (heard e at volume least)]
      [x (out/to pid (sound p v))])))

(defn- playsound-deltas
  [world eid [src id s x y z volume pitch min-volume]]
  (let [at (if (some? x) [x y z] (vec (sel/source-pos world)))
        volume (float (or volume 1.0))
        least (or min-volume 0.0)
        seed (random/mix64 (hash [(:tick world) eid id]))
        src (or src "master") pitch (or pitch 1.0)
        sound #(out/named-sound id src %1 %2 pitch seed)
        play (player-hears at volume least sound)
        played (keep play (sound-listeners world eid s))]
    (playsound-result world eid s id played)))

(def ^:private ^:const particle-near-sq (* 32.0 32.0))

(def ^:private ^:const particle-far-sq (* 512.0 512.0))

(defn- centre-sq ^double [e at]
  (let [p (:pos e)
        c [(v/x p) (v/y p) (v/z p)]
        d #(- (+ (Math/floor (double %1)) 0.5) (double %2))]
    (reduce + (map #(* (d %1 %2) (d %1 %2)) c at))))

(defn- sees-particle?
  "Returns true when the centre of the block player e stands in is
  nearer to at than 32 blocks, or 512 when forced."
  [e at force?]
  (< (centre-sq e at) (if force? particle-far-sq particle-near-sq)))

(defn- particle-id [p]
  (data/wire (data/entry-name "particle_type" (first p))))

(defn- particle-fx [p at [dx dy dz] speed n force?]
  (out/particle p at [(or dx 0.0) (or dy 0.0) (or dz 0.0)]
                (or speed 0.0) (or n 0) force?))

(defn- particle-viewers [world eid s at force?]
  (let [dim (sel/source-dim world)
        near? (fn [[_ d e]] (and (= d dim) (sees-particle? e at force?)))]
    (filter near? (if s
                    (sel/player-selected world eid s)
                    (sel/player-entries world)))))

(defn- particle-deltas
  [world eid [mode p x y z dx dy dz speed n s]]
  (let [at (if (some? x) [x y z] (vec (sel/source-pos world)))
        force? (= mode :force)
        shown (particle-viewers world eid s at force?)
        fx (particle-fx p at [dx dy dz] speed n force?)]
    (cond
      (and s (empty? (sel/player-selected world eid s)))
      (fail eid "argument.entity.notfound.player")
      (empty? shown) (fail eid "commands.particle.failed")
      :else (concat (map (fn [[id]] (out/to id fx)) shown)
                    (say eid "commands.particle.success"
                         (particle-id p))))))

(defn- stop-report [src id]
  (let [src (when (not= "*" src) src)]
    (cond
      (and src id) ["commands.stopsound.success.source.sound" id src]
      src ["commands.stopsound.success.source.any" src]
      id ["commands.stopsound.success.sourceless.sound" id]
      :else ["commands.stopsound.success.sourceless.any"])))

(defn- stopsound-deltas [world eid [src s id]]
  (let [xs (sel/player-selected world eid s)
        m (out/stop-sound id (when (not= "*" src) src))]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.player")
      (concat (map (fn [[pid]] (out/to pid m)) xs)
              (apply say eid (stop-report src id))))))

(defn- title-report [eid xs key]
  (if (= 1 (count xs))
    (say eid (str key ".single") (entity-name (nth (first xs) 2)))
    (say eid (str key ".multiple") (count xs))))

(defn- titled [key fx]
  (fn [world eid [s & more]]
    (let [xs (sel/player-selected world eid s)
          m (apply fx more)]
      (if (empty? xs)
        (fail eid "argument.entity.notfound.player")
        (concat (map (fn [[id]] (out/to id m)) xs)
                (title-report eid xs key))))))

(defn- shown [kind]
  (titled (str "commands.title.show." (name kind))
          #(out/title kind %)))

(def handlers
  {:give give-deltas :clear clear-deltas
   :xp-add xp-add-deltas :xp-set xp-set-deltas
   :xp-query xp-query-deltas
   :effect-give effect-give-deltas :effect-clear effect-clear-deltas
   :gamemode gamemode-deltas :spawnpoint spawnpoint-deltas
   :playsound playsound-deltas :stopsound stopsound-deltas
   :particle particle-deltas
   :title-clear (titled "commands.title.cleared"
                        #(out/clear-titles false))
   :title-reset (titled "commands.title.reset"
                        #(out/clear-titles true))
   :title-title (shown :title) :title-subtitle (shown :subtitle)
   :title-actionbar (shown :actionbar)
   :title-times (titled "commands.title.times" out/title-times)})
