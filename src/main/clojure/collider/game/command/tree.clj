(ns collider.game.command.tree
  "What typed commands mean, and their completions."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.world.env.dimension :as dimension]
            [collider.game.command.args :as args]
            [collider.game.command.args.pos :as pos]
            [collider.game.command.args.block :as blocks]
            [collider.game.command.dispatcher :as d]
            [collider.game.command.args.item :as items]
            [collider.game.command.args.particle :as particles]
            [collider.game.command.forms :as forms]
            [collider.game.command.nodes :as node-tree]
            [collider.game.command.nbt-path :as nbt-path]
            [collider.game.command.reader :as r]
            [collider.game.command.selector :as sel]
            [collider.game.command.snbt :as snbt]
            [collider.game.command.suggest :as suggest]
            [collider.game.command.text :as tc]
            [collider.game.mode :as game-mode]
            [collider.game.schema :as schema]))

(set! *warn-on-reflection* true)

(def ^:private readers
  {:ticks #(args/time-arg (:min %))
   :float #(r/float-arg (:min %) (:max %))
   :marker (fn [_] (args/id-arg))
   :sound (fn [_] (args/id-arg))
   :particle (fn [_] (particles/particle-arg))
   :item-predicate (fn [_] (items/item-predicate-arg))
   :block (fn [_] (blocks/block-state-arg))
   :block-predicate (fn [_] (blocks/block-predicate-arg))
   :clock (fn [_] (args/resource-arg "world_clock"))
   :timeline (fn [_] (args/resource-arg "timeline"))})

(defn- game-mode-read [rd]
  (let [[w end] (r/read-unquoted rd)]
    (if-let [m (game-mode/named w)]
      [m end]
      (r/error-at end "argument.gamemode.invalid" w))))

(defn- anchor-read [rd]
  (let [[w end] (r/read-unquoted rd)]
    (if (#{"eyes" "feet"} w)
      [(keyword w) end]
      (r/error-at rd "argument.anchor.invalid" w))))

(def ^:private message-limit 256)

(def ^:private skipped-selector-errors
  #{"argument.entity.selector.missing"
    "argument.entity.selector.unknown"})

(defn- part-read [rd]
  (let [p (sel/parse rd true) err (:error p)]
    (cond (nil? err) [(:sel p) (:rd p)]
          (skipped-selector-errors (:key err)) nil
          :else err)))

(defn- message-parts [[s n0 :as rd]]
  (loop [[_ i :as rd] rd acc []]
    (if (and (r/can-read? rd) (= \@ (nth s i)))
      (let [res (part-read rd)
            [sel [_ e :as end]] (when (vector? res) res)]
        (cond (nil? res) (recur [s (inc i)] acc)
              (r/error? res) res
              :else (recur end (conj acc [(- i n0) (- e n0) sel]))))
      (if (r/can-read? rd) (recur [s (inc i)] acc) [acc rd]))))

(defn- message-read [[s _ :as rd] cx]
  (let [text (r/remaining rd) n (count text)]
    (cond (> n (long message-limit))
          (r/error "argument.message.too_long" n message-limit)
          (not (forms/selectors? cx))
          [{:text text :parts []} [s (count s)]]
          :else (let [res (message-parts rd)]
                  (if (r/error? res)
                    res
                    [{:text text :parts (first res)}
                     (second res)])))))

(defn- component-read [rd]
  (let [res (snbt/read-tag rd)]
    (if (r/error? res)
      res
      (let [[tag end] res [op v] (tc/text-of tag)]
        (case op
          :ok [v end]
          (r/error-at rd "argument.component.invalid" (str v)))))))

(defn- toplevel [rd] [(first rd) 0])

(defn- too-many [players?]
  (if players? "argument.player.toomany" "argument.entity.toomany"))

(defn- targets-read [{:keys [single? players?]} rd cx]
  (let [p (sel/parse rd (forms/selectors? cx)) s (:sel p)]
    (cond (:error p) (:error p)
          (and single? (> (long (:limit s)) 1))
          (r/error-at (toplevel rd) (too-many players?))
          (and players? (:entities? s) (not (:self? s)))
          (r/error-at (toplevel rd) "argument.player.entities")
          :else [s (:rd p)])))

(defn- int-reader [lo hi] (:parse (r/int-arg lo hi)))

(defn- rule-value-read [{:keys [type min max]}]
  (let [f (if (= :bool type)
            r/read-boolean
            (int-reader (or min Integer/MIN_VALUE)
                        (or max Integer/MAX_VALUE)))]
    (fn [[s n :as rd]]
      (let [res (f rd)]
        (if (r/error? res)
          res
          [(subs s n (second (second res))) (second res)])))))

(defn- plain-reader [kind]
  (case kind
    :game-mode game-mode-read
    :anchor anchor-read
    :bool r/read-boolean
    :word r/read-unquoted
    (:greedy :text) r/read-greedy
    :component component-read
    :dimension args/read-id
    :compound snbt/read-compound
    :nbt-path nbt-path/read-path
    :double r/read-double
    nil))

(defn- reader-for [[_ [kind opts]]]
  (or (plain-reader kind)
      (case kind
        :int (int-reader (:min opts) (:max opts))
        :effect-seconds (int-reader 1 1000000)
        :coord (:parse (pos/block-pos-arg))
        :dcoord (:parse (pos/vec3-arg (:center opts true)))
        :angle (:parse (pos/rotation-arg))
        :duration (:parse (args/time-arg (:min opts)))
        :mob-effect (:parse (args/resource-arg "mob_effect"))
        :entity-type (:parse (args/resource-arg "entity_type"))
        :item (:parse (items/item-stack-arg))
        :rule-value (rule-value-read opts)
        (:parse ((readers kind) opts)))))

(defn- arg-parse [[_ [kind opts] :as a]]
  (case kind
    :targets (fn [rd cx _] (targets-read opts rd cx))
    :message (fn [rd cx _] (message-read rd cx))
    (let [f (reader-for a)] (fn [rd _ _] (f rd)))))

(defn- dimension-of [s]
  (let [k (data/kebab (str/replace (str s) #"^minecraft:" ""))]
    (some #{k} schema/dims)))

(defn- scale ^double [from to]
  (/ (double (:coordinate-scale (dimension/type-of from)))
     (double (:coordinate-scale (dimension/type-of to)))))

(defn- scaled [origin from to]
  (if (or (nil? origin) (= from to))
    origin
    (let [k (scale from to)]
      [(* k (double (nth origin 0))) (nth origin 1)
       (* k (double (nth origin 2)))])))

(defn- arg-of [n]
  (or (:arg (meta n))
      (when (= :dimension (:parser n)) [:dimension [:dimension {}]])))

(defn- engine-node [n]
  (let [m (meta n) a (arg-of n)]
    (with-meta
      (cond-> (update n :children #(mapv engine-node %))
        (:executable? n) (assoc :command (:way m))
        (= :argument (:type n))
        (assoc :parse (arg-parse a)
               :suggest (suggest/arg-suggest a #((reader-for a) %))))
      m)))

(defn- usable-top [need n]
  (let [lv (need (:name n))]
    (assoc n :usable? #(forms/allowed? (:level %) lv))))

(defn- graph-of [extra]
  (let [top (partial usable-top (forms/needs extra))
        root (-> (node-tree/root-node forms/owner-level extra)
                 (node-tree/with-paths [])
                 engine-node
                 (update :children #(mapv top %)))]
    (into {:root root} (map (fn [n] [(:name n) n]))
          (:children root))))

(def ^:private ^:table base-graph (delay (graph-of nil)))

(def ^:private extra-graph (memoize graph-of))

(defn graph
  "Returns the parse graph of the commands with the plugin forms
  extra."
  [extra]
  (if (empty? extra) @base-graph (extra-graph extra)))

(defn- arg-width ^long [[_ [kind]]]
  (case kind (:coord :dcoord) 3 :angle 2 1))

(defn- resolved [[_ [kind]] v src]
  (case kind
    :coord (mapv long (pos/block-pos v src))
    :dcoord (pos/position v src)
    :angle (mapv (fn [{k :kind x :value}] [(= :relative k) x]) v)
    [v]))

(defn- node-values [{n :node raw :value} src]
  (let [{a :arg lit :value} (meta n)]
    (cond (nil? a) []
          (not= :literal (:type n)) (resolved a raw src)
          (= :literal (first (second a))) []
          :else [(if (some? lit) lit (:name n))])))

(defn- consumed ^long [nodes]
  (transduce (keep #(some-> (:arg (meta (:node %))) arg-width)) + 0
             nodes))

(def ^:private ^:table self-selector
  (delay (:sel (sel/parse (r/reader "@s") true))))

(defn- default-of [[_ [_ {v :default}]]]
  (if (= :self v) @self-selector v))

(defn- delta [{[args action] :command nodes :nodes} src]
  (-> action
      (into (mapcat #(node-values % src)) nodes)
      (into (map default-of) (drop (consumed nodes) args))))

(defn- relative-of [v]
  (if (= :local (:kind (first v)))
    [0 1 2]
    (keep-indexed #(when (= :relative (:kind %2)) %1) v)))

(defn- relative-axes [nodes]
  (let [kind #(first (second (:arg (meta (:node %)))))]
    (set (some #(when (= :dcoord (kind %)) (relative-of (:value %)))
               nodes))))

(defn- moved-to [src raw]
  (if-let [to (dimension-of raw)]
    (assoc src :dim to :pos (scaled (:pos src) (:dim src) to))
    (r/error "argument.dimension.invalid" raw)))

(defn- stop-at-error [x] (if (r/error? x) (reduced x) x))

(defn- modified [src ctx]
  (reduce (fn [src {n :node raw :value}]
            (if (= :dimension (:parser n))
              (stop-at-error (moved-to src raw))
              src))
          src (:nodes ctx)))

(defn- final-source [chain src]
  (reduce #(stop-at-error (modified %1 %2)) src (butlast chain)))

(defn- failed [{:keys [key args cursor]}]
  (cond-> {:failure {:translate key :with (vec args)}}
    (>= (long cursor) 0) (assoc :cursor cursor)))

(defn- meaning [chain src]
  (let [ctx (last chain)]
    (cond-> {:delta (delta ctx src)
             :relative (relative-axes (:nodes ctx))}
      (next chain) (assoc :dim (:dim src) :origin (:pos src)))))

(defn- parsed [g text origin dim level rot]
  (let [res (d/parse g (subs text 1) 0 {:level level})
        err (d/failure res)
        chain (d/chain (:ctx res))
        src {:pos (or origin [0.0 0.0 0.0]) :rot rot :dim dim}
        src (when-not err (final-source chain src))]
    (cond err (failed err)
          (r/error? src) (failed src)
          :else (meaning chain src))))

(defn parse
  "Returns the delta the typed command means.
  It also returns the axes written relative to the source as
  :relative, or else a :failure message and the :cursor it points
  at. Relative coordinates count from origin in level dim, local
  ones also from rotation rot. A command run through a redirect
  also returns its level and origin as :dim and :origin. Forms
  extra add the commands of plugins."
  ([text] (parse text nil))
  ([text origin] (parse text origin :overworld))
  ([text origin dim] (parse text origin dim forms/gamemaster))
  ([text origin dim level] (parse text origin dim level [0.0 0.0]))
  ([text origin dim level rot] (parse text origin dim level rot nil))
  ([text origin dim level rot extra]
   (parsed (graph extra) text origin dim level rot)))

(defn- command-suggestions [world text cx]
  (let [g (graph (forms/extra-of world))]
    (d/suggestions (d/parse g text 1 cx) text cx)))

(defn suggestions
  "Returns the completions for half-typed text as :texts that
  replace it from index :start. A player of permission level sees
  the commands it may run."
  ([world text] (suggestions world text forms/gamemaster))
  ([world text level]
   (let [text (or text "")
         start (inc (long (or (str/last-index-of text " ") -1)))
         cx {:level level :dim (:dim world :overworld)
             :players (sort (keys (:players world)))}]
     (if (str/starts-with? text "/")
       (command-suggestions world text cx)
       (->> (suggest/suggest-player world (subs text start))
            (suggest/sugg start))))))
