(ns collider.game.command.nodes
  "The command node tree that clients get."
  (:require [collider.game.command.forms :as forms]
            [collider.game.gamerules :as rules]))

(set! *warn-on-reflection* true)

(def ^:private brigadier-integer (keyword "brigadier:integer"))

(def ^:private brigadier-bool (keyword "brigadier:bool"))

(def ^:private brigadier-string (keyword "brigadier:string"))

(def ^:private brigadier-float (keyword "brigadier:float"))

(def ^:private ask-server "minecraft:ask_server")

(def ^:private plain-arguments
  {:duration [:time {:min 1}]
   :coord [:block-pos nil]
   :dcoord [:vec3 nil]
   :angle [:rotation nil]
   :block [:block-state nil]
   :item [:item-stack nil]
   :entity-type [:resource {:registry "minecraft:entity_type"}]
   :text [brigadier-string {:kind 0}]
   :game-mode [:gamemode nil]
   :mob-effect [:resource {:registry "minecraft:mob_effect"}]
   :bool [brigadier-bool nil]
   :greedy [brigadier-string {:kind 2}]
   :item-predicate [:item-predicate nil]
   :block-predicate [:block-predicate nil]
   :anchor [:entity-anchor nil]
   :message [:message nil]
   :component [:component nil]
   :dimension [:dimension nil]})

(defn- entity-props [single? players?]
  {:single? (boolean single?) :players? (boolean players?)})

(defn- clock-nodes [n kind {:keys [min max]}]
  (case kind
    :ticks [[n :time {:min min}]]
    :float [[n brigadier-float {:min min :max max}]]
    (:marker :sound) [[n :resource-location nil ask-server]]
    :clock [[n :resource {:registry "minecraft:world_clock"}]]
    :timeline [[n :resource {:registry "minecraft:timeline"}
                ask-server]]))

(defn- shaped-nodes [n kind opts]
  (let [{:keys [min max values single? players?]} opts]
    (case kind
      :int [[n brigadier-integer {:min min :max max}]]
      :effect-seconds
      {:or-literals ["infinite"]
       :arg [n brigadier-integer {:min 1 :max 1000000}]}
      :enum {:literals (sort values)}
      :literal {:literals [(:name opts)]}
      :targets [[n :entity (entity-props single? players?)]]
      :word (cond-> [n brigadier-string {:kind 0}]
              (:tags opts) (conj ask-server)
              :always vector)
      :rule {:rules true}
      (clock-nodes n kind opts))))

(defn- argument-nodes [[nm [kind opts]]]
  (if-let [[parser props] (plain-arguments kind)]
    [[(name nm) parser props]]
    (shaped-nodes (name nm) kind opts)))

(defn- coords-merged [args]
  (loop [as args acc []]
    (if-let [[nm [kind opts] :as a] (first as)]
      (if (and (#{:coord :dcoord :angle} kind)
               (= 0 (long (:axis opts 0))))
        (recur (drop (if (= :angle kind) 2 3) as)
               (conj acc [(keyword (:node opts nm)) [kind opts]]))
        (recur (rest as) (conj acc a)))
      acc)))

(defn- optional? [[_ [_ opts]]] (contains? opts :default))

(defn- optional-from [args]
  (or (first (keep-indexed
               (fn [i _] (when (every? optional? (drop i args)) i))
               args))
      (count args)))

(defn- rule-value [{:keys [type min max] :as spec} way]
  (with-meta
    {:type :argument :name "value" :executable? true
     :parser (if (= :bool type) brigadier-bool brigadier-integer)
     :props (when (= :int type) {:min min :max max})}
    {:arg [:value [:rule-value spec]] :way way}))

(defn- rule-nodes [way]
  (vec (for [[rule spec] rules/table
             :let [id (rules/wire-name rule)]
             nm [(subs id 10) id]]
         (with-meta {:type :literal :executable? true :name nm
                     :children [(rule-value spec way)]}
           {:value rule}))))

(defn- literal-nodes [ls exec? children]
  (mapv (fn [l] {:type :literal :name l :executable? exec?
                 :children children})
        ls))

(defn- spec-nodes [spec exec? children way]
  (cond
    (:rules spec) (rule-nodes way)
    (:or-literals spec)
    (into (spec-nodes [(:arg spec)] exec? children way)
          (map #(with-meta % {:value (keyword (:name %))}))
          (literal-nodes (:or-literals spec) exec? children))
    (:literals spec) (literal-nodes (:literals spec) exec? children)
    :else
    (let [[n parser props suggests] (first spec)]
      [(cond-> {:type :argument :name n :parser parser :props props
                :executable? exec? :children children}
         suggests (assoc :suggests suggests))])))

(defn- chain [way args ^long i]
  (if (>= i (count args))
    [[] true]
    (let [optional (optional-from args)
          a (nth args i)
          spec (argument-nodes a)
          tail (if (:rules spec) [[] true] (chain way args (inc i)))
          [kids tail-exec] tail
          exec? (or tail-exec (>= (inc i) optional))]
      [(mapv #(vary-meta % merge {:arg a :way way})
             (spec-nodes spec exec? kids way))
       (>= i optional)])))

(defn- same-node? [a b]
  (let [k #(dissoc % :children :executable?)]
    (= (k a) (k b))))

(defn index-of
  "Returns the index of the first of xs that pred accepts, or nil."
  [xs pred]
  (first (keep-indexed (fn [i x] (when (pred x) i)) xs)))

(declare merged-nodes)

(defn- merged-node [a b]
  (-> a
      (update :executable? #(boolean (or % (:executable? b))))
      (update :children
              #(merged-nodes (into (vec %) (:children b))))
      (with-meta (if (and (:executable? b) (not (:executable? a)))
                   (meta b)
                   (meta a)))))

(defn- merged-nodes [nodes]
  (reduce (fn [acc n]
            (if-let [i (index-of acc #(same-node? % n))]
              (update acc i merged-node n)
              (conj acc n)))
          [] nodes))

(defn- form-node [form]
  (if (forms/subcommands? form)
    {:type :literal :name (forms/cmd-name form)
     :children (mapv form-node (drop 2 form))}
    (let [ws (forms/ways form)
          chains (mapv #(chain % (coords-merged (first %)) 0) ws)
          way (some (fn [[w c]] (when (second c) w))
                    (map vector ws chains))]
      (with-meta
        {:type :literal :name (forms/cmd-name form)
         :executable? (some? way)
         :children (merged-nodes (into [] (mapcat first) chains))}
        {:way way}))))

(def ^:private dimension-node
  {:type :argument :name "dimension" :parser :dimension :props nil
   :children [] :redirect "execute"})

(def ^:private execute-node
  {:type :literal :name "execute"
   :children [{:type :literal :name "in" :children [dimension-node]}
              {:type :literal :name "run" :children []
               :redirect :root}]})

(defn- alias-node [[alias target]]
  {:type :literal :name alias :children [] :redirect target})

(defn- flat [nodes node]
  (let [step (fn [[ns ks] c]
               (let [[ns i] (flat ns c)] [ns (conj ks i)]))
        [nodes kids] (reduce step [nodes []] (:children node))]
    [(conj nodes (assoc node :children kids)) (count nodes)]))

(defn- redirected [nodes]
  (let [root (dec (count nodes))
        top (into {} (map (fn [i] [(:name (nth nodes i)) i]))
                  (:children (peek nodes)))
        at #(if (= :root %) root (top %))]
    (mapv #(if (:redirect %) (update % :redirect at) %) nodes)))

(defn- alias-nodes [ok?]
  (into [] (comp (filter (comp ok? key)) (map alias-node))
        forms/aliases))

(defn root-node
  "Returns the root of the commands that permission level may run,
  with the plugin forms extra."
  [level extra]
  (let [need (forms/needs extra)
        ok? #(forms/allowed? level (need %))
        all (into forms/commands (map forms/plain-form) extra)
        fs (filter (comp ok? forms/cmd-name) all)]
    {:type :root
     :children (-> (mapv form-node fs)
                   (cond-> (ok? "execute") (conj execute-node))
                   (into (alias-nodes ok?)))}))

(defn flattened
  "Returns the nodes under root in a vector, root last.
  Children and redirects name nodes by index."
  [root]
  (redirected (first (flat [] root))))

(defn tree
  "Returns the command nodes a client of permission level gets with
  the command forms extra, the root last."
  ([] (tree forms/gamemaster))
  ([level] (tree level nil))
  ([level extra] (flattened (root-node level extra))))

(defn with-paths
  "Returns node n with the path of names to each node as :path."
  [n path]
  (let [p (if (:name n) (conj path (:name n)) path)]
    (-> (assoc n :path p)
        (update :children (fn [cs] (mapv #(with-paths % p) cs))))))
