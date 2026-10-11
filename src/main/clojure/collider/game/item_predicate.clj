(ns collider.game.item-predicate
  "Item predicates: the stacks a lock, a loot condition or a command
  test takes. A predicate is read from its tag, then tested."
  (:require [collider.data :as data]
            [collider.game.command.components :as cs]
            [collider.game.command.decode :as dfu
             :refer [by-name enum-of bool-of]]
            [collider.game.command.snbt :as snbt]
            [collider.game.command.text :as tc]
            [collider.game.stack :as stack]))

(set! *warn-on-reflection* true)

(defn- double-of [tag]
  (if (number? tag) [:ok (double tag)] [:malformed "Not a number"]))

(defn- bounds [number]
  (let [span (dfu/record [:min :min number :opt] [:max :max number :opt])
        exactly (dfu/mapped number #(hash-map :min % :max %))]
    (fn [tag]
      (let [[op {lo :min hi :max} :as r]
            (dfu/settled tag (dfu/either (span tag) (exactly tag)))]
        (if (and (= :ok op) lo hi (> lo hi))
          [:malformed (str "Swapped bounds in range: Optional[" lo
                           "] is higher than Optional[" hi "]")]
          r)))))

(def ^:private int-bounds (bounds dfu/int-of))

(def ^:private double-bounds (bounds double-of))

(defn- holders [registry]
  (dfu/holder-set registry (dfu/fixed registry)))

(defn- collection [element]
  (dfu/record
    [:contains :contains (dfu/listed element) :opt]
    [:count :count (dfu/listed (dfu/record [:test :test element :req]
                                           [:count :count int-bounds :req]))
     :opt]
    [:size :size int-bounds :opt]))

(def ^:private enchantments
  (dfu/listed (dfu/record [:enchantments :enchantments
                           (holders "enchantment") :opt]
                          [:levels :levels int-bounds :opt])))

(def ^:private firework
  (dfu/record [:shape :shape
               (enum-of [:small-ball :large-ball :star :creeper :burst])
               :opt]
              [:has_twinkle :twinkle bool-of :opt]
              [:has_trail :trail bool-of :opt]))

(def ^:private modifier
  (dfu/record [:attribute :attribute (holders "attribute") :opt]
              [:id :id dfu/identifier :opt]
              [:amount :amount double-bounds :opt]
              [:operation :operation
               (enum-of [:add-value :add-multiplied-base
                         :add-multiplied-total])
               :opt]
              [:slot :slot
               (enum-of [:any :mainhand :offhand :hand :feet :legs :chest
                         :head :armor :body :saddle])
               :opt]))

(declare decode)

(def ^:private items-of
  (dfu/record [:items :items (collection #(decode %)) :opt]))

(def ^:private kinds
  {:damage (dfu/record [:durability :durability int-bounds :opt]
                       [:damage :damage int-bounds :opt])
   :enchantments enchantments
   :stored-enchantments enchantments
   :potion-contents (holders "potion")
   :custom-data cs/tag-data
   :container items-of
   :bundle-contents items-of
   :firework-explosion firework
   :fireworks (dfu/record [:explosions :explosions (collection firework)
                           :opt]
                          [:flight_duration :flight int-bounds :opt])
   :writable-book-content
   (dfu/record [:pages :pages (collection dfu/string-of) :opt])
   :written-book-content
   (dfu/record [:pages :pages (collection tc/text-of) :opt]
               [:author :author dfu/string-of :opt]
               [:title :title dfu/string-of :opt]
               [:generation :generation int-bounds :opt]
               [:resolved :resolved bool-of :opt])
   :attribute-modifiers
   (dfu/record [:modifiers :modifiers (collection modifier) :opt])
   :trim (dfu/record [:material :material (holders "trim_material") :opt]
                     [:pattern :pattern (holders "trim_pattern") :opt])
   :jukebox-playable
   (dfu/record [:song :song (holders "jukebox_song") :opt])
   :villager/variant (holders "villager_type")})

(defn kind
  "Returns the decoder of the value of predicate type k."
  [k]
  (kinds k))

(defn- predicate-key [s]
  (let [p ((by-name "data_component_predicate_type") s)
        c ((by-name "data_component_type") s)]
    (cond (= :ok (first p)) [:ok [:predicate (second p)]]
          (= :ok (first c)) [:ok [:exists (second c)]]
          :else (dfu/either p c))))

(defn- predicate-value [[how k]]
  (if (= :exists how) dfu/tag-unit (kinds k)))

(def ^:private exact
  (dfu/mapped (dfu/dispatched-map (by-name "data_component_type")
                                  (fn [k] #(cs/decode k %)))
              vec))

(def ^:private item-predicate
  (dfu/record [:items :items (holders "item") :opt]
              [:count :count int-bounds :opt]
              [:components :components exact :opt]
              [:predicates :predicates
               (dfu/mapped (dfu/dispatched-map predicate-key
                                               predicate-value)
                           vec)
               :opt]))

(defn decode
  "Decodes item predicate tag."
  [tag]
  (dfu/settled tag (item-predicate tag)))

(defn- members [registry v]
  (cond (keyword? v) #{v}
        (map? v) (let [[_ path] (dfu/parse-id (dfu/full-id (:tag v)))]
                   (set (data/tag-values registry path)))
        :else (set v)))

(defn- in? [b x]
  (let [{lo :min hi :max} (if (number? b) {:min b :max b} b)]
    (and (or (nil? lo) (<= lo x)) (or (nil? hi) (<= x hi)))))

(defn component
  "Returns the value of component k on stack s, the default of its
  item included."
  [s k]
  (or (stack/component s k)
      (when-not (or (contains? (:components s) k)
                    (contains? (:removed s) k))
        (cs/default-value (:item s) k))))

(defn- every-test? [ps xs test]
  (empty? (reduce (fn [left x]
                    (let [l (remove #(test % x) left)]
                      (if (empty? l) (reduced l) l)))
                  ps xs)))

(defn- collection? [{:keys [contains size] counts :count} xs test]
  (and (every-test? contains xs test)
       (every? (fn [c] (in? (:count c) (count (filter #(test (:test c) %) xs))))
               counts)
       (or (nil? size) (in? size (count xs)))))

(defn- enchanted? [{es :enchantments levels :levels} m]
  (cond es (boolean (some #(let [n (long (get m % 0))]
                             (and (pos? n) (or (nil? levels) (in? levels n))))
                          (members "enchantment" es)))
        levels (boolean (some #(in? levels %) (vals m)))
        :else (boolean (seq m))))

(defn- template-stack [{:keys [item patch] n :count}]
  (cond-> {:item item :count n}
    (:components patch) (assoc :components (:components patch))
    (:removed patch) (assoc :removed (:removed patch))))

(defn- fired? [p x]
  (every? (fn [[pk xk]] (or (nil? (pk p)) (= (pk p) (xk x))))
          [[:shape :shape] [:twinkle :twinkle] [:trail :trail]]))

(defn- text= [a b]
  (let [plain #(if (string? %) {:text %} %)] (= (plain a) (plain b))))

(defn- modifier? [p {:keys [attribute modifier slot]}]
  (and (or (nil? (:attribute p))
           (contains? (members "attribute" (:attribute p)) attribute))
       (or (nil? (:id p)) (= (:id p) (:id modifier)))
       (or (nil? (:amount p)) (in? (:amount p) (:amount modifier)))
       (or (nil? (:operation p)) (= (:operation p) (:operation modifier)))
       (or (nil? (:slot p)) (= (:slot p) slot))))

(declare matches?)

(defn- book? [p v]
  (and (or (nil? (:author p)) (= (:author p) (:author v)))
       (or (nil? (:title p)) (= (:title p) (:raw (:title v))))
       (or (nil? (:generation p)) (in? (:generation p) (:generation v 0)))
       (or (nil? (:resolved p)) (= (:resolved p) (boolean (:resolved v))))
       (or (nil? (:pages p))
           (collection? (:pages p) (:pages v) #(text= %1 (:raw %2))))))

(defn- held? [p xs]
  (or (nil? (:items p))
      (collection? (:items p) (map template-stack (remove nil? xs))
                   matches?)))

(defn- kind? [k p v s]
  (case k
    :damage (let [md (long (or (component s :max-damage) 0))]
              (and (some? v)
                   (or (nil? (:durability p))
                       (in? (:durability p) (- md (long v))))
                   (or (nil? (:damage p)) (in? (:damage p) v))))
    (:enchantments :stored-enchantments)
    (and (some? v) (every? #(enchanted? % v) p))
    :potion-contents
    (boolean (and v (contains? (members "potion" p) (:potion v))))
    :custom-data (snbt/compare-nbt p (or v {}) true)
    (:container :bundle-contents) (and (some? v) (held? p v))
    :firework-explosion (and (some? v) (fired? p v))
    :fireworks (and (some? v)
                    (or (nil? (:explosions p))
                        (collection? (:explosions p) (:explosions v)
                                     fired?))
                    (or (nil? (:flight p))
                        (in? (:flight p) (:flight-duration v))))
    :writable-book-content
    (and (some? v)
         (or (nil? (:pages p))
             (collection? (:pages p) v #(= %1 (:raw %2)))))
    :written-book-content (and (some? v) (book? p v))
    :attribute-modifiers
    (and (some? v)
         (or (nil? (:modifiers p))
             (collection? (:modifiers p) v modifier?)))
    :trim (and (some? v)
               (or (nil? (:material p))
                   (contains? (members "trim_material" (:material p))
                              (:material v)))
               (or (nil? (:pattern p))
                   (contains? (members "trim_pattern" (:pattern p))
                              (:pattern v))))
    :jukebox-playable
    (and (some? v)
         (or (nil? (:song p))
             (contains? (members "jukebox_song" (:song p)) (:song v))))
    :villager/variant
    (boolean (and v (contains? (members "villager_type" p) v)))))

(defn kind-matches?
  "Returns true when stack s passes predicate p of type k."
  [k p s]
  (kind? k p (component s k) s))

(defn- passes? [s [key p]]
  (if (= :exists (when (vector? key) (first key)))
    (some? (component s (peek key)))
    (kind-matches? (if (vector? key) (peek key) key) p s)))

(defn matches?
  "Returns true when stack s, nil for none, passes item predicate p.
  Its predicates may also be a map of type to value, as loot holds
  them, and a bound a number."
  [p s]
  (let [s (or s {:item :air :count 0})]
    (and (or (nil? (:items p)) (contains? (members "item" (:items p))
                                          (:item s)))
         (or (nil? (:count p)) (in? (:count p) (stack/size s)))
         (every? (fn [[k v]] (= v (component s k))) (:components p))
         (every? #(passes? s %) (:predicates p)))))
