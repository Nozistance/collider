(ns collider.game.block.smithing
  "The smithing recipes and what they make of three items."
  (:require [collider.data :as data]
            [collider.game.stack :as stack]))

(set! *warn-on-reflection* true)

(def ^:private ^:table recipes (delay (data/smithing-recipes)))

(defn- property-set [name]
  (set (get-in (data/recipes) [:property-sets name])))

(def ^:private ^:table template-items
  (delay (property-set "smithing_template")))

(def ^:private ^:table base-items
  (delay (property-set "smithing_base")))

(def ^:private ^:table addition-items
  (delay (property-set "smithing_addition")))

(defn template? [stack] (contains? @template-items (:item stack)))

(defn base? [stack] (contains? @base-items (:item stack)))

(defn addition? [stack] (contains? @addition-items (:item stack)))

(defn- fits? [items stack]
  (if items
    (and (some? stack) (contains? items (:item stack)))
    (nil? stack)))

(defn- matches? [r template base addition]
  (and (fits? (:template r) template)
       (fits? (:base r) base)
       (fits? (:addition r) addition)))

(defn recipe-for
  "Returns the smithing recipe the three slots call for, or nil."
  [template base addition]
  (first (filter #(matches? % template base addition) @recipes)))

(defn- with-patch [out src]
  (reduce #(stack/put %1 %2 nil)
          (reduce-kv stack/put out (:components src))
          (:removed src)))

(defn- transformed [r base]
  (let [out (:result r)]
    (-> {:item (:item out) :count (long (:count out 1))}
        (with-patch base)
        (with-patch {:components (:components out)}))))

(defn- trimmed [r base addition]
  (when-let [material (data/trim-material (:item addition))]
    (let [trim {:material material :pattern (:pattern r)}]
      (when-not (= trim (stack/component base :trim))
        (stack/put (assoc base :count 1) :trim trim)))))

(defn assemble
  "Returns what the recipe makes of the three slots, or nil."
  [r template base addition]
  (when r
    (if (= :trim (:type r))
      (trimmed r base addition)
      (transformed r base))))

(defn result
  "Returns what a smithing table offers for the three slots."
  [template base addition]
  (assemble (recipe-for template base addition)
            template base addition))
