(ns collider.game.block.smithing
  "Smithing recipes and their results."
  (:require [collider.data :as data]
            [collider.game.block.menu :as menu]
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

(defn- slots-result [inv]
  (result (get inv 0) (get inv 1) (get inv 2)))

(defn- may-place? [slot stack]
  (case (long slot)
    0 (template? stack)
    1 (base? stack)
    2 (addition? stack)
    3 false))

(defn- quick [v slot]
  (let [i (menu/index-of v slot)
        total (count v)]
    (cond
      (= 3 i) (menu/span v 4 total true)
      (< i 3) (menu/span v 4 total false)
      :else {:try  (menu/span v 0 3 false)
             :else (if (< i 31)
                     (menu/span v 31 total false)
                     (menu/span v 4 31 false))})))

(defn- derive-result [inv]
  (if-let [r (slots-result inv)]
    (assoc inv 3 r)
    (dissoc inv 3)))

(defn- taken [inv]
  (-> inv (menu/shrink 0) (menu/shrink 1) (menu/shrink 2)))

(defn layout
  "Returns the slot layout of a smithing table menu."
  []
  (let [base (menu/slots-layout 4 may-place?)
        v (:visible base)]
    (assoc base
      :result 3
      :no-gather #{3}
      :stat {:slot 3 :by :taken}
      :quick (fn [_ slot] (quick v slot))
      :on-take taken
      :derive derive-result)))

(defn changed
  "Returns smithing menu m flagged when its three full input slots
  of items make nothing."
  [m items]
  (let [bad? (and (every? some? (take 3 items))
                  (nil? (slots-result items)))]
    (assoc m :recipe-error? (boolean bad?))))
