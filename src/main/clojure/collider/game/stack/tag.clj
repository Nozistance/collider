(ns collider.game.stack.tag
  "Item stacks as tags, as a block entity saves the stacks it holds."
  (:require [collider.data :as data]
            [collider.hash-order :as hash-order]))

(set! *warn-on-reflection* true)

(defn- filterable-tag [{:keys [raw filtered]}]
  (hash-order/compound [[:raw raw] [:filtered filtered]]))

(defn- pages-tag [pages]
  (not-empty (mapv filterable-tag pages)))

(defn- book-tag [{:keys [title author generation pages resolved]}]
  (hash-order/compound
    [[:title (filterable-tag title)] [:author author]
     [:generation (when (pos? (long generation)) (int generation))]
     [:pages (pages-tag pages)]
     [:resolved (when resolved true)]]))

(defn- writable-tag [{:keys [pages]}]
  (hash-order/compound [[:pages (pages-tag pages)]]))

(def ^:private component-tags
  {:written-book-content book-tag
   :writable-book-content writable-tag
   :custom-name identity})

(defn- components-tag [cs]
  (not-empty
    (hash-order/compound
      (keep (fn [[k v]]
              (when-let [f (component-tags k)] [(data/wire k) (f v)]))
            cs))))

(defn of
  "Returns the tag of stack, the entries lead before its own. Of its
  components only the contents of books and the name are kept."
  ([stack] (of stack nil))
  ([stack lead]
   (when stack
     (let [cs (components-tag (:components stack))]
       (hash-order/compound
         (concat lead
                 [[:id (data/wire (:item stack))]
                  [:count (int (:count stack 1))]
                  [:components cs]]))))))

(defn- filterable [m] {:raw (:raw m) :filtered (:filtered m)})

(def ^:private tag-components
  {:written-book-content
   (fn [m]
     {:title (filterable (:title m)) :author (:author m)
      :generation (long (:generation m 0))
      :pages (mapv filterable (:pages m))
      :resolved (boolean (:resolved m))})
   :writable-book-content
   (fn [m] {:pages (mapv filterable (:pages m))})
   :custom-name identity})

(defn- component [[k v]]
  (let [k (data/kebab (name k))]
    (when-let [f (tag-components k)] [k (f v)])))

(defn stack
  "Returns the stack that tag t holds."
  [t]
  (let [cs (into {} (keep component) (:components t))]
    (cond-> {:item (data/kebab (:id t)) :count (long (:count t 1))}
      (seq cs) (assoc :components cs))))
