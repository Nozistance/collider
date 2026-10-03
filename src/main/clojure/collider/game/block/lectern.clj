(ns collider.game.block.lectern
  "The book on a lectern, its pages and its menu."
  (:require [collider.data :as data]
            [collider.game.block.blockentity :as be]
            [collider.game.book :as book]
            [collider.game.changes :as changes]
            [collider.game.out :as out]
            [collider.world.blocks.chest :as chest]
            [collider.world.blocks.lectern :as block-lectern]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:table lectern-books
  (delay (set (data/tag-values "item" "lectern_books"))))

(defn book?
  "Returns true when a lectern takes stack."
  [stack]
  (contains? @lectern-books (:item stack)))

(defn- page-count ^long [stack]
  (let [c (:components stack)
        written (:written-book-content c)
        writable (:writable-book-content c)]
    (cond
      written (count (:pages written))
      writable (count writable)
      :else 0)))

(defn- clamp-page ^long [^long page ^long pages]
  (if (< page 0) 0 (min page (dec pages))))

(defn book-of
  [world m]
  (:book (be/at world (:pos m))))

(defn page
  ^long [world m]
  (long (:page (be/at world (:pos m)) 0)))

(defn- below [[x y z]] [x (dec (long y)) z])

(defn- lectern-set [world pos st]
  (let [base (dec (long (:tick world)))]
    (changes/flagged-deltas world [[pos st]] 3 [(below pos)] base)))

(defn place-book-deltas
  [world pos ^long st stack]
  (let [e (or (be/at world pos) (be/fresh :lectern nil))
        one (book/resolved (assoc stack :count 1))
        e' (assoc e :book one :page 0)]
    (concat
      [[:set-block-entity pos e']]
      (lectern-set world pos (block-lectern/reset st true))
      [(out/all (out/block-sound :item.book.put pos 1.0 1.0))])))

(defn remove-book-deltas
  [world pos]
  (let [st (chest/state-at (:chunks world) pos)
        e (be/at world pos)]
    (cons [:set-block-entity pos (assoc e :book nil :page 0)]
          (lectern-set world pos
                       (block-lectern/reset st false)))))

(defn next-page
  "Returns page want of the book of menu m, kept inside the book."
  ^long [world m ^long want]
  (clamp-page want (page-count (book-of world m))))

(defn- turned-deltas [world pos e ^long p]
  (let [st (chest/state-at (:chunks world) pos)
        at (+ (long (:tick world)) block-lectern/impulse-ticks -1)
        ids [(chunk/block-pos->id pos)]]
    (concat
      [[:set-block-entity pos (assoc e :page p)]]
      (lectern-set world pos (block-lectern/powered st true))
      [[:schedule-ticks {at ids}]
       (out/all (out/level-event :sound-page-turn pos 0))])))

(defn page-deltas
  "Returns the deltas of the lectern of menu m turned to page want,
  or nil when the page stays."
  [world m ^long want]
  (let [pos (:pos m)
        e (be/at world pos)
        p (clamp-page want (page-count (:book e)))]
    (when (not= p (long (:page e 0)))
      (turned-deltas world pos e p))))

(defn layout
  []
  {:count 1 :visible [0]
   :place (fn [_ _] false)
   :swap  (fn ^long [^long _] 0)
   :quick (fn [_ _] nil)})
