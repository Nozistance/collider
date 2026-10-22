(ns collider.game.block.analog
  "The signal a comparator reads from a block whose block
  entity decides it."
  (:require [collider.data :as data]
            [collider.game.block.blockentity :as be]
            [collider.game.stack :as stack]
            [collider.math :as math]
            [collider.world.block :as block]
            [collider.world.blocks.chest :as chest]
            [collider.world.blocks.lectern :as lectern]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def ^:private ^:table outputs
  (delay (update-keys (update-vals (data/pack "jukebox_song")
                                   #(get % "comparator_output"))
                      data/kebab)))

(defn- part ^double [s]
  (math/f32 (/ (double (stack/size s)) (double (stack/max-size s)))))

(defn filled
  "Returns the signal of a container that holds items in its slots,
  by how full they are on average."
  ^long [items]
  (let [add (fn [^double a s] (if s (math/f32 (+ a (part s))) a))
        sum (reduce add 0.0 items)
        f (math/f32 (/ sum (double (count items))))]
    (+ (math/floor (math/f32 (* f 14.0))) (if (pos? f) 1 0))))

(defn- entity
  "Returns the block entity of kind k at p in bes, nil when the block
  there changed kind since bes was taken."
  [bes p k]
  (let [e (get-in bes [(chunk/block-chunk p) p])]
    (when (= k (:kind e)) e)))

(defn- blocked? [chunks p]
  (block/conductor? (chunk/at chunks (dir/toward p :up))))

(defn- chest-signal
  "Returns the signal of the chest st at p, read with its other half
  when it pairs, none when a block sits on either half."
  ^long [bes chunks p ^long st]
  (let [k (be/kind st)
        e (entity bes p k)
        q (chest/partner chunks p st)
        f (when q (entity bes q k))]
    (cond
      (or (nil? e) (blocked? chunks p)) 0
      (nil? q) (filled (:items e))
      (blocked? chunks q) 0
      (nil? f) (filled (:items e))
      (= :right (:type (block/props-of st)))
      (filled (into (:items e) (:items f)))
      :else (filled (into (:items f) (:items e))))))

(defn page-count
  "Returns the pages of book."
  ^long [book]
  (let [c (:components book)]
    (if-let [w (:written-book-content c)]
      (count (:pages w))
      (count (:writable-book-content c)))))

(defn- lectern-signal ^long [e ^long st]
  (let [book (:book e)
        n (page-count book)
        f (if (> n 1)
            (math/f32 (/ (double (:page e 0)) (double (dec n))))
            1.0)]
    (if (lectern/has-book? st)
      (+ (math/floor (math/f32 (* f 14.0))) (if book 1 0))
      0)))

(defn- shelf-signal ^long [e ^long st side]
  (if (= side (dir/opposite (block/facing-of st)))
    (reduce (fn [^long m ^long i]
              (if (nth (:items e) i nil) (bit-set m i) m))
            0 (range 3))
    0))

(defn- song-signal ^long [e]
  (if-let [song (data/jukebox-song (:item (:record e)))]
    (long (get @outputs song))
    0))

(defn- of-entity ^long [e ^long st side k]
  (case k
    :decorated-pot (filled [(:item e)])
    :lectern (lectern-signal e st)
    :jukebox (song-signal e)
    :chiseled-bookshelf (inc (long (:last-slot e)))
    :shelf (shelf-signal e st side)
    (filled (:items e))))

(def ^:private read-kinds
  #{:decorated-pot :lectern :jukebox :chiseled-bookshelf :shelf
    :chest :trapped-chest :barrel :shulker-box :furnace :blast-furnace
    :smoker :brewing-stand :hopper :dispenser})

(defn signal
  "Returns the signal block st at p gives a comparator on side, as
  the block entities bes hold it. Throws for a block whose signal is
  not read yet."
  [bes chunks p st side]
  (let [k (be/kind (long st))
        e (entity bes p k)]
    (cond
      (not (contains? read-kinds k))
      (throw (ex-info "analog output the level decides is not read yet"
                      {:pos p :block (block/block-of st)}))
      (#{:chest :trapped-chest} k) (chest-signal bes chunks p (long st))
      e (of-entity e (long st) side k)
      :else 0)))
