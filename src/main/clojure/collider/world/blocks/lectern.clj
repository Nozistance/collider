(ns collider.world.blocks.lectern
  "Lectern with its book and its redstone pulse."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:const impulse-ticks
  "The ticks a lectern stays powered after a page turns."
  2)

(defn lectern?
  "Returns true when st is a lectern."
  [^long st]
  (= :lectern (block/type-of st)))

(defn has-book?
  "Returns true when the lectern st holds a book."
  [^long st]
  (= :true (:has-book (block/props-of st))))

(defn powered?
  "Returns true when the lectern st sends a redstone pulse."
  [^long st]
  (= :true (:powered (block/props-of st))))

(defn reset
  "Returns the lectern st unpowered, with a book when book? is true."
  ^long [^long st book?]
  (block/with st :powered :false :has-book (block/flag book?)))

(defn powered
  "Returns the lectern st powered when on? is true, else unpowered."
  ^long [^long st on?]
  (block/with st :powered (block/flag on?)))

(def rule
  "The block rule that ends the pulse of a powered lectern."
  {:name   :lectern
   :match? (fn [_chunks st _p] (lectern? st))
   :wake   (fn [_chunks _dim _tick _p _old _side] nil)
   :due    (fn [chunks p _ctx]
             (let [st (chunk/chunks-get-block chunks p)]
               (when (and (lectern? st) (powered? st))
                 [[p (powered st false)]])))})
