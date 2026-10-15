(ns collider.game.potion
  "The contents of a potion, its effects and its colour."
  (:require [collider.data :as data]
            [collider.game.effect :as effect]
            [collider.game.stack :as stack]))

(set! *warn-on-reflection* true)

(def ^:private ^:const base-color -13083194)

(defn- contents [stack]
  (get-in stack [:components :potion-contents]))

(defn water?
  "Returns true when stack holds plain water."
  [stack]
  (let [c (contents stack)]
    (and (= :water (:potion c)) (empty? (:custom-effects c)))))

(defn- brewed [c]
  (get (data/potions) (:potion c)))

(defn- effects [c]
  (concat (brewed c) (:custom-effects c)))

(defn- visible? [e]
  (get-in e [:details :show-particles] true))

(defn- amplifier ^long [e]
  (long (or (:amplifier e) (get-in e [:details :amplifier]) 0)))

(defn- effect-color ^long [e]
  (long (:color (get (data/mob-effects) (:effect e)) 0)))

(defn- color-byte ^long [e ^long shift]
  (bit-and (bit-shift-right (effect-color e) shift) 0xFF))

(defn- channel ^long [rows ^long shift ^long weight]
  (let [add (fn ^long [^long a e]
              (+ a (* (inc (amplifier e)) (color-byte e shift))))]
    (quot (reduce add 0 rows) weight)))

(defn- blend [effects]
  (let [rows (filterv visible? effects)
        w (reduce (fn [^long a e] (+ a (inc (amplifier e)))) 0 rows)]
    (when (pos? w)
      (bit-or -16777216 (bit-shift-left (channel rows 16 w) 16)
              (bit-shift-left (channel rows 8 w) 8)
              (channel rows 0 w)))))

(defn color
  "Returns the colour the client paints the splash and cloud with."
  ^long [stack]
  (let [c (contents stack)]
    (long (or (:custom-color c) (blend (effects c))
              base-color))))

(defn has-effects?
  "Returns true when the potion in stack carries any effect, its own
  or a custom one."
  [stack]
  (let [c (contents stack)]
    (boolean (or (seq (:custom-effects c)) (seq (brewed c))))))

(defn instant?
  "Returns true when effect k acts at once."
  [k]
  (boolean (:instant? (get (data/mob-effects) k))))

(defn has-instant-effects?
  "Returns true when the potion itself acts at once.
  Custom effects do not count."
  [stack]
  (boolean (some (comp instant? :effect) (brewed (contents stack)))))

(defn- empty-contents? [c]
  (and (nil? (:potion c)) (nil? (:custom-color c))
       (empty? (:custom-effects c)) (nil? (:custom-name c))))

(defn tipped?
  "Returns true when stack carries a potion of any kind."
  [stack]
  (not (empty-contents? (contents stack))))

(defn arrow-color
  "Returns the colour of an arrow that carries stack, -1 for none."
  ^long [stack]
  (if (tipped? stack) (color stack) -1))

(defn- instance-of [e]
  (let [d (:details e)
        n (or (:duration e) (:duration d) 0)]
    (if d
      (effect/instance n (amplifier e) (:ambient d)
                       (:show-particles d) (:show-icon d))
      (effect/instance n (amplifier e)))))

(defn instances
  "Returns the [k instance] pairs of the potion in stack, in order."
  [stack]
  (for [e (effects (contents stack))]
    [(:effect e) (instance-of e)]))

(defn duration-scale
  "Returns the part of their time the effects in stack last."
  ^double [stack]
  (double (or (stack/component stack :potion-duration-scale) 1.0)))

