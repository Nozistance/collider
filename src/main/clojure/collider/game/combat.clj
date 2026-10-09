(ns collider.game.combat
  "How armor, resistance, protection and absorption take damage off."
  (:require [collider.data :as data]
            [collider.game.attribute :as attribute]
            [collider.game.enchantment :as enchantment]
            [collider.game.slots :as slots]
            [collider.num :as num]))

(set! *warn-on-reflection* true)

(defn- clamp ^double [^double x ^double lo ^double hi]
  (max lo (min hi x)))

(defn after-armor
  "Returns damage after armor of the given toughness."
  ^double [damage armor toughness]
  (let [d (num/f32 damage) a (num/f32 armor)
        t (num/f32 (+ 2.0 (num/f32 (/ (num/f32 toughness) 4.0))))
        real (clamp (num/f32 (- a (num/f32 (/ d t))))
                    (num/f32 (* a 0.2)) 20.0)
        frac (num/f32 (/ real 25.0))]
    (num/f32 (* d (num/f32 (- 1.0 frac))))))

(defn after-protection
  "Returns damage after protection points."
  ^double [damage points]
  (let [real (clamp (num/f32 points) 0.0 20.0)]
    (num/f32 (* (num/f32 damage)
                (num/f32 (- 1.0 (num/f32 (/ real 25.0))))))))

(defn tagged?
  "Returns true when damage of type t is in damage type tag tag."
  [t tag]
  (contains? (set (data/tag-values "damage_type" tag)) t))

(defn- armor-of ^double [e]
  (Math/floor (attribute/value e (:effects e) :armor)))

(defn- toughness-of ^double [e]
  (num/f32 (attribute/value e (:effects e) :armor-toughness)))

(defn- armored ^double [e src ^double d]
  (if (tagged? (:type src) "bypasses_armor")
    d
    (after-armor d (armor-of e) (toughness-of e))))

(defn- resisted ^double [e src ^double d]
  (let [amp (get-in e [:effects :resistance :amplifier])]
    (if (and amp (not (tagged? (:type src) "bypasses_resistance")))
      (let [absorb (* (inc (long amp)) 5)
            left (- 25 absorb)]
        (max (num/f32 (/ (num/f32 (* d left)) 25.0)) 0.0))
      d)))

(defn- worn-enchantments [e]
  (keep #(get-in e [:inventory % :components :enchantments])
        (vals slots/armor)))

(defn- protected ^double [e src ^double d]
  (let [t (:type src)
        points (reduce + 0.0 (map #(enchantment/protection
                                     % (fn [tag] (tagged? t tag)))
                                  (worn-enchantments e)))]
    (if (pos? points) (after-protection d points) d)))

(defn- magic ^double [e src ^double d]
  (if (tagged? (:type src) "bypasses_effects")
    d
    (let [d (resisted e src d)]
      (cond (<= d 0.0) 0.0
            (tagged? (:type src) "bypasses_enchantments") d
            :else (protected e src d)))))

(defn damage-after
  "Returns the damage d from src that reaches the health and
  absorption of living entity e, after its armor, resistance and
  protection."
  ^double [e src ^double d]
  (magic e src (armored e src d)))
