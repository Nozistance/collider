(ns collider.game.command.block-args
  (:require [collider.data :as data]
            [collider.game.command.args :as args]
            [collider.game.command.reader :as r]
            [collider.game.command.snbt :as snbt]
            [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(def ^:private ^:table block-ids
  (delay (into {} (map (fn [k] [(data/wire k) k]))
               (keys (data/blocks)))))

(def ^:private ^:table block-tags
  (delay (into {}
               (map (fn [[t bs]] [(str "minecraft:" t) (set bs)]))
               (get (data/tags) "block"))))

(defn- at? [[s n :as rd] c]
  (and (r/can-read? rd) (= c (nth s n))))

(defn- skip [[s n]] [s (inc n)])

(defn- lookup [ids [id end] rd k]
  (if-let [v (ids id)]
    [v end]
    (r/error-at rd k id)))

(defn- read-block [rd]
  (let [res (args/read-id rd)]
    (if (r/error? res)
      res
      (lookup @block-ids res rd "argument.block.id.invalid"))))

(defn- read-tag [rd]
  (let [res (args/read-id (skip rd))]
    (cond (r/error? res) res
          (contains? @block-tags (first res)) res
          :else
          (r/error-at rd "arguments.block.tag.unknown" (first res)))))

(defn- property-of [b key]
  (some #(when (= key (data/snake %)) %)
        (keys (get-in (data/blocks) [b :props]))))

(defn- int-value [values raw]
  (try (let [v (keyword (str (Integer/parseInt raw)))]
         (some #{v} values))
       (catch NumberFormatException _ nil)))

(defn- integer-property? [values]
  (every? #(re-matches #"-?[0-9]+" (name %)) values))

(defn- value-of [b prop raw]
  (let [values (get-in (data/blocks) [b :props prop])]
    (if (integer-property? values)
      (int-value values raw)
      (some #(when (= raw (name %)) %) values))))

(defn- unclosed [rd]
  (r/error-at rd "argument.block.property.unclosed"))

(defn- read-key [b id props rd]
  (let [res (r/read-string rd)
        [k end] (when-not (r/error? res) res)
        prop (when k (property-of b k))]
    (cond (r/error? res) res
          (nil? prop)
          (r/error-at rd "argument.block.property.unknown" id k)
          (contains? props prop)
          (r/error-at rd "argument.block.property.duplicate" k id)
          :else [[prop k] (r/skip-whitespace end)])))

(defn- read-value [b id [prop k] rd]
  (let [res (r/read-string rd)
        v (when-not (r/error? res) (value-of b prop (first res)))]
    (cond (r/error? res) res
          (nil? v)
          (r/error-at rd "argument.block.property.invalid"
                      id (first res) k)
          :else [v (second res)])))

(defn- read-pair [b id props rd]
  (let [res (read-key b id props rd)
        [[prop k :as key] end] (when-not (r/error? res) res)]
    (cond (r/error? res) res
          (not (at? end \=))
          (r/error-at end "argument.block.property.novalue" k id)
          :else
          (let [at (r/skip-whitespace (skip end))
                vr (read-value b id key at)]
            (if (r/error? vr)
              vr
              [(assoc props prop (first vr)) (second vr)])))))

(defn- after-value [props rd]
  (let [end (r/skip-whitespace rd)]
    (cond (not (r/can-read? end)) [props end false]
          (at? end \,) [props (skip end) true]
          (at? end \]) [props end false]
          :else (unclosed end))))

(defn- close [[props rd]]
  (if (r/can-read? rd) [props (skip rd)] (unclosed rd)))

(defn- read-props [pair start]
  (loop [props {} rd (r/skip-whitespace (skip start))]
    (if (or (not (r/can-read? rd)) (at? rd \]))
      (close [props rd])
      (let [res (pair props (r/skip-whitespace rd))
            nx (if (r/error? res) res (apply after-value res))]
        (cond (r/error? nx) nx
              (nx 2) (recur (nx 0) (nx 1))
              :else (close nx))))))

(defn- vague-key [props rd]
  (let [res (r/read-string rd)
        [k end] (when-not (r/error? res) res)
        end (when k (r/skip-whitespace end))]
    (cond (r/error? res) res
          (contains? props k)
          (r/error-at rd "argument.block.property.duplicate"
                      k "minecraft:")
          (not (at? end \=))
          (r/error-at rd "argument.block.property.novalue"
                      k "minecraft:")
          :else [k (r/skip-whitespace (skip end))])))

(defn- vague-pair [props rd]
  (let [res (vague-key props rd)
        [k at] (when-not (r/error? res) res)
        vr (when k (r/read-string at))]
    (cond (r/error? res) res
          (r/error? vr) vr
          (r/can-read? (r/skip-whitespace (second vr)))
          [(assoc props k (first vr)) (second vr)]
          :else (unclosed at))))

(defn- read-nbt [v rd]
  (if (at? rd \{)
    (let [res (snbt/read-compound rd)]
      (if (r/error? res)
        res
        [(assoc v :nbt (first res)) (second res)]))
    [(assoc v :nbt nil) rd]))

(defn- with-props [v read rd]
  (if (at? rd \[)
    (let [res (read-props read rd)]
      (if (r/error? res)
        res
        (read-nbt (assoc v :props (first res)) (second res))))
    (read-nbt (assoc v :props {}) rd)))

(defn- parse-block [rd]
  (let [res (read-block rd)
        [b end] (when-not (r/error? res) res)
        id (when b (data/wire b))]
    (if (r/error? res)
      res
      (let [out (with-props {:block b} (partial read-pair b id) end)]
        (if (r/error? out)
          out
          (let [[{:keys [props] :as v} rd'] out]
            [(-> (assoc v :state (block/state b props))
                 (dissoc :block))
             rd']))))))

(defn- parse-tag [rd]
  (let [res (read-tag rd)]
    (if (r/error? res)
      res
      (with-props {:tag (first res)} vague-pair (second res)))))

(defn- block-value [{:keys [state props nbt]}]
  {:state state :props props :nbt nbt})

(defn- tidy [res]
  (if (r/error? res) res (update res 0 block-value)))

(defn block-state-arg []
  {:id "minecraft:block_state"
   :parse (fn [rd]
            (if (at? rd \#)
              (r/error-at rd "argument.block.tag.disallowed")
              (tidy (parse-block rd))))})

(defn block-predicate-arg []
  {:id "minecraft:block_predicate"
   :parse (fn [rd]
            (if (at? rd \#) (parse-tag rd) (tidy (parse-block rd))))})

(defn- nbt-match? [pred nbt]
  (or (nil? (:nbt pred))
      (and (some? nbt) (snbt/compare-nbt (:nbt pred) nbt true))))

(defn- block-match? [{:keys [state props]} st]
  (let [b (block/name-of state) have (block/props-of st)]
    (and (= b (block/name-of st))
         (every? (fn [[k v]] (= v (get have k))) props))))

(defn- vague-match? [b have [key raw]]
  (let [prop (property-of b key)]
    (and (some? prop)
         (some? (value-of b prop raw))
         (= (value-of b prop raw) (get have prop)))))

(defn- tag-match? [{:keys [tag props]} st]
  (let [b (block/name-of st) have (block/props-of st)]
    (and (contains? (get @block-tags tag) b)
         (every? #(vague-match? b have %) props))))

(defn matches? [pred state nbt]
  (and (if (:tag pred)
         (tag-match? pred state)
         (block-match? pred state))
       (nbt-match? pred nbt)))
