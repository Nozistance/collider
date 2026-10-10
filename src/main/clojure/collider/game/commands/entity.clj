(ns collider.game.commands.entity
  "The kill, summon, tag and swing commands."
  (:require [collider.game.command.targets :as targets]
            [collider.game.commands.pos :as pos]
            [collider.game.commands.reply
             :refer [answer entity-name fail name-list say success]]
            [collider.game.effect.account :as account]
            [collider.game.entity :as entity]
            [collider.game.entity.hurt :as hurt]
            [collider.game.entity.save-data :as save-data]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.variant :as variant]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(def ^:private generic-kill {:type :generic-kill})

(defn- removed [id e]
  (into [[:remove-entity id]]
        (signal/game-event :entity-die (:pos e) id)))

(defn- slain [world id e]
  (when-let [ds (hurt/damage-deltas
                  world id e Float/MAX_VALUE generic-kill)]
    (->> (hurt/hurt-now world id e ds)
         (hurt/report-deltas world id)
         (into ds))))

(defn- kill-in [lv id e]
  (cond
    (not (entity/living? e)) (removed id e)
    (and (entity/player? e)
         (not (player/client-loaded? e (long (:tick lv)))))
    nil
    :else (slain lv id e)))

(defn- killed [world [id dim e]]
  (targets/in-level world dim (kill-in (targets/level-view world dim) id e)))

(defn- kill-report [eid xs]
  (if (= 1 (count xs))
    (say eid "commands.kill.success.single"
         (entity-name (nth (first xs) 2)))
    (say eid "commands.kill.success.multiple" (count xs))))

(defn- kill-deltas [world eid [s]]
  (let [xs (targets/selected world eid s)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.entity")
      (concat (mapcat #(killed world %) xs)
              (kill-report eid xs)))))

(defn- summon-mob [world eid type at nbt]
  (let [t (:tick world)
        place #(variant/place world (targets/source-dim world) at)]
    (if nbt
      (-> (mobs/new-mob type at nil t)
          (save-data/loaded nbt t)
          (assoc :pos at))
      (mobs/command-mob type at [t eid :summon at] t (place)))))

(defn- summoned [world eid type at nbt]
  (let [dim (targets/source-dim world)
        kind {:translate (str "entity.minecraft." (name type))}
        msg {:translate "commands.summon.success" :with [kind]}
        mob (summon-mob world eid type at nbt)]
    (concat (targets/in-level world dim [[:spawn-entity mob]])
            (success [(out/to eid (out/system-chat msg))]))))

(defn- summon-deltas [world eid [type x y z nbt]]
  (let [p (targets/source-pos world)
        at [(double (or x (nth p 0)))
            (double (or y (nth p 1)))
            (double (or z (nth p 2)))]]
    (cond
      (not (mobs/mob-type? type)) (fail eid "commands.summon.failed")
      (pos/spawnable? at) (summoned world eid type at nbt)
      :else (fail eid "commands.summon.invalidPosition"))))

(def ^:private tag-limit 1024)

(defn- tag-added [world name [id dim e]]
  (let [tags (or (:tags e) #{})]
    (when-not (or (contains? tags name)
                  (>= (count tags) (long tag-limit)))
      (targets/in-level world dim
                    [[:merge-entity id {:tags (conj tags name)}]]))))

(defn- tag-removed [world name [id dim e]]
  (when (contains? (:tags e) name)
    (let [tags (disj (:tags e) name)]
      (targets/in-level world dim [[:merge-entity id {:tags tags}]]))))

(defn- tag-report [eid xs base name]
  (if (= 1 (count xs))
    (say eid (str base "single") name
         (entity-name (nth (first xs) 2)))
    (say eid (str base "multiple") name (count xs))))

(defn- tag-changed [f op]
  (fn [world eid [s name]]
    (let [xs (targets/selected world eid s)
          dss (keep #(f world name %) xs)
          base (str "commands.tag." op ".success.")]
      (cond
        (empty? xs) (fail eid "argument.entity.notfound.entity")
        (empty? dss) (fail eid (str "commands.tag." op ".failed"))
        :else (concat (apply concat dss)
                      (tag-report eid xs base name))))))

(defn- tag-names [tags]
  (name-list (mapv (fn [t] {:text t :color "green"}) (sort tags))))

(defn- tag-list [eid xs tags]
  (let [one? (= 1 (count xs))
        who (when one? (entity-name (nth (first xs) 2)))
        n (count tags)]
    (cond
      (empty? xs) (fail eid "argument.entity.notfound.entity")
      (and one? (zero? n))
      (say eid "commands.tag.list.single.empty" who)
      one? (say eid "commands.tag.list.single.success" who n
                (tag-names tags))
      (zero? n)
      (say eid "commands.tag.list.multiple.empty" (count xs))
      :else (say eid "commands.tag.list.multiple.success" (count xs)
                 n (tag-names tags)))))

(defn- tag-list-deltas [world eid [s]]
  (let [xs (targets/selected world eid s)
        tags (into #{} (mapcat #(:tags (nth % 2))) xs)]
    (answer (tag-list eid xs tags))))

(defn- swung [world hand [id dim e]]
  (when (account/living? e)
    (let [ds (player/swing-deltas id e hand (:tick world) true)]
      (or (targets/in-level world dim ds) []))))

(defn- swing-report [eid xs n]
  (if (= 1 n)
    (say eid "commands.swing.success.single"
         (entity-name (nth (first xs) 2)))
    (say eid "commands.swing.success.multiple" n)))

(defn- swing-deltas [world eid [hand s]]
  (let [xs (if s (targets/selected world eid s) (targets/self world eid))
        dss (keep #(swung world (or hand :main) %) xs)]
    (cond
      (empty? xs) (fail eid "argument.entity.notfound.entity")
      (empty? dss) (fail eid "commands.swing.failed.notliving")
      :else (concat (apply concat dss)
                    (swing-report eid xs (count dss))))))

(def handlers
  {:kill kill-deltas :summon summon-deltas
   :tag-add (tag-changed tag-added "add")
   :tag-remove (tag-changed tag-removed "remove")
   :tag-list tag-list-deltas :swing swing-deltas})
