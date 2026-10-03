(ns collider.game.mob.spec
  "The parts of a mob turn that differ by mob type."
  (:require [collider.game.mob.animal :as animal]
            [collider.game.mob.chicken :as chicken]
            [collider.game.mob.cow :as cow]
            [collider.game.mob.mooshroom :as mooshroom]
            [collider.game.mob.pig :as pig]
            [collider.game.mob.rabbit :as rabbit]
            [collider.game.mob.sheep :as sheep]))

(set! *warn-on-reflection* true)

(def ^:private mushroom-results
  [mooshroom/bowl-result mooshroom/shear-result
   mooshroom/flower-result cow/milk-result animal/feed-result])

(def ^:private rabbit-kind
  {:brain rabbit/brain :goals rabbit/spec :ai-step rabbit/ai-step
   :jump-share rabbit/jump-share :hop rabbit/hopped
   :bump rabbit/bumped :steer rabbit/steered :bites? rabbit/raiding?
   :results [animal/feed-result]})

(def ^:private kinds
  {:sheep {:brain sheep/brain :goals sheep/spec :bites? sheep/biting?
           :results [sheep/shear-result animal/feed-result]}
   :cow {:brain cow/brain :goals cow/spec
         :results [cow/milk-result animal/feed-result]}
   :mooshroom {:brain mooshroom/brain :goals mooshroom/spec
               :results mushroom-results}
   :pig {:brain pig/brain :goals pig/spec
         :results [animal/feed-result]}
   :chicken {:brain chicken/brain :goals chicken/spec
             :ai-step chicken/ai-step :results [animal/feed-result]}
   :rabbit rabbit-kind})

(defn of
  "Returns the parts of the turn of a mob of type, or nil for a type
  without them."
  [type]
  (kinds type))

(def goals
  (update-vals kinds :goals))
