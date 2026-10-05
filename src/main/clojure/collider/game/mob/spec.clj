(ns collider.game.mob.spec
  "The parts of a mob turn that differ by mob type."
  (:require [collider.game.mob.animal :as animal]
            [collider.game.mob.armadillo :as armadillo]
            [collider.game.mob.armadillo-ai :as armadillo-ai]
            [collider.game.mob.chicken :as chicken]
            [collider.game.mob.cow :as cow]
            [collider.game.mob.goat-ai :as goat-ai]
            [collider.game.mob.mooshroom :as mooshroom]
            [collider.game.mob.pig :as pig]
            [collider.game.mob.rabbit :as rabbit]
            [collider.game.mob.sheep :as sheep]))

(set! *warn-on-reflection* true)

(def ^:private mushroom-results
  [mooshroom/bowl-result mooshroom/shear-result
   mooshroom/flower-result cow/milk-result animal/feed-result])

(def ^:private rabbit-kind
  {:think rabbit/brain :goals rabbit/spec :ai-step rabbit/ai-step
   :jump-share rabbit/jump-share :hop rabbit/hopped
   :bump rabbit/bumped :steer rabbit/steered :bites? rabbit/raiding?
   :results [animal/feed-result]})

(def ^:private armadillo-kind
  {:brain armadillo-ai/breed :custom-step armadillo/custom-step
   :ai-step armadillo/ai-step
   :results [armadillo/brush-result armadillo/scared-result
             animal/feed-result]})

(def ^:private goat-kind
  {:brain goat-ai/breed
   :results [cow/milk-result animal/feed-result]})

(def ^:private kinds
  {:sheep {:think sheep/brain :goals sheep/spec :bites? sheep/biting?
           :results [sheep/shear-result animal/feed-result]}
   :cow {:think cow/brain :goals cow/spec
         :results [cow/milk-result animal/feed-result]}
   :mooshroom {:think mooshroom/brain :goals mooshroom/spec
               :results mushroom-results}
   :pig {:think pig/brain :goals pig/spec
         :results [animal/feed-result]}
   :chicken {:think chicken/brain :goals chicken/spec
             :ai-step chicken/ai-step :results [animal/feed-result]}
   :rabbit rabbit-kind
   :armadillo armadillo-kind
   :goat goat-kind})

(defn of
  "Returns the parts of the turn of a mob of type, or nil for a type
  without them. A mob thinks with goals by :think and :goals, or
  with the compiled brain :brain. Its own server step :custom-step
  comes after either."
  [type]
  (kinds type))

(def child-looks
  (update-vals kinds #(:child-look (:goals %) animal/parent-look)))
