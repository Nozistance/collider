(ns collider.world.env.difficulty
  "Difficulty of the world.")

(set! *warn-on-reflection* true)

(defn id
  "Returns the difficulty id of the world, always peaceful."
  ^long [_ctx]
  0)
