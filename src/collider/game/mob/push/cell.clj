(ns collider.game.mob.push.cell
  "The bodies one cell of the push grid holds.")

(deftype PushCell [^longs eids ^doubles xs ^doubles ys ^doubles zs
                   ^doubles halfs ^doubles heights])
