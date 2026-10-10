(ns collider.game.slots
  "Slot numbers of the player inventory window.")

(def ^:const crafting-result 0)

(def crafting-grid [1 2 3 4])

(def armor {:head 5 :chest 6 :legs 7 :feet 8})

(def main (vec (range 9 36)))

(def ^:const hotbar 36)

(def ^:const hotbar-end 44)

(def ^:const offhand 45)

(def ^:const offhand-button 40)

(defn of-hotbar
  "Returns the window slot of hotbar place n, or of the off hand for
  button 40. Returns nil for any other n."
  [^long n]
  (cond (<= 0 n 8) (+ hotbar n)
        (= offhand-button n) offhand))
