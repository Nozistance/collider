(ns collider.game.book
  "Books a player writes in and signs, and written books it reads."
  (:require [collider.game.out :as out]
            [collider.game.stack :as stack]))

(set! *warn-on-reflection* true)

(defn- page [s] {:raw s :filtered nil})

(defn- book-slot [^long slot]
  (cond (<= 0 slot 8) (+ 36 slot)
        (= 40 slot) 45))

(defn- written [e s title pages]
  (-> (stack/transmute s :written-book)
      (stack/put :writable-book-content nil)
      (stack/put :written-book-content
                 {:title (page title) :author (:name e)
                  :generation 0 :pages (mapv page pages)
                  :resolved true})))

(defn- edited [e s {:keys [title pages]}]
  (if title
    (written e s title pages)
    (stack/put s :writable-book-content (mapv page pages))))

(defn- sent-deltas [e eid slot s]
  [[:set-slot eid slot s]
   (out/to eid (out/set-slot slot s))
   [:client-slots eid {slot s} (get-in e [:track :carried])]])

(defn edit-deltas
  "Returns the deltas of a player that edits or signs the book in
  hotbar slot n, or 40 for the off hand."
  [e eid n book]
  (when-let [slot (book-slot n)]
    (let [s (get-in e [:inventory slot])]
      (when (stack/has? s :writable-book-content)
        (sent-deltas e eid slot (edited e s book))))))

(defn resolved
  "Returns stack with its written book content marked resolved."
  [s]
  (let [c (stack/component s :written-book-content)]
    (if (or (nil? c) (:resolved c))
      s
      (stack/put s :written-book-content (assoc c :resolved true)))))

(defn open-deltas
  "Returns the deltas of a player that reads the written book it holds
  in hand at slot."
  [e eid hand slot]
  (let [s (get-in e [:inventory slot])
        s' (resolved s)]
    (when (stack/has? s :written-book-content)
      (conj (if (identical? s s') [] (sent-deltas e eid slot s'))
            (out/to eid (out/open-book hand))))))
