(ns chess.uci)

(defn valid-move? [pieces from to]
  true)

(defn move [pieces from to]
  (if (and (pieces from)
           to
           (valid-move? pieces from to))
    (let [moving (pieces from)]
      (assoc (dissoc pieces from) to moving))
   pieces
   ))
