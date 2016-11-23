(ns chess.app
  (:require [quil.core :as q :include-macros true]
            [chess.gui :as gui]
            [quil.middleware :as m]))

(def position
  "Initial position, will move when UCI module ready"
  {
   :a1 :R
   :b1 :N
   :c1 :B
   :d1 :Q
   :e1 :K
   :f1 :B
   :g1 :N
   :h1 :R
   :a2 :P
   :b2 :P
   :c2 :P
   :d2 :P
   :e2 :P
   :f2 :P
   :g2 :P
   :h2 :P

   :a8 :r
   :b8 :n
   :c8 :b
   :d8 :q
   :e8 :k
   :f8 :b
   :g8 :n
   :h8 :r
   :a7 :p
   :b7 :p
   :c7 :p
   :d7 :p
   :e7 :p
   :f7 :p
   :g7 :p
   :h7 :p
   })

(defn setup []
  (q/frame-rate 30)
  {:pieces-img (q/load-image "resources/pieces.png")
   :pieces position
   :from nil
   })

(defn init []
  (q/defsketch chess
    :size [500 500]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update gui/update-state
    :draw gui/draw-state
    :mouse-clicked gui/mouse-clicked
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))
