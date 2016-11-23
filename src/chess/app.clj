(ns chess.app
  (:require [quil.core :as q :include-macros true]
            [chess.uci :as uci]
            [quil.middleware :as m]))

(def UNIT 50)

(defn enumerate
  ([coll start]
   (map vector (iterate inc start) coll))

  ([coll]
   (enumerate coll 0)))

(def position
  "Initial position, will move when UIC module ready"
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

(def sub-img
  "Coordinates of pieces in texture"
  {:K [0 0 100 100]
   :Q [100 0 100 100]
   :B [200 0 100 100]
   :N [300 0 100 100]
   :R [400 0 100 100]
   :P [500 0 100 100]
   :k [0 100 100 100]
   :q [100 100 100 100]
   :b [200 100 100 100]
   :n [300 100 100 100]
   :r [400 100 100 100]
   :p [500 100 100 100] }
  )

(defn loc->coords [loc]
  "convert keyword to coords"
  (let [[l n] (name loc)
        letters "abcdefgh"
        l-mul (into {} (map vector letters (iterate inc 0)))
        n (dec (Integer/parseInt (str n)))]
      [(* UNIT (l-mul l)) (* UNIT (- 7 n))]
    )
  )

(defn coords->loc [x y]
  (let [l (int (/ x UNIT))
        n (- 7 (int (/ y UNIT)))]
    (keyword (str (nth "abcdefgh" l) (inc n))))
  )

(defn setup []
  (q/frame-rate 30)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:pieces-img (q/load-image "resources/pieces.png")
   :pieces position
   :from nil
   })

(defn in-board? [x y]
  (and (> x 0) (< x (* UNIT 8))
       (> y 0) (< y (* UNIT 8))))

(defn select-square [{:keys [from] :as state} x y]
  (if (in-board? x y)
    (let [loc (coords->loc x y)]
      (if (= from loc)
        (assoc state :from nil)
        (if (nil? from)
          (assoc state :from loc)
          (assoc state :to loc)
          )))
    state
    )
  )

(defn mouse-clicked [state {:keys [x y] :as event}]
  (let [offset-x (/ (- (q/width) (* UNIT 8)) 2)
        offset-y (/ (- (q/height) (* UNIT 8)) 2)]

    (case (event :button)
      :left (select-square state (- x offset-x) (- y offset-y))
      state
      ))
  )

(defn update-state [{:keys [from to pieces] :as state}]
  (if (and from to)
    (-> state
        (assoc :pieces (uci/move pieces from to))
        (assoc :from nil)
        (assoc :to nil))
    state
    ))

(defn -draw-numbers! []
  (doseq [n (range 8)]
    (q/text-size 18)
    (q/fill 0)
    (q/text (-> n inc str) (- (/ UNIT 2)) (+ (/ UNIT 2) (* UNIT (- 7 n))))
    (q/text (-> n inc str) (+ (* 8 UNIT) (- (/ UNIT 2) 10)) (+ (/ UNIT 2) (* UNIT (- 7 n))))
    ))

(defn -draw-letters! []
  (doseq [[i l] (enumerate "abcdefgh")]
    (q/text-size 18)
    (q/fill 0)
    (q/text (str l) (+ (/ UNIT 2) (* UNIT i)) (- 10 (/ UNIT 2)))
    (q/text (str l) (+ (/ UNIT 2) (* UNIT i)) (+ (/ UNIT 2) (* 8 UNIT)))
    ))

(defn draw-board! [state]
  (q/stroke 0)
  (q/fill 255)
  (q/rect -1 -1 (+ 1 (* UNIT 8)) (+ 1 (* UNIT 8)))
  (doseq [l (range 8)
          c (range 8)
          :let [color (if (even? (+ c l)) 255 100)]
          ]
    (q/no-stroke)
    (q/fill color)
    (q/rect (* c UNIT) (* l UNIT) UNIT UNIT))
  (-draw-numbers!)
  (-draw-letters!)
  ; highlight from
  (when (:from state)
    (let [[x y] (loc->coords (:from state))]
      (q/fill (q/color 0 255 255 127))
      (q/rect x y UNIT UNIT)))
   
  (when (:to state)
    (let [[x y] (loc->coords (:to state))]
      (q/fill (q/color 0 255 0 127))
      (q/rect x y UNIT UNIT)))
  )


(defn draw-state [{:keys [pieces-img pieces] :as state}]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 255)
  ; Set circle color.

  ; Calculate x and y coordinates of the circle.
  (q/with-translation [(/ (- (q/width) (* 8 UNIT)) 2)
                       (/ (- (q/height) (* 8 UNIT)) 2)]
    (draw-board! state)

    ; draw the pieces
    (q/image-mode :corner)
    (doseq [[pos piece] pieces
            :let [[x y] (loc->coords pos)
                  [ix iy w h] (sub-img piece)]]
      (q/image (q/get-pixel pieces-img ix iy w h) x y UNIT UNIT)))
  )

(defn init []
  (q/defsketch test-chess
    ; :host "chess"
    :size [500 500]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :mouse-clicked mouse-clicked
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))
