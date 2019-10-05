(ns tetris.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.set :as set]))

(def insert-first
  (comp
    (partial apply concat)
    (juxt (comp vector first)
          second)
    reverse
    vector))

(defn part-> [f & args]
  (fn [m]
    (apply f m args)))

(def o (comp (partial zipmap [:x :y]) vector))

(def up* (part-> update :y dec))
(def down* (part-> update :y inc))
(def right* (part-> update :x inc))
(def left* (part-> update :x dec))
(def up (partial map up*))
(def down (partial map down*))
(def right (partial map right*))
(def left (partial map left*))

(def l-shape [(o 0 0) (o 0 1) (o 0 2) (o 1 2)])
(def mirror-l-shape [(o 1 0) (o 1 1) (o 1 2) (o 0 2)])
(def t-shape [(o 0 1) (o 1 1) (o 1 0) (o 2 1)])
(def z-shape [(o 0 0) (o 1 0) (o 1 1) (o 2 1)])
(def s-shape [(o 0 1) (o 1 1) (o 1 0) (o 2 0)])
(def i-shape [(o 0 0) (o 0 1) (o 0 2) (o 0 3)])
(def block-shape [(o 0 0) (o 1 0) (o 1 1) (o 0 1)])
(defn random-piece []
  (rand-nth [l-shape
             mirror-l-shape
             t-shape
             z-shape
             s-shape
             i-shape
             block-shape]))

(defn collision? [positions1 positions2]
  (seq
    (set/intersection (set positions1)
                      (set positions2))))

(defn inside? [positions1 positions2]
  (= (set/intersection (set positions1)
                       (set positions2))
     (set positions1)))

(def y-inside?
  (comp (every-pred (comp (partial apply <)
                          (juxt (comp
                                  (partial apply max)
                                  (partial map :y)
                                  second)
                                (comp :height first)))
                    (comp (partial <= 0)
                          (partial apply min)
                          (partial map :y)
                          second))
        vector))

(def board
  {:height 24
   :width 10
   :x 5
   :y 5
   :piece []
   :filled-blocks []})

(defn board-blocks [{:keys [height width]}]
  (for [y (range height)
        x (range width)]
    {:x x :y y}))

(defn add-piece [board piece]
  (assoc board
         :piece (-> piece
                    right
                    right
                    right
                    right)))

(defn update-board [board move next-piece]
  (let [move-fn (case move
                  :down down
                  :right right
                  :left left)]
    (cond
      (and (= :down move)
           (or (collision? (move-fn (:piece board))
                           (:filled-blocks board))
               (not (inside? (move-fn (:piece board))
                             (board-blocks board)))))
      (-> board
          (update :filled-blocks concat (:piece board))
          (add-piece next-piece))

      (or (collision? (move-fn (:piece board))
                      (:filled-blocks board))
          (not (inside? (move-fn (:piece board))
                        (board-blocks board))))
      board

      :else
      (-> board
          (update :piece move-fn)))))

(defn draw-one! [{:keys [x y]}
                 {board-x :x board-y :y}
                 size]
  (q/rect (* (+ board-x x) size)
          (* (+ board-y y) size)
          size
          size))

(defn draw-piece! [board size]
  (run! (part-> draw-one!
                board
                size)
        (:piece board))
  (run! (part-> draw-one!
                board
                size)
        (:filled-blocks board)))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 60)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:board (add-piece board
                     l-shape)
   :speed-x 6
   :frame 0})

(defn target-frame [speed-x]
  (-> 60 (/ speed-x) Math/abs))

(defn non-frame [{:keys [speed-x] :as state}]
  (update state :frame (comp (part-> mod
                                     (target-frame speed-x))
                             inc)))

(defn can-move? [move-fn {:keys [piece] :as board}]
  (and (inside? board (move-fn piece))
       (not (collision? (move-fn piece)
                        (:filled-blocks board)))))

(defn keep-moving? [move-fn {:keys [piece] :as board}]
  (and (y-inside? board (move-fn piece))
       (not (collision? (move-fn piece)
                        (:filled-blocks board)))))

(defn move-piece [move-fn
                  {{:keys [piece] :as board}
                   :board :as state}]
  (cond
    (can-move? move-fn board)
    (-> state
        (update-in [:board :piece] move-fn)
        non-frame)

    (keep-moving? move-fn board)
    (-> state
        non-frame)

    :else
    (-> state
        (update-in [:board :filled-blocks] concat piece)
        (update :board add-piece l-shape)
        non-frame)))

(defn update-state [{:keys [frame speed-x]
                     :as state}]
  (if (>= frame
          (- (target-frame speed-x) 1))
    (-> state
        (update :board
          update-board :down (random-piece))
        non-frame)
    (non-frame state)))

(defn draw-state [state]
  (let [board (:board state)]
    (q/background 240)
    (q/fill 220 200 100)
    (doseq [y (range (:height board))
            x (range (:width board))
            :let [x (+ (:x board)
                       x)
                  y (+ (:y board)
                       y)]]
      (q/rect (* 15 x)
              (* 15 y)
              15
              15))
    (q/fill 220 200 255)
    (draw-piece! board 15)))

(defn main []
  (q/defsketch tetris
    :title "You spin my circle right round"
    :size [500 500]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    :key-pressed
    (fn [state {:keys [key]}]
      (if (#{:right :left :down} key)
        (update state
                :board
                update-board key (random-piece))
        state))
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))

(main)
