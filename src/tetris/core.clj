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

(def l-shape [(o 0 1) (o 0 0) (o 0 2) (o 1 2)])
(def mirror-l-shape [(o 1 1) (o 1 0) (o 1 2) (o 0 2)])
(def t-shape [(o 1 1) (o 0 1) (o 1 0) (o 2 1)])
(def z-shape [(o 1 0) (o 0 0) (o 1 1) (o 2 1)])
(def s-shape [(o 1 1) (o 0 1) (o 1 0) (o 2 0)])
(def i-shape [(o 0 1) (o 0 0) (o 0 2) (o 0 3)])
(def block-shape [(o 0 0) (o 1 0) (o 1 1) (o 0 1)])

(defn is-block? [shape]
  (let [{:keys [x y]} (first shape)]
    (zero?
      (- (apply + (map (comp #(- % x) :x) shape))
         (apply + (map (comp #(- % y) :y) shape))))))


;;y  a mais vira x a menos
;;x a mais vira y a msi

(defn rotate [shape]
  (let [{pivot-x :x
         pivot-y :y} (first shape)]
    (if (is-block? shape)
      shape
      (map (fn [{:keys [x y]}]
             (o (- pivot-x
                   (- y
                      pivot-y))
                (+ pivot-y
                   (- x
                      pivot-x))
                ))
           shape))))

(def turn
  (partial map (fn [{:keys [x y]}]
                 (if (= x
                        (:x (nth l-shape 2)))
                   (o (:x (nth l-shape 2))
                      x)
                   (o y
                      (:y (nth l-shape 2)))))))

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

(def board
  {:height 24
   :width 10
   :x 5
   :y 5
   :piece []
   :next-piece []
   :filled-blocks []})

(defn board-blocks [{:keys [height width]}]
  (for [y (range height)
        x (range width)]
    {:x x :y y}))

(defn virtual-board-blocks [{:keys [height width]}]
  (for [y (range -5 height)
        x (range width)]
    {:x x :y y}))

(defn add-piece [board piece]
  (let [piece (loop [target-piece piece]
                (if (< 0 (->> target-piece
                              (map :y)
                              (apply max)))
                  (recur (up target-piece))
                  target-piece))]
    (assoc board
           :piece (-> piece
                      right right
                      right right))))

(defn update-board [{:keys [next-piece]
                     :as board}
                    move]
  (let [move-fn (case move
                  :down down
                  :right right
                  :left left
                  :up rotate)]
    (cond
      (and (= :down move)
           (or (collision? (move-fn (:piece board))
                           (:filled-blocks board))
               (not (inside? (move-fn (:piece board))
                             (virtual-board-blocks board)))))
      (-> board
          (update :filled-blocks concat (:piece board))
          (add-piece next-piece)
          (dissoc :next-piece))

      (or (collision? (move-fn (:piece board))
                      (:filled-blocks board))
          (not (inside? (move-fn (:piece board))
                        (virtual-board-blocks board))))
      board

      :else
      (update board :piece move-fn))))

(defn draw-one! [{:keys [x y]}
                 {board-x :x board-y :y}
                 size]
  (q/rect (* (+ board-x x) size)
          (* (+ board-y y) size)
          size
          size))

(defn draw-rects! [size positions]
  (doseq [[x y] positions]
    (q/rect (* x size)
            (* y size)
            size
            size)))

(defn draw-board! [board size]
  (run! (part-> draw-one!
                board
                size)
        (:piece board))
  (run! (part-> draw-one!
                board
                size)
        (:filled-blocks board))
  (run! (part-> draw-one!
                board
                size)
        (-> (:next-piece board)
            down
            right right right right right
            right right right right right
            right right right)))

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

(defn update-state [{:keys [frame
                            speed-x]
                     :as state}]
  (let [new-state
        (if (>= frame
                (- (target-frame speed-x) 1))
          (-> state
              (update :board
                      update-board :down)
              non-frame)
          (non-frame state))]
    (if (not (get-in new-state [:board :next-piece]))
      (assoc-in new-state [:board :next-piece]
                (random-piece))
      new-state)))

(defn call-times [f]
  (fn [n x]
    (loop [c 0
           v x]
      (if (< c n)
        (recur (inc c)
               (f v))
        v))))
(def repeat-down (call-times down))
(def repeat-right (call-times right))

(def base-state
  {:state :ticking-away
   :filled-blocks []
   :board-height 24
   :board-width 10
   :board-x 5
   :board-y 5
   :current-piece []
   :next-pieces [[]]
   :piece-generator random-piece
   :ticks-per-second 1
   :size 15})

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsb)
  (-> base-state
      (assoc :filled-blocks
             (for [x (range 8)
                   y (range 20 24)]
               {:x x :y y}))
      (assoc :current-piece (repeat-right 4 l-shape))
      (assoc :next-pieces [t-shape
                           block-shape
                           z-shape
                           s-shape])))

;; shapes should be a calculation, and not defined
(defn draw-state [state]

  (q/background 240)
  (q/fill 220 200 100)

  ;; draw board
  (->> (for [x (range (:board-width state))
             y (range (:board-height state))
             :let [x (+ x (:board-x state))
                   y (+ y (:board-y state))]]
         [x y])
       (draw-rects! (:size state)))

  (q/fill 220 200 240)

  ;; draw next pieces
  (-> (:next-pieces state)
      vec
      (update 0
              (comp (partial repeat-right 12)
                    (partial repeat-down 1)))
      (update 1
              (comp (partial repeat-right 12)
                    (partial repeat-down 6)))
      (update 2
              (comp (partial repeat-right 12)
                    (partial repeat-down 11)))
      (update 3
              (comp (partial repeat-right 12)
                    (partial repeat-down 16)))
      (->> (apply concat)
           (map (juxt (comp (partial +
                                     (:board-x state))
                            :x)
                      (comp (partial +
                                     (:board-y state))
                            :y)))
           (draw-rects! (:size state))))

  ;; draw current piece

  (->> (:current-piece state)
       (map (juxt (comp (partial +
                                 (:board-x state))
                        :x)
                  (comp (partial +
                                 (:board-y state))
                        :y)))
       (draw-rects! (:size state)))

  ;; draw filled blocks
  (->> (:filled-blocks state)
       (map (juxt (comp (partial +
                                 (:board-x state))
                        :x)
                  (comp (partial +
                                 (:board-y state))
                        :y)))
       (draw-rects! (:size state))))

(defn main []
  (q/defsketch tetris
    :title "You spin my circle right round"
    :size [500 500]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update identity
    :draw draw-state
    :features [:keep-on-top]
    #_#_
    :key-pressed
    (fn [{:keys [next-piece]
          :as state}
         {:keys [key]}]
      (if (#{:right :left :up :down} key)
        (let [new-state
              (update state
                      :board update-board key)]
          (doto
            (get new-state :board))
          (if (not (get-in new-state [:board :next-piece]))
            (assoc-in new-state [:board :next-piece]
                      (random-piece))
            new-state))
        state))
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))

(main)
