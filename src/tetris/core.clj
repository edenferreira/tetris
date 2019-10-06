(ns tetris.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.set :as set]))

;; TODOs
;; test the increase velocity and o better by it
;; create points
;; improve the game over
;; better setup, and test the keys of configuration

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
  (let [min-x (->> shape
                   (map :x)
                   (apply min))
        min-y (->> shape
                   (map :y)
                   (apply min))
        normalized-shape
        (->> shape
             (map (fn [s]
                    (update s :x
                            #(- % min-x))))
             (map (fn [s]
                    (update s :y
                            #(- % min-y)))))]
    (= (set normalized-shape)
       (set block-shape)) ))

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

(defn board-blocks [{:keys [board-height
                            board-width]}]
  (for [y (range -2 board-height)
        x (range board-width)]
    {:x x :y y}))

(defn draw-rects! [size positions]
  (doseq [[x y] positions]
    (q/rect (* x size)
            (* y size)
            size
            size)))

(defn can-move? [move-fn {:keys [piece] :as board}]
  (and (inside? board (move-fn piece))
       (not (collision? (move-fn piece)
                        (:filled-blocks board)))))

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

(defn completed-lines [filled-blocks]
  (->> filled-blocks
       (group-by :y)
       (map (juxt key
                  (comp
                    (partial apply +)
                    (partial map inc)
                    (partial map :x)
                    val)))
       (filter (comp (partial <= 55) second))
       (map first)
       not-empty))

(def states (atom {}))

(defn tick [{:keys [current-piece
                    next-pieces
                    piece-generator
                    current-frame
                    frame-rate
                    filled-blocks
                    flashing-before-merge
                    frames-before-flashing
                    current-flashing-frame
                    ticks-per-second]
             current-state :state
             :as state}]
  (let [next-frame (inc (mod current-frame
                             (int
                               (/ frame-rate
                                  ticks-per-second))))
        filled-blocks-with-piece (concat filled-blocks
                                         current-piece) ]
    (cond
      (= :just-merged-piece current-state)
      (-> state
          (assoc :current-piece (->> next-pieces
                                     first
                                     up
                                     (repeat-right 4)))
          (update :next-pieces (comp vec rest))
          (update :next-pieces conj (piece-generator))
          (assoc :state :ticking-away)
          (assoc :current-frame 1))

      (and (= current-state :ticking-away)
           (inside? (down current-piece)
                   (board-blocks state))
           (not (collision? (down current-piece)
                            filled-blocks))
           (= next-frame 1))
      (-> state
          (update :current-piece down)
          (assoc :current-frame next-frame))

      (and (= current-state :ticking-away)
           (inside? (down current-piece)
                    (board-blocks state))
           (not (collision? (down current-piece)
                            filled-blocks)))
      (-> state
          (assoc :current-frame next-frame))

      (and flashing-before-merge
           current-flashing-frame
           (< current-flashing-frame
              frames-before-flashing))
      (update state :current-flashing-frame inc)

      (and flashing-before-merge
           (odd? flashing-before-merge)
           (completed-lines filled-blocks-with-piece))
      (-> state
          (assoc :current-piece [])
          (update :filled-blocks
                  (partial
                    remove
                    (comp
                      (set (completed-lines filled-blocks-with-piece))
                      :y)))
          (assoc :current-flashing-frame 0)
          (update :flashing-before-merge inc))

      (and flashing-before-merge
           (even? flashing-before-merge)
           (< flashing-before-merge
              (* 2 (:flashes-before-merging state))))
      (-> state
          (assoc :current-piece [])
          (assoc :filled-blocks
                 (->> (for [x (range (:board-width state))
                            y (:merging-lines state)]
                        {:x x :y y})
                      (concat filled-blocks)))
          (assoc :current-flashing-frame 0)
          (update :flashing-before-merge inc))

      (and flashing-before-merge
           (even? flashing-before-merge))
      (-> state
          (assoc :state :just-merged)
          (assoc :current-piece [])
          (update :filled-blocks
                  (fn [filled-blocks]
                    (loop [filled-blocks filled-blocks
                           [merging & others]
                           (sort (:merging-lines state))]
                      (let [new-filled
                            (->> filled-blocks
                                 (map #(if (< (:y %)
                                              merging)
                                         (update % :y inc)
                                         %)))]
                        (if others
                          (recur new-filled others)
                          new-filled)))))
          (update :ticks-per-second inc)
          (dissoc :current-flashing-frame)
          (dissoc :merging-lines)
          (dissoc :flashing-before-merge))

      (completed-lines filled-blocks-with-piece)
      (-> state
          (assoc :state :flashing-for-merge)
          (assoc :merging-lines (completed-lines filled-blocks-with-piece))
          (assoc :current-piece [])
          (assoc :filled-blocks filled-blocks-with-piece)
          (assoc :current-flashing-frame 0)
          (assoc :flashing-before-merge 1))

      :else
      (-> state
          (assoc :current-piece [])
          (assoc :filled-blocks filled-blocks-with-piece)
          (assoc :state :just-merged-piece)
          (assoc :current-frame next-frame)))))

(defn key-pressed [{:keys [current-piece] :as state}
                  {:keys [key]}]
  (let [move-fn (case key
                  :right right
                  :left left
                  :down down
                  :up rotate
                  :nothing)]
    (if (and (not= :nothing move-fn)
             ;; test without current piece
             (inside? (move-fn current-piece)
                      (board-blocks state))
             (not (collision? (move-fn current-piece)
                              (:filled-blocks state))))
      (update state :current-piece move-fn)
      state)))

(def base-state
  {:state :ticking-away
   :filled-blocks []
   :board-height 24
   :board-width 10
   :board-x 5
   :board-y 5
   :flashes-before-merging 2
   :frames-before-flashing 3
   :current-piece []
   :next-pieces []
   :piece-generator random-piece
   :ticks-per-second 1
   :current-frame 1
   :frame-rate 60
   :size 15})

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsb)
  (let [state (-> base-state
                  (assoc :current-piece (up (repeat-right 4
                                                          (random-piece))))
                  (assoc :frames-before-flashing 6)
                  (assoc :ticks-per-second 2)
                  (assoc :size 20)
                  (assoc :next-pieces [(random-piece)
                                       (random-piece)
                                       (random-piece)
                                       (random-piece)]))]
    (reset! states [state])
    state))

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

  (some->> (:current-piece state)
           (map (juxt :x
                      :y))
           (remove (comp neg? second))
           (map (juxt (comp (partial +
                                     (:board-x state))
                            first)
                      (comp (partial +
                                     (:board-y state))
                            second)))
           (draw-rects! (:size state)))

  ;; draw filled blocks
  (some->> (:filled-blocks state)
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
    :size [500 800]
    :setup setup
    :update (fn [state]
              (let [new-state (tick state)]
                (swap! states conj new-state)
                new-state))
    :draw draw-state
    :features [:keep-on-top]
    :key-pressed key-pressed
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))
