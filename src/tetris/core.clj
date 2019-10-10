(ns tetris.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tetris.state]
            [events]
            [clojure.set :as set]))

;; TODOs
;; test the increase velocity and o better by it
;; create points
;; improve the game over
;; better setup, and test the keys of configuration
;; only permit moving the piece after it got in the board
;; document tests

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
       (set block-shape))))

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

(defn draw-rects! [size positions]
  (doseq [[x y] positions]
    (q/rect (* x size)
            (* y size)
            size
            size)))

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

(defn putting-piece-just-outside-board [piece]
  (let [max-y (->> piece (map :y) (apply max))
        diff (- 0 1 max-y)]
    (map #(update % :y + diff) piece)))

(def states (atom {}))

(defn tick [{current-piece :tetris.board/current-piece
             next-pieces :tetris.board/next-pieces
             piece-generator :tetris.generators/piece
             current-frame :tetris.execution.frames/tick
             frame-rate :tetris.definition/frame-rate
             filled-blocks :tetris.board/filled-blocks
             current-flashing-for-merge-frame :tetris.execution.frames/flashing-for-merge
             blinking-frames :tetris.definition/blinking-frames
             current-blinking-frame :tetris.execution.frames/blinking
             ticks-per-second :tetris.definition/ticks-per-second
             current-state :tetris.execution/stage
             :as state}]
  (cond
    (tetris.state/completed-columns? state)
    (-> state
        (assoc :tetris.execution/stage :game-over))

    (tetris.state/just-merge-piece? state)
    (-> state
        (assoc :tetris.board/current-piece (->> next-pieces
                                                first
                                                putting-piece-just-outside-board
                                                (repeat-right 4)))
        (update :tetris.board/next-pieces (comp vec rest))
        (update :tetris.board/next-pieces conj (piece-generator))
        (assoc :tetris.execution/stage :ticking-away)
        (assoc :tetris.execution.frames/tick 1))

    (and (tetris.state/ticking-away? state)
         (tetris.state/piece-inside-board-after-move? down state)
         (not (tetris.state/collision-after-move? down state))
         (tetris.state/down-piece-frame? state))
    (-> state
        (update :tetris.board/current-piece down)
        (assoc :tetris.execution.frames/tick (tetris.state/next-frame state)))

    (and (tetris.state/ticking-away? state)
         (tetris.state/piece-inside-board-after-move? down state)
         (not (tetris.state/collision-after-move? down state)))
    (-> state
        (assoc :tetris.execution.frames/tick (tetris.state/next-frame state)))

    (tetris.state/continue-blinking? state)
    (update state :tetris.execution.frames/blinking inc)

    (and (tetris.state/frame-for-flash-off? state)
         (tetris.state/completed-lines? state))
    (-> state
        (assoc :tetris.board/current-piece [])
        (update :tetris.board/filled-blocks
                (partial
                  remove
                  (comp
                    (set (tetris.state/completed-lines state))
                    :y)))
        (assoc :tetris.execution.frames/blinking 0)
        (update :tetris.execution.frames/flashing-for-merge inc))

    (and (tetris.state/frame-for-flash-on? state)
         (not (tetris.state/merge-frame? state)))
    (-> state
        (assoc :tetris.board/current-piece [])
        (assoc :tetris.board/filled-blocks
          (->> (for [x (range (:tetris.board/width state))
                     y (:merging-lines state)]
                 {:x x :y y})
               (concat filled-blocks)))
        (assoc :tetris.execution.frames/blinking 0)
        (update :tetris.execution.frames/flashing-for-merge inc))

    (and (tetris.state/frame-for-flash-on? state)
         (tetris.state/merge-frame? state))
    (-> state
        (assoc :tetris.execution/stage :just-merged)
        (assoc :tetris.board/current-piece [])
        (update :tetris.board/filled-blocks
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
        (update :tetris.definition/ticks-per-second inc)
        (dissoc :tetris.execution.frames/blinking)
        (dissoc :merging-lines)
        (dissoc :tetris.execution.frames/flashing-for-merge))

    (tetris.state/completed-lines? state)
    (-> state
        (assoc :tetris.execution/stage :flashing-for-merge)
        (assoc :merging-lines (tetris.state/completed-lines state))
        (assoc :tetris.board/current-piece [])
        (assoc :tetris.board/filled-blocks (concat filled-blocks current-piece))
        (assoc :tetris.execution.frames/blinking 0)
        (assoc :tetris.execution.frames/flashing-for-merge 1))

    (and (tetris.state/merge-piece? state)
         (tetris.state/preparing-to-merged-piece? state))
    (-> state
        (assoc :tetris.board/current-piece [])
        (assoc :tetris.board/filled-blocks (concat filled-blocks current-piece))
        (assoc :tetris.execution/stage :just-merged-piece)
        (dissoc :tetris.execution.frames/before-merge)
        (assoc :tetris.execution.frames/tick (tetris.state/next-frame state)))

    (and (not (tetris.state/merge-piece? state))
         (tetris.state/preparing-to-merged-piece? state))
    (-> state
        (update :tetris.execution.frames/before-merge inc))

    :else
    (-> state
        (assoc :tetris.execution/stage :preparing-to-merged-piece)
        (assoc :tetris.execution.frames/before-merge 0))))

(defn key-pressed [{current-piece :tetris.board/current-piece
                    :as state}
                   {:keys [key]}]
  (let [move-fn (case key
                  :right right
                  :left left
                  :down down
                  :up rotate
                  :nothing)]
    (if (and (not= :nothing move-fn)
             ;; test without current piece
             (seq current-piece)
             (tetris.state/piece-inside-board-after-move? move-fn state)
             (not (tetris.state/collision-after-move? move-fn state)))
      (update state :tetris.board/current-piece move-fn)
      state)))

(def base-state
  {:tetris.execution/stage :ticking-away
   :tetris.board/filled-blocks []
   :tetris.board/height 24
   :tetris.board/width 10
   :board-x 5
   :board-y 5
   :flashes-before-merging 2
   :tetris.definition/blinking-frames 3
   :tetris.board/current-piece []
   :tetris.board/next-pieces []
   :tetris.generators/piece random-piece
   :tetris.definition/ticks-per-second 1
   :tetris.execution.frames/tick 1
   :tetris.definition/frames-before-merge 3
   :tetris.definition/frame-rate 60
   :tetris.definition/size 15})

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsb)
  (let [state (-> base-state
                  (assoc :tetris.board/current-piece
                    (putting-piece-just-outside-board
                      (repeat-right 4
                                    (random-piece))))
                  (assoc :tetris.definition/blinking-frames 6)
                  (assoc :tetris.definition/ticks-per-second 2)
                  (assoc :tetris.definition/size 20)
                  (assoc :tetris.definition/frames-before-merge 15)
                  (assoc :tetris.board/next-pieces [(random-piece)
                                                    (random-piece)
                                                    (random-piece)
                                                    (random-piece)]))]
    (reset! states [state])
    state))

;; shapes should be a calculation, and not defined
(defn draw-state [state]

  (q/background 240)

  (if (tetris.state/game-over? state)
    (q/fill 120 200 250)
    (q/fill 220 200 100))

  ;; draw board
  (->> (for [x (range (:tetris.board/width state))
             y (range (:tetris.board/height state))
             :let [x (+ x (:board-x state))
                   y (+ y (:board-y state))]]
         [x y])
       (draw-rects! (:tetris.definition/size state)))


  (if (tetris.state/game-over? state)
    (q/fill 120 100 250)
    (q/fill 220 200 240))

  ;; draw next pieces
  (-> (:tetris.board/next-pieces state)
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
           (draw-rects! (:tetris.definition/size state))))

  ;; draw current piece

  (some->> (:tetris.board/current-piece state)
           (map (juxt :x
                      :y))
           (remove (comp neg? second))
           (map (juxt (comp (partial +
                                     (:board-x state))
                            first)
                      (comp (partial +
                                     (:board-y state))
                            second)))
           not-empty
           (draw-rects! (:tetris.definition/size state)))

  ;; draw filled blocks
  (some->> (:tetris.board/filled-blocks state)
           (map (juxt (comp (partial +
                                     (:board-x state))
                            :x)
                      (comp (partial +
                                     (:board-y state))
                            :y)))
           (draw-rects! (:tetris.definition/size state))))

(defn main []
  (q/defsketch tetris
               :title "You spin my circle right round"
               :size [500 800]
               :setup (fn []
                        (events/start! {} (setup)))
               :update (fn [state]
                         (events/new! :event.type/tick
                                      {}
                                      (tick state)))
               :draw draw-state
               :features [:keep-on-top]
               :key-pressed (fn [state context]
                              (events/new! :event.type/key-pressed
                                           context
                                           (key-pressed state context)))
               ; This sketch uses functional-mode middleware.
               ; Check quil wiki for more info about middlewares and particularly
               ; fun-mode.
               :middleware [m/fun-mode]))
