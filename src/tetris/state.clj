(ns tetris.state
  (:require [clojure.set :as set]))

(defn board-blocks [{board-height :tetris.board/height
                     board-width :tetris.board/width}]
  (for [y (range -2 board-height)
        x (range board-width)]
    {:x x :y y}))

(defn piece-inside-board-after-move? [move-fn
                                      {:tetris.board/keys [width
                                                           height
                                                           current-piece]}]
  (let [after-move (move-fn current-piece)
        max-y (->> after-move (map :y) (apply max))
        min-x (->> after-move (map :x) (apply min))
        max-x (->> after-move (map :x) (apply max))]
    (and (< max-y height)
         (< max-x width)
         (<= 0 min-x))))

(defn collision-after-move? [move-fn state]
  (seq
    (set/intersection (set (move-fn (:tetris.board/current-piece state)))
                      (set (:tetris.board/filled-blocks state)))))

(defn just-merge-piece? [state]
  (= :just-merged-piece
     (:tetris.execution/stage state)))

(defn ticking-away? [state]
  (= :ticking-away
     (:tetris.execution/stage state)))

(defn preparing-to-merged-piece? [state]
  (= :preparing-to-merged-piece
     (:tetris.execution/stage state)))

(defn game-over? [state]
  (= :game-over
     (:tetris.execution/stage state)))

(defn continue-blinking?
  [{current-flashing-for-merge-frame :tetris.execution.frames/flashing-for-merge
    blinking-frames :tetris.definition/blinking-frames
    current-blinking-frame :tetris.execution.frames/blinking}]
  (and current-flashing-for-merge-frame
       current-blinking-frame
       (< current-blinking-frame
          blinking-frames)))

(defn merge-piece?
  [{:tetris.execution.frames/keys [before-merge]
    :tetris.definition/keys [frames-before-merge]
    :or {before-merge 0}}]
  (<= frames-before-merge
      before-merge))

(defn frame-for-flash-off? [state]
  (some-> state :tetris.execution.frames/flashing-for-merge odd?))

(defn frame-for-flash-on? [state]
  (some-> state :tetris.execution.frames/flashing-for-merge even?))

(defn merge-frame? [state]
  (some-> state
          :tetris.execution.frames/flashing-for-merge
          (>= (* 2 (:flashes-before-merging state)))))

(defn completed-lines [{:tetris.board/keys [filled-blocks
                                            current-piece]}]
  (->> current-piece
       (concat filled-blocks)
       (group-by :y)
       (map (juxt key
                  (comp
                    (partial apply +)
                    (partial map inc)
                    (partial map :x)
                    val)))
       (filter (comp (partial <= 55) second))
       (map first)))

(defn completed-lines? [state]
  (not-empty (completed-lines state)))

(defn next-frame [{frame-rate :tetris.definition/frame-rate
                   current-frame :tetris.execution.frames/tick
                   ticks-per-second :tetris.definition/ticks-per-second}]
  (inc (mod current-frame
            (int
              (/ frame-rate
                 ticks-per-second)))))

(defn down-piece-frame?
  [state]
  (= 1
     (next-frame state)))

(defn putting-piece-just-outside-board [piece]
  (let [max-y (->> piece (map :y) (apply max))
        diff (- 0 1 max-y)]
    (map #(update % :y + diff) piece)))

(defn completed-columns? [{:tetris.board/keys [filled-blocks
                                              current-piece]}]
  (->> filled-blocks
       (map :y)
       distinct
       (mapcat #(map vector
                     (range 24)
                     (repeat %)))
       (map (fn [[x y]] {:x x :y y}))
       (concat current-piece)
       (group-by :x)
       (map (juxt key
                  (comp
                    (partial apply +)
                    (partial remove neg?)
                    (partial map inc)
                    (partial map :y)
                    val)))
       (filter (comp (partial <= 300) second))
       (map first)
       seq))
