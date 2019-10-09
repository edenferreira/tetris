(ns tetris.state
  (:require [clojure.set :as set]))

(defn board-blocks [{board-height :tetris.board/height
                     board-width :tetris.board/width}]
  (for [y (range -2 board-height)
        x (range board-width)]
    {:x x :y y}))

(defn piece-inside-board-after-move? [move-fn state]
  (= (set/intersection (set (move-fn (:tetris.board/current-piece state)))
                       (set (board-blocks state)))
     (set (move-fn (:tetris.board/current-piece state)))))

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

(defn continue-blinking?
  [{current-flashing-for-merge-frame :tetris.execution.frames/flashing-for-merge
    blinking-frames :tetris.definition/blinking-frames
    current-blinking-frame :tetris.execution.frames/blinking}]
  (and current-flashing-for-merge-frame
       current-blinking-frame
       (< current-blinking-frame
          blinking-frames)))

(defn frame-for-flash-off? [state]
  (some-> state :tetris.execution.frames/flashing-for-merge odd?))

(defn frame-for-flash-on? [state]
  (some-> state :tetris.execution.frames/flashing-for-merge even?))

(defn merge-frame? [state]
  (some-> state
          :tetris.execution.frames/flashing-for-merge
          (>= (* 2 (:flashes-before-merging state)))))

(defn completed-lines? [{:tetris.board/keys [filled-blocks
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
       (map first)
       not-empty))

(defn down-piece-frame?
  [{frame-rate :tetris.definition/frame-rate
    current-frame :tetris.execution.frames/tick
    ticks-per-second :tetris.definition/ticks-per-second}]
  (= 1
     (inc (mod current-frame
                             (int
                               (/ frame-rate
                                  ticks-per-second))))))