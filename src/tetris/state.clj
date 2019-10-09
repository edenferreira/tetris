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