(ns tetris.state
  (:require [clojure.set :as set]))

(defn board-blocks [{:keys [board-height
                            board-width]}]
  (for [y (range -2 board-height)
        x (range board-width)]
    {:x x :y y}))

(defn piece-inside-board-after-move? [move-fn state]
  (= (set/intersection (set (move-fn (:tetris.board/current-piece state)))
                       (set (board-blocks state)))
     (set (move-fn (:tetris.board/current-piece state)))) )
