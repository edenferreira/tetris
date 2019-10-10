(ns tetris.core-test
  (:require [clojure.test :refer :all]
            [tetris.core :as tetris :refer [o]]))

(deftest down
  (is (= [{:x 0 :y 1} {:x 0 :y 2} {:x 0 :y 3} {:x 1 :y 3}]
         (tetris/down
           [{:x 0 :y 0} {:x 0 :y 1} {:x 0 :y 2} {:x 1 :y 2}])))
  (is (= [{:x 2 :y 4} {:x 3 :y 4} {:x 3 :y 5} {:x 4 :y 5}]
         (tetris/down
           [{:x 2 :y 3} {:x 3 :y 3} {:x 3 :y 4} {:x 4 :y 4}]))))

(deftest right
  (is (= [{:x 1 :y 0} {:x 1 :y 1} {:x 1 :y 2} {:x 2 :y 2}]
         (tetris/right
           [{:x 0 :y 0} {:x 0 :y 1} {:x 0 :y 2} {:x 1 :y 2}])))
  (is (= [{:x 3 :y 3} {:x 4 :y 3} {:x 4 :y 4} {:x 5 :y 4}]
         (tetris/right
           [{:x 2 :y 3} {:x 3 :y 3} {:x 3 :y 4} {:x 4 :y 4}]))))

(deftest left
  (is (= [{:x -1 :y 0} {:x -1 :y 1} {:x -1 :y 2} {:x 0 :y 2}]
         (tetris/left
           [{:x 0 :y 0} {:x 0 :y 1} {:x 0 :y 2} {:x 1 :y 2}])))
  (is (= [{:x 1 :y 3} {:x 2 :y 3} {:x 2 :y 4} {:x 3 :y 4}]
         (tetris/left
           [{:x 2 :y 3} {:x 3 :y 3} {:x 3 :y 4} {:x 4 :y 4}]))))

(deftest collision?
  (is (tetris/collision?
        [{:x 2 :y 3} {:x 3 :y 3}]
        [{:x 2 :y 3} {:x 3 :y 3} {:x 2 :y 1}]))
  (is (not
        (tetris/collision?
          [{:x 2 :y 2} {:x 2 :y 4}]
          [{:x 2 :y 3} {:x 3 :y 3} {:x 2 :y 1}])))
  (is (tetris/collision?
        [{:x 2 :y 2} {:x 2 :y 4} {:x 2 :y 1} {:x 2 :y 1}]
        [{:x 2 :y 1}]))
  (is (not
        (tetris/collision?
          []
          [{:x 2 :y 3} {:x 3 :y 3} {:x 2 :y 1}]))))

(deftest inside?
  (is (tetris/inside?
        [{:x 2 :y 1} {:x 2 :y 2}]
        [{:x 2 :y 2} {:x 2 :y 4} {:x 2 :y 1} {:x 2 :y 1}]))
  (is (not
        (tetris/inside?
          [{:x 2 :y 1} {:x 2 :y 3}]
          [{:x 2 :y 2} {:x 2 :y 4} {:x 2 :y 1} {:x 2 :y 1}]))))

(def base-state
  (assoc tetris/base-state
         :tetris.generators/piece identity) )

(deftest tick
  (is (= (assoc base-state
                :tetris.execution.frames/tick 1
                :tetris.board/current-piece [{:x 0 :y 2}
                                             {:x 0 :y 1}
                                             {:x 0 :y 3}
                                             {:x 1 :y 3}])
         (tetris/tick
           (assoc base-state
                  :tetris.execution.frames/tick 60
                  :tetris.board/current-piece [{:x 0 :y 1}
                                               {:x 0 :y 0}
                                               {:x 0 :y 2}
                                               {:x 1 :y 2}]))))

  (is (= (assoc base-state
                :tetris.execution.frames/tick 60
                :tetris.board/current-piece [{:x 0 :y 1}
                                             {:x 0 :y 0}
                                             {:x 0 :y 2}
                                             {:x 1 :y 2}])
         (tetris/tick
           (assoc base-state
                  :tetris.execution.frames/tick 59
                  :tetris.board/current-piece [{:x 0 :y 1}
                                               {:x 0 :y 0}
                                               {:x 0 :y 2}
                                               {:x 1 :y 2}]))))

  (is (= (assoc base-state
                :tetris.execution/stage :preparing-to-merged-piece
                :tetris.execution.frames/before-merge 0
                :tetris.board/filled-blocks (for [x (range 1 9)
                                                  y (range 22 24)]
                                                {:x x :y y})
                  :tetris.board/current-piece [{:x 9 :y 22}
                                               {:x 9 :y 23}])
         (tetris/tick
           (assoc base-state
                  :tetris.execution/stage :ticking-away
                  :tetris.definition/frames-before-merge 3
                  :tetris.board/filled-blocks (for [x (range 1 9)
                                                    y (range 22 24)]
                                                {:x x :y y})
                  :tetris.board/current-piece [{:x 9 :y 22}
                                               {:x 9 :y 23}]))))

  (is (= (assoc base-state
                :tetris.execution/stage :game-over
                :tetris.board/filled-blocks (for [x (range 3 6)
                                                    y (range 1 24)]
                                                {:x x :y y})
                :tetris.board/current-piece [{:x 3 :y -2}
                                             {:x 3 :y -1}
                                             {:x 3 :y 0}])
         (tetris/tick
           (assoc base-state
                  :tetris.execution/stage :ticking-away
                  :tetris.board/filled-blocks (for [x (range 3 6)
                                                    y (range 1 24)]
                                                {:x x :y y})
                  :tetris.board/current-piece [{:x 3 :y -2}
                                               {:x 3 :y -1}
                                               {:x 3 :y 0}]))))

  (is (= (assoc base-state
                :tetris.execution/stage :game-over
                :tetris.board/filled-blocks (concat
                                              (for [x (range 3 6)
                                                    y (range 3 24)]
                                                {:x x :y y})
                                              (for [x (range 1 4)
                                                    y (range 1 3)]
                                                {:x x :y y}))
                :tetris.board/current-piece [{:x 1 :y -2}
                                             {:x 1 :y -1}
                                             {:x 1 :y 0}])
         (tetris/tick
           (assoc base-state
                  :tetris.execution/stage :ticking-away
                :tetris.board/filled-blocks (concat
                                              (for [x (range 3 6)
                                                    y (range 3 24)]
                                                {:x x :y y})
                                              (for [x (range 1 4)
                                                    y (range 1 3)]
                                                {:x x :y y}))
                :tetris.board/current-piece [{:x 1 :y -2}
                                             {:x 1 :y -1}
                                             {:x 1 :y 0}]))))

  (is (= (assoc base-state
                :tetris.execution/stage :just-merged-piece
                :tetris.execution.frames/tick 2
                :tetris.board/filled-blocks (for [x (range 1 10)
                                                  y (range 22 24)]
                                              {:x x :y y}))
         (tetris/tick
           (assoc base-state
                  :tetris.execution/stage :preparing-to-merged-piece
                  :tetris.definition/frames-before-merge 3
                  :tetris.execution.frames/before-merge 3
                  :tetris.board/filled-blocks (for [x (range 1 9)
                                                    y (range 22 24)]
                                                {:x x :y y})
                  :tetris.board/current-piece [{:x 9 :y 22}
                                               {:x 9 :y 23}]))))

  (testing "flashing when merging"
    (is (= (assoc base-state
                  :tetris.execution/stage :flashing-for-merge
                  :tetris.execution.frames/flashing-for-merge 1
                  :tetris.execution.frames/blinking 0
                  :tetris.board/filled-blocks (for [x (range 10)
                                                    y (range 22 24)]
                                                {:x x :y y})
                  :merging-lines [22 23])
           (tetris/tick
             (assoc base-state
                    :tetris.execution/stage :ticking-away
                    :tetris.board/filled-blocks (for [x (range 9)
                                                      y (range 22 24)]
                                                  {:x x :y y})
                    :tetris.board/current-piece [{:x 9 :y 22}
                                                 {:x 9 :y 23}]))))

    (is (= (assoc base-state
                  :tetris.execution/stage :flashing-for-merge
                  :tetris.execution.frames/flashing-for-merge 2
                  :tetris.execution.frames/blinking 0
                  :tetris.board/filled-blocks []
                  :merging-lines [22 23])
           (tetris/tick
             (assoc base-state
                    :tetris.execution/stage :flashing-for-merge
                    :tetris.execution.frames/flashing-for-merge 1
                    :tetris.execution.frames/blinking 3
                    :tetris.board/filled-blocks (for [x (range 10)
                                                      y (range 22 24)]
                                                  {:x x :y y})
                    :merging-lines [22 23]))))

    (is (= (assoc base-state
                  :tetris.execution/stage :flashing-for-merge
                  :tetris.execution.frames/flashing-for-merge 3
                  :tetris.execution.frames/blinking 0
                  :tetris.board/filled-blocks
                  (concat (for [x (range 9)
                                y (range 20 22)]
                            {:x x :y y})
                          (for [x (range 10)
                                y (range 22 24)]
                            {:x x :y y}))
                  :merging-lines [22 23])
           (tetris/tick
             (assoc base-state
                    :tetris.execution/stage :flashing-for-merge
                    :tetris.execution.frames/flashing-for-merge 2
                    :tetris.execution.frames/blinking 3
                    :tetris.board/filled-blocks (for [x (range 9)
                                                      y (range 20 22)]
                                                  {:x x :y y})
                    :merging-lines [22 23]))))

    (is (= (assoc base-state
                  :tetris.execution/stage :flashing-for-merge
                  :tetris.execution.frames/flashing-for-merge 4
                  :tetris.execution.frames/blinking 0
                  :tetris.board/filled-blocks
                  (for [x (range 9)
                        y (range 20 22)]
                    {:x x :y y})
                  :merging-lines [22 23])
           (tetris/tick
             (assoc base-state
                    :tetris.execution/stage :flashing-for-merge
                    :tetris.execution.frames/flashing-for-merge 3
                    :tetris.execution.frames/blinking 3
                    :tetris.board/filled-blocks
                    (concat (for [x (range 9)
                                  y (range 20 22)]
                              {:x x :y y})
                            (for [x (range 10)
                                  y (range 22 24)]
                              {:x x :y y}))
                    :merging-lines [22 23]))))

    (is (= (assoc base-state
                  :tetris.execution/stage :flashing-for-merge
                  :tetris.execution.frames/flashing-for-merge 3
                  :tetris.execution.frames/blinking 1
                  :tetris.board/filled-blocks
                  (concat (for [x (range 9)
                                y (range 20 22)]
                            {:x x :y y})
                          (for [x (range 10)
                                y (range 22 24)]
                            {:x x :y y}))
                  :merging-lines [22 23])
           (tetris/tick
             (assoc base-state
                    :tetris.execution/stage :flashing-for-merge
                    :tetris.execution.frames/flashing-for-merge 3
                    :tetris.execution.frames/blinking 0
                    :tetris.board/filled-blocks
                    (concat (for [x (range 9)
                                  y (range 20 22)]
                              {:x x :y y})
                            (for [x (range 10)
                                  y (range 22 24)]
                              {:x x :y y}))
                    :merging-lines [22 23])))))

  (is (= (assoc base-state
                :tetris.execution/stage :just-merged
                :tetris.board/filled-blocks (for [x (range 9)
                                                  y (range 22 24)]
                                              {:x x :y y})
                :tetris.definition/ticks-per-second 2)
         (tetris/tick
           (assoc base-state
                  :tetris.execution/stage :flashing-for-merge
                  :tetris.execution.frames/flashing-for-merge 4
                  :tetris.execution.frames/blinking 3
                  :tetris.board/filled-blocks
                  (for [x (range 9)
                        y (range 20 22)]
                    {:x x :y y})
                  :merging-lines [22 23]))))

  (let [piece-generator (constantly
                          [{:x 0 :y 1}
                           {:x 0 :y 0}])]
    (is (= (assoc base-state
                  :tetris.execution.frames/tick 1
                  :tetris.board/filled-blocks [{:x 0 :y 22}
                                               {:x 0 :y 21}
                                               {:x 0 :y 23}
                                               {:x 1 :y 23}]
                  :tetris.board/current-piece [{:x 4 :y -2}
                                               {:x 4 :y -1}]
                  :tetris.board/next-pieces [[{:x 0 :y 1}
                                              {:x 0 :y 0}]]
                  :tetris.generators/piece piece-generator)
           (tetris/tick
             (assoc base-state
                    :tetris.execution/stage :just-merged-piece
                    :tetris.execution.frames/tick 50
                    :tetris.board/filled-blocks [{:x 0 :y 22}
                                                 {:x 0 :y 21}
                                                 {:x 0 :y 23}
                                                 {:x 1 :y 23}]
                    :tetris.board/next-pieces [[{:x 0 :y 1}
                                                {:x 0 :y 2}]]
                    :tetris.generators/piece piece-generator))))))

(deftest keypressed
  (is (= (assoc base-state
                :tetris.board/filled-blocks []
                :tetris.board/current-piece [{:x 4 :y 1}
                                             {:x 4 :y 2}])
         (tetris/key-pressed
           (assoc base-state
                  :tetris.board/filled-blocks []
                  :tetris.board/current-piece [{:x 4 :y 0}
                                               {:x 4 :y 1}])
           {:key :down})))

  (is (= (assoc base-state
                :tetris.board/filled-blocks []
                :tetris.board/current-piece [{:x 3 :y 0}
                                             {:x 3 :y 1}])
         (tetris/key-pressed
           (assoc base-state
                  :tetris.board/filled-blocks []
                  :tetris.board/current-piece [{:x 4 :y 0}
                                               {:x 4 :y 1}])
           {:key :left})))

  (is (= (assoc base-state
                :tetris.board/filled-blocks []
                :tetris.board/current-piece [{:x 5 :y 0}
                                             {:x 5 :y 1}])
         (tetris/key-pressed
           (assoc base-state
                  :tetris.board/filled-blocks []
                  :tetris.board/current-piece [{:x 4 :y 0}
                                               {:x 4 :y 1}])
           {:key :right})))

  (is (= (assoc base-state
                :tetris.board/filled-blocks []
                :tetris.board/current-piece [{:x 5 :y -1}
                                             {:x 5 :y 0}])
         (tetris/key-pressed
           (assoc base-state
                  :tetris.board/filled-blocks []
                  :tetris.board/current-piece [{:x 4 :y -1}
                                               {:x 4 :y 0}])
           {:key :right})))

  (is (= (assoc base-state
                :tetris.board/filled-blocks []
                :tetris.board/current-piece [{:x 4 :y 0}
                                             {:x 3 :y 0} ])
         (tetris/key-pressed
           (assoc base-state
                  :tetris.board/filled-blocks []
                  :tetris.board/current-piece [{:x 4 :y 0}
                                               {:x 4 :y 1}])
           {:key :up})))

  (is (= (assoc base-state
                :tetris.board/filled-blocks []
                :tetris.board/current-piece [{:x 9 :y 0}
                                             {:x 9 :y 1}])
         (tetris/key-pressed
           (assoc base-state
                  :tetris.board/filled-blocks []
                  :tetris.board/current-piece [{:x 9 :y 0}
                                               {:x 9 :y 1}])
           {:key :right})))

  (is (= (assoc base-state
                :tetris.board/filled-blocks [{:x 8 :y 0}
                                             {:x 8 :y 1}]
                :tetris.board/current-piece [{:x 7 :y 0}
                                             {:x 7 :y 1}])
         (tetris/key-pressed
           (assoc base-state
                  :tetris.board/filled-blocks [{:x 8 :y 0}
                                               {:x 8 :y 1}]
                  :tetris.board/current-piece [{:x 7 :y 0}
                                               {:x 7 :y 1}])
           {:key :right})))

  (is (= (assoc base-state
                :tetris.execution/stage :preparing-to-merged-piece
                :tetris.execution.frames/before-merge 0
                :tetris.board/filled-blocks (for [x (range 1 5)
                                                  y (range 22 24)]
                                                {:x x :y y})
                  :tetris.board/current-piece [{:x 8 :y 22}
                                               {:x 8 :y 23}])
         (tetris/key-pressed
           (assoc base-state
                :tetris.execution/stage :preparing-to-merged-piece
                :tetris.execution.frames/before-merge 0
                :tetris.board/filled-blocks (for [x (range 1 5)
                                                  y (range 22 24)]
                                                {:x x :y y})
                  :tetris.board/current-piece [{:x 9 :y 22}
                                               {:x 9 :y 23}])
           {:key :left})))

  (is (= (assoc base-state :tetris.board/current-piece [])
         (tetris/key-pressed
           (assoc base-state :tetris.board/current-piece [])
           {:key :left}))))
