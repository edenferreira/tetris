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

(deftest update-board
  (is (= {:height 24
          :width 10
          :piece [{:x 4 :y 0}
                  {:x 5 :y 0}]
          :filled-blocks [{:x 0 :y 23}
                          {:x 1 :y 23}]}
         (tetris/update-board {:height 24
                               :width 10
                               :piece [{:x 0 :y 23}
                                       {:x 1 :y 23}]
                               :filled-blocks []}
                              :down
                              [{:x 0 :y 0}
                               {:x 1 :y 0}])))

  (is (= {:height 24
          :width 10
          :piece [{:x 1 :y 23}
                  {:x 2 :y 23}]
          :filled-blocks []}
         (tetris/update-board {:height 24
                               :width 10
                               :piece [{:x 0 :y 23}
                                       {:x 1 :y 23}]
                               :filled-blocks []}
                              :right
                              [{:x 0 :y 0}
                               {:x 1 :y 0}])))

  (is (= {:height 24
          :width 10
          :piece [{:x 4 :y 1}
                  {:x 5 :y 1}]
          :filled-blocks [{:x 0 :y 23}
                          {:x 1 :y 23}]}
         (tetris/update-board {:height 24
                               :width 10
                               :piece [{:x 4 :y 0}
                                       {:x 5 :y 0}]
                               :filled-blocks [{:x 0 :y 23}
                                               {:x 1 :y 23}]}
                              :down
                              [{:x 0 :y 0}
                               {:x 0 :y 1}])))

  (is (= {:height 24
          :width 10
          :piece [{:x 4 :y 0}
                  {:x 4 :y 1}]
          :filled-blocks [{:x 0 :y 23}
                          {:x 1 :y 23}
                          {:x 1 :y 22}
                          {:x 2 :y 22}]}
         (tetris/update-board {:height 24
                               :width 10
                               :piece [{:x 1 :y 22}
                                       {:x 2 :y 22}]
                               :filled-blocks [{:x 0 :y 23}
                                               {:x 1 :y 23}]}
                              :down
                              [{:x 0 :y 0}
                               {:x 0 :y 1}])))

  (is (= {:height 24
          :width 10
          :piece [{:x 8 :y 21}
                  {:x 9 :y 21}]
          :filled-blocks [{:x 0 :y 23}
                          {:x 1 :y 23}]}
         (tetris/update-board {:height 24
                               :width 10
                               :piece [{:x 8 :y 21}
                                       {:x 9 :y 21}]
                               :filled-blocks [{:x 0 :y 23}
                                               {:x 1 :y 23}]}
                              :right
                              [{:x 0 :y 0}
                               {:x 0 :y 1}]))))
