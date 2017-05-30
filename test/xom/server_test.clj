(ns xom.server-test
  (:require
   [clojure.test :refer [deftest testing is are]]
   [xom.server :as xom]
   ))

(defn make-move
  [row col mark]
  {:pos/row row :pos/col col :pos/mark mark})

(defn with-move
  [game move]
  (update game :xom/positions (fnil conj []) (apply make-move move)))

(defn with-moves
  [game & moves]
  (reduce with-move game moves))

(deftest test-game-code
  (testing "game basics"
    (testing "winning boards"
      (are [board winner] (= winner (xom/winner board))
       [[:x :x :x] [nil :o nil] [:o nil :o]] :x
       [[:x nil :x] [:o :o :o] [:o :x :o]] :o
       [[:x nil :x] [:o :x :o] [:o :o :o]] :o
       [[:x nil :x] [:x :nil :o] [:x :o :o]] :x
       [[:o :x :x] [nil :x :o] [nil :x :o]] :x
       [[:o :x :o] [nil :x :o] [nil nil :o]] :o
       [[:o :x :o] [nil :o :x] [nil :x :o]] :o
       [[:o nil :o] [nil :o :x] [:o :x nil]] :o
       [[:o nil :o] [nil :o :x] [nil :x nil]] :none
       [[:o :x :o] [:x :o :x] [:x :o :x]] :cat))
    (testing "player turn"
      (let [test-game (xom/create-game "a" "b")]
        (is (= :x (xom/player-turn test-game)))
        (is (= :o (xom/player-turn (with-move test-game [0 0 :x]))))
        )
      )
    (testing "valid moves"
      (let [test-game (xom/create-game "a" "b")]
        (is (not (xom/valid-move? test-game (make-move 0 0 :o))))
        (is (xom/valid-move? (with-move test-game [0 0 :x]) (make-move 1 1 :o)))
        (is (not (xom/valid-move? (with-move test-game [0 0 :x]) (make-move 0 0 :o))))
        ))))

