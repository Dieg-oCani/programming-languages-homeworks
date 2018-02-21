;----------------------------------------------------------
; Activity: Problem Set: Recursive Functions I
; Date: February 28, 2018.
; Author:
;          A01376119  Diego Canizales Bollain Goytia
;----------------------------------------------------------

(use 'clojure.test)


(defn positives
  "Takes a list as an argument and returns a new list that only contains the positive numbers of lst."
  [lst]
  (filter pos? lst))

(defn dot-product
  "Returns the result of applying the dot product of list one and list two."
  [lst1 lst2]
  (reduce + (map * lst1 lst2)))

(defn pow
  "Takes two numbers, one number a and a positive integer b.
  Returns the result of computing a to the power b."
  [a b]
  (reduce * (repeat b a)))

(defn replic
  "Returns a new list with n copies of each element in lst")





;; Unit tests

(deftest test-positives
  (is (= () (positives '())))
  (is (= () (positives '(-4 -1 -10 -13 -5))))
  (is (= '(3 6) (positives '(-4 3 -1 -10 -13 6 -5))))
  (is (= '(4 3 1 10 13 6 5) (positives '(4 3 1 10 13 6 5)))))


(deftest test-dot-product
  (is (= 0 (dot-product () ())))
  (is (= 32 (dot-product '(1 2 3) '(4 5 6))))
  (is (= 21.45 (dot-product '(1.3 3.4 5.7 9.5 10.4)
                            '(-4.5 3.0 1.5 0.9 0.0)))))

(deftest test-pow
  (is (= 1 (pow 0 0)))
  (is (= 0 (pow 0 1)))
  (is (= 1 (pow 5 0)))
  (is (= 5 (pow 5 1)))
  (is (= 125 (pow 5 3)))
  (is (= 25 (pow -5 2)))
  (is (= -125 (pow -5 3)))
  (is (= 1024 (pow 2 10)))
  (is (= 525.21875 (pow 3.5 5)))
  (is (= 129746337890625 (pow 15 12)))
  (is (= 3909821048582988049 (pow 7 22))))

(run-tests)