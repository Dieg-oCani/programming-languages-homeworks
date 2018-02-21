;----------------------------------------------------------
; Activity: Problem Set: Recursive Functions I
; Date: January 26, 2018.
; Author:
;          A01376119  Diego Canizales Bollain Goytia
;----------------------------------------------------------


(use 'clojure.test)


(defn my-count
  "Returns number of elements recursively."
  [lst]
  (if (empty? lst) 0
    (inc (my-count (rest lst)))))

(defn add-list
  "Returns the sum of all numbers in a list."
  [lst]
  (if (empty? lst)
    0
    (+ (first lst) (add-list (rest lst)))))

(defn member?
  "Returns true if an item is in a list. Returns false otherwise."
  [item, lst]
  (if (empty? lst)
    false
    (if (= (first lst) item)
      true
      (member? item (rest lst)))))

(defn list-of-symbols?
  "Returns true if all elements in a list are symbols or false otherwise."
  [lst]
  (if (empty? lst)
    true
    (if (symbol? (first lst))
      (list-of-symbols? (rest lst))
      false)))

(defn my-last
  "Returns the last symbol of a given list or nil if the list is empty."
  [lst]
  (if (empty? lst) nil
    (if (empty? (rest lst)) (first lst)
      (my-last (rest lst)))))

(defn cons-end
  "Returns the same received list but with the item at the end of the list."
  [x, lst]
  (if (empty? lst)  (cons x ())
                    (cons (first lst) (cons-end x (rest lst)))))

(defn my-reverse
  "Returns the given list in reverse order."
  [lst]
  (if (empty? (rest lst))   lst
                            (concat (my-reverse (rest lst)) (list (first lst)))))


(defn my-butlast
  "Returns the given list excepting the last element."
  [lst]
  (cond
    (empty? lst)    nil
    (empty? (rest lst))   ()
    :else
    (concat (list (first lst)) (my-butlast (rest lst)))))

(defn my-concat
  "Returns the result of concatenating two given lists."
  [lstA, lstB]
  (cond
    (empty? lstA)  lstB
    (empty? lstB)  lstA
    (empty? (rest lstA))  (cons (first lstA) lstB)
    :else
    (cons (first lstA) (my-concat (rest lstA) lstB))))

(defn deep-reverse
  "Returns the list in reverse order. Works with nested lists."
  [lst]
  (if (empty? lst)
    lst
    (if (list? (last lst))
      (cons (deep-reverse (last lst)) (deep-reverse (butlast lst)))
      (cons (last lst) (deep-reverse (butlast lst))))))



;Unit tests

;; tests for my-count
(deftest test-my-count
  (is (= 0 (my-count ())))
  (is (= 1 (my-count '(a))))
  (is (= 3 (my-count '(a b c)))))

;; tests for add-list
(deftest test-add-list
  (is (= 0 (add-list ())))
  (is (= 10 (add-list '(2 4 1 3))))
  (is (= 55 (add-list '(1 2 3 4 5 6 7 8 9 10)))))

;; tests for member?
(deftest test-member?
  (is (not (member? 'a ())))
  (is (member? 'a '(a b c)))
  (is (member? 'a '(c b a b c)))
  (is (not (member? 'x '(a b c)))))

;; tests for lists-of-symbols?
(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))

;; tests for my-last
(deftest test-my-last
  (is (nil? (my-last ())))
  (is (= 'x (my-last '(x))))
  (is (= 'c (my-last '(a b c)))))

;; tests for cons-end
(deftest test-cons-end
  (is (= '(b c d a) (cons-end 'a '(b c d))))
  (is (= '(a) (cons-end 'a ()))))

;; tests for my-reverse
(deftest test-my-reverse
  (is (= () (my-reverse ())))
  (is (= '(c b a) (my-reverse '(a b c))))
  (is (= '(3 (b c d) a) (my-reverse '(a (b c d) 3)))))

;; tests for my-butlast
(deftest test-my-butlast
  (is (nil? (my-butlast ())))
  (is (= () (my-butlast '(x))))
  (is (= '(a b) (my-butlast '(a b c)))))

;; tests for my-concat
(deftest test-my-concat
  (is (= '(a b c) (my-concat '(a b c) ())))
  (is (= '(1 2 3) (my-concat () '(1 2 3))))
  (is (= '(a b c 1 2 3) (my-concat '(a b c) '(1 2 3)))))

;; tests for deep-reverse
(deftest test-deep-reverse
  (is (= () (deep-reverse ())))
  (is (= '(3 (d c b) a) (deep-reverse '(a (b c d) 3))))
  (is (= '(((6 5) 4) 3 (2 1))
         (deep-reverse '((1 2) 3 (4 (5 6)))))))

(run-tests)