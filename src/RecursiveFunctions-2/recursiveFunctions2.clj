;----------------------------------------------------------
; Activity: Problem Set: Recursive Functions I
; Date: January 26, 2018.
; Author:
;          A01376119  Diego Canizales Bollain Goytia
;----------------------------------------------------------

(use 'clojure.test)


(defn my-repeat
  "Takes a number n and any data x as it's arguments.
  Returns a list that contains n copies of x."
  [n, x]
  (cond
    (zero? n) ()
    (= n 1)  (cons x ())
    :else
    (cons x (my-repeat (dec n) x))))

(defn invert-pairs
  "Takes as an argument a list of vectors containing two elements each.
  Returns a new list with every vector pair inverted."
  [lst]
  (cond

    (empty? lst)
    lst

    (or (number? (first lst)) (symbol? (first lst)))
    (into [] (concat (rest lst) (list (first lst))))

    :else
    (cons (invert-pairs (first lst)) (invert-pairs (rest lst)))))


(defn enlist
  "Surrounds in a list every upper-level element of the list it takes as an argument."

  [lst]
  (cond
    (empty? lst) lst

    (empty? (rest lst)) (list lst)

    :else
    (cons (list (first lst)) (enlist (rest lst)))))

(defn my-interleave
  "Returns a new list with every element of a[n] interleaved with an element of b[n].
  example: (my-interleave '(1 2 3 4) '(a b c d)) --> '(1 a 2 b 3 c 4 d)"
  [a, b]
  (cond
    (or (empty? a) (empty? b))  ()
    :else
    (concat (cons (first a) (list (first b))) (my-interleave (rest a) (rest b)))))

(defn my-flatten
  "Removes all the interior parenthesis of the list it takes as an input."
  [lst]
  (cond
    (empty? lst)  lst
    (list? (first lst))  (concat (my-flatten (first lst)) (my-flatten (rest lst)))
    :else
    (cons (first lst) (my-flatten (rest lst)))))


(defn exchange
  [x1, x2, lst]
  (cond
    (empty? lst)  ()

    (= (first lst) x1)
    (cons x2 (exchange x1 x2 (rest lst)))

    (= (first lst) x2)
    (cons x1 (exchange x1 x2 (rest lst)))

    (list? (first lst))
    (cons (exchange x1 x2 (first lst)) (exchange x1 x2 (rest lst)))
    :else
    (cons (first lst) (exchange x1 x2 (rest lst)))))


(defn insert
  "Takes an element n and inserts it in the correct position in a list lst."
  [n, lst]
  (cond
    (empty? lst) (cons n lst)
    (>= (first lst) n)  (cons n lst)
    :else
    (cons (first lst) (insert n (rest lst)))))

(defn my-sort
  "Takes an unordered list of numbers as an argument and returns a new list
  with it's elements in ascending order."
  [lst]
  (cond
    (empty? lst)  lst
    :else
    (insert (first lst) (my-sort (rest lst)))))


(defn prime?
  "Function to check if a given number is a prime."
  [n]
  (loop [i 2
         n n]
    (cond
      (< n (* 2 i) ) true
      (not (= (mod n i) 0)) (recur (inc i) n)
      :else
      false)))


(defn prime-factors
  "Returns the number n received as an argument in form of it's prime factors."
  [n]
  (loop [n n
         i 2
         lst ()]
    (cond
      (= n 1)  (my-sort lst)
      (prime? i) (cond
                   (= (mod n i) 0) (recur (/ n i) i (cons i lst))
                   :else
                   (recur n (inc i) lst))
      :else
      (recur n (inc i) lst))))

(defn compress
  "Returns a list with the consecutive repeated elements compressed into one only element."
  [lst]
  (loop [lst lst
         res ()]
    (cond
      (empty? lst) (reverse res)
      (empty? res)  (recur (rest lst) (cons (first lst) res))
      (= (first res) (first lst))  (recur (rest lst) res)
      :else
      (recur (rest lst) (cons (first lst) res)))))


(defn firstof?
  "Determines wether an item is the first element of a given list lst. Returns false otherwise."
  [item, lst]
  (cond
    (empty? lst)  false
    (= item (first lst)) true
    :else
    false))


(defn pack
  "Takes a list as an argument and packs repeated values into separate sublists."
  [lst]
  (loop [lst lst
         res ()
         tmp ()
         flag false]
    (cond
      (empty? lst)  (reverse (drop-last(drop-last (conj res tmp))))
      (firstof? (first lst) tmp)  (recur (rest lst) res (cons (first lst) tmp) true)
      (false? flag) (recur (rest lst) (list tmp res) (list (first lst)) true)
      :else
      (recur (rest lst) (conj res tmp) (list (first lst)) true))))



(defn encode
  "Takes a list lst as an argument. The function encodes into a list of vectors,
  where vector[0] = number of repetitions of element, vector[1] = element."
  [lst]
  (cond
    (empty? lst)  ()
    :else
    (#(loop [lst %1
             count 0
             res ()
             tmp %2]
        (cond
          (empty? lst) (reverse (conj res (into [] (concat (list count) (list tmp)))))
          (firstof? tmp lst)  (recur (rest lst) (inc count) res (first lst))
          :else
          (recur (rest lst) 1 (conj res (into [] (concat (list count) (list tmp)))) (first lst)))) lst (first lst))))


(defn encode-modified
  "Takes a list lst as an argument. The function encodes into a list of vectors if the number has duplicates.
  Where the vector[0] is the number of repetitions of element and vector[1] is the element.
  The arguments that do not have any duplicates simply themeselves to the list."
  [lst]
  (cond
    (empty? lst) ()
    :else
    (#(loop [lst %1
             count 0
             res ()
             tmp %2]
        (cond
          (empty? lst) (cond
                         (> count 1) (reverse (conj res (into [] (concat (list count) (list tmp)))))
                         :else
                         (reverse (conj res tmp)))
          (firstof? tmp lst)  (recur (rest lst) (inc count) res (first lst))
          :else
          (cond
            (> count 1)  (recur (rest lst) 1 (conj res (into [] (concat (list count) (list tmp)))) (first lst))
            :else
            (recur (rest lst) 1 (conj res tmp) (first lst))))) lst (first lst))))





; Unit tests

; tests for my-repeat
(deftest test-my-repeat
         (is (= () (my-repeat 0 'x)))
         (is (= '(6 6 6) (my-repeat 3 6)))
         (is (= '((ha ha) (ha ha) (ha ha)) (my-repeat 3 '(ha ha))))
         (is (= '(true true true true true) (my-repeat 5 true))))

; tests for invert-pairs
(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))
      (invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))

; tests for enlist
(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8))
         (enlist '((1 2 3) 4 (5) 7 8)))))

; tests for my-interleave
(deftest test-my-interleave
  (is (= () (my-interleave () ())))
  (is (= () (my-interleave '(a) ())))
  (is (= () (my-interleave () '(1))))
  (is (= '(a 1 b 2 c 3 d 4 e 5)
         (my-interleave '(a b c d e) '(1 2 3 4 5))))
  (is (= '(a 1 b 2 c 3 d 4)
         (my-interleave '(a b c d e) '(1 2 3 4))))
  (is (= '(a 1 b 2 c 3 d 4)
         (my-interleave '(a b c d) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a b c d e) '(1)))))

; tests for my-flatten
(deftest test-my-flatten
  (is (= () (my-flatten ())))
  (is (= '(a b c d e) (my-flatten '((a b) ((c) d (e))))))
  (is (= '(one two three four)
         (my-flatten '(((one) ((two))) () (three (())) four)))))

; tests for exchange
(deftest test-exchange
  (is (= () (exchange 'x 'y ())))
  (is (= '(d b c a) (exchange 'a 'd '(a b c d))))
  (is (= '((42) true ((cool (true)) (42))))
      (exchange true 42 '((true) 42 ((cool (42)) (true))))))

; tests for insert
(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

; tests for my-sort
(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))

; tests for prime-factors
(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))


; tests for compress.
(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e)
         (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

; tests for pack
(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

; tests for encode
(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5]) (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))


; tests for encode-modified
(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))
(run-tests)