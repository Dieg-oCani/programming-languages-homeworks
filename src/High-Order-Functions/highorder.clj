;----------------------------------------------------------
; Activity: Problem Set: Recursive Functions I
; Date: January 26, 2018.
; Author:
;          A01376119  Diego Canizales Bollain Goytia
;----------------------------------------------------------

(use 'clojure.test)
(use 'clojure.math.numeric-tower)


(defn my-map-indexed
  "This functions returns a list containing the result of applying f to 0
  and the first element of lst, followed by applying f to 1 and the second element in lst,
  and so on until lst is exhausted."
  [fun lst]
  (#(loop [lst lst
           result ()
           index 0]
      (cond
        (empty? lst )  (reverse result)

        :else
        (recur (rest lst) (conj result (apply %1 index (list (first lst)) )) (inc index))))fun))


(defn my-drop-while
  "It returns a list of items of lst dropping the initial items that evaluate to true when passed to f.
  Once a false value is encountered the rest of the list is returned."
  [fun lst]
  (cond
    (empty? lst)  lst
    (false? (apply fun (list(first lst)))) lst
    :else
    (my-drop-while fun (rest lst))))


(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (abs (- x y)) epsilon))


(defn bisection
  "Bisection method is a root finding algorithm.
  Receives a function f, and two points of the function, a and b."
  [a b fun]
  (let [c (/ (+ a b) 2)]
    (cond
      (< (abs (fun c)) (* 1 (expt 10 -15)))    c
      (> (* (fun a) (fun c)) 0)   (bisection c b fun)
      :else
      (bisection a c fun))))

(defn deriv
  "This function returns the derivative of a function f(x) with respect to x, as h goes to zero."
  [fun h]
  (fn [x]
    (/ (- (fun (+ x h)) (fun x))h)))


(defn integral
  "This function returns a usable function, resulting from integrating fn using Simpson's rule.
   It takes lowerlimit, upperlimit and interval size as arguments, where the interval is an even positive integer."
  [a b n fun])

(defn integral
  "It returns the value of the integral, using Simpson's rule.
  (takes as arguments a, b, n, and f)"
  [a b n f]
  (loop [total 0
         cont    0]
    (if (= (inc n) cont)
      (* total (/ (/ (- b a) n) 3))
      (recur (cond
               (or (= cont n) (zero? cont))
               (+ total (f (+ a (* cont (/ (- b a) n)))))

               (odd?  cont)
               (+ total (* 4 (f (+ a (* cont (/ (- b a) n))))))

               (even? cont)
               (+ total (* 2 (f (+ a (* cont (/ (- b a) n)))))))

             (inc cont)))))


; tests for my-map-indexed
(deftest test-my-map-indexed
  (is (= () (my-map-indexed vector ())))
  (is (= '([0 a] [1 b] [2 c] [3 d])
         (my-map-indexed vector '(a b c d))))
  (is (= '(10 4 -2 8 5 5 13)
         (my-map-indexed + '(10 3 -4 5 1 0 7))))
  (is (= '(0 1 -4 3 1 0 6)
         (my-map-indexed min '(10 3 -4 5 1 0 7)))))


; tests for my-drop-while
(deftest test-my-drop-while
  (is (= () (my-drop-while neg? ())))
  (is (= '(0 1 2 3 4)
         (my-drop-while
           neg?
           '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4))))
  (is (= '(2 three 4 five)
         (my-drop-while
           symbol?
           '(zero one 2 three 4 five))))
  (is (= '(0 one 2 three 4 five)
         (my-drop-while
           symbol?
           '(0 one 2 three 4 five)))))

; tests for bisection
(deftest test-bisection
  (is (aprox= 0.0001
              3.0
              (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              -4.0
              (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              Math/PI
              (bisection 1 4 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              (* 2 Math/PI)
              (bisection 5 10 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              1.618033988749895
              (bisection 1 2 (fn [x] (- (* x x) x 1)))))
  (is (aprox= 0.0001
              -0.6180339887498948
              (bisection -10 1 (fn [x] (- (* x x) x 1))))))


; tests for deriv
(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (aprox= 0.05 75 (df 5)))
  (is (aprox= 0.05 30 (ddf 5)))
  (is (aprox= 0.05 6 (dddf 5))))


(run-tests)