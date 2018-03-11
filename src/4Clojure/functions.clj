(defn myFactorial
  "Returns the factorial of a given number."
  [n]
  (cond
    (zero? n)  1
    :else
    (*' n (myFactorial (dec n)))))

(defn myiterate
  "The result is a list that does: f, (f x) (f (f x)), ... up to infinity.
  My implementation of the Iterate function in clojure."
  [f x]
  (lazy-seq (cons x (myiterate f (f x)))))

(defn comparisons
  "It's a function that takes three arguments, a less than operator and 2 elements to compare.
  Returns a keyword describing the relatonship between the two elements, they keywords are as follow:
  :gt greater than
  :lt less than
  :eq equal"
  [op e1 e2]
  (cond
    (true? (op e1 e2)) :lt
    (true? (op e2 e1)) :gt
    :else
    :eq))

(defn cartesianProduct
  "Returns the cartesian product of two sets."
  [k v]
  (#(set (for [a (vec %1) b (vec %2)] [a b]))k v))

(defn productDigits
  "Returns the digits of a the multiplication of a times b."
  [a b]
  (loop [res ()
           n (* a b)]
    (cond
      (zero? n) (vec res)
      :else
      (recur (conj res (mod n 10)) (quot n 10)))))

(defn groupSequence
  "Given a function f and a sequence s, write a function which returns a map.
   The keys should be the values of f applied to each item in s.
   The value at each key should be a vector of corresponding items in the order
    they appear in s."
  [func s]
  (apply merge-with into (for [v s] {(func v) [v]})))









;; Helper functions for comparisons debugging #(fn [x y] (< (count x) (count y)))



