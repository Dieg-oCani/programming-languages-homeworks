(use 'clojure.set)

(defn myFactorial
  "Returns the factorial of a given number."
  [n]
  (cond
    (zero? n)  1
    :else
    (*' n (myFactorial (dec n)))))

(defn myIterate
  "The result is a list that does: f, (f x) (f (f x)), ... up to infinity.
  My implementation of the Iterate function in clojure."
  [f x]
  (lazy-seq (cons x (myIterate f (f x)))))

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
  (#(set (for [a (vec %1) b (vec %2)] [a b]))k v)) ;; uses destructuring.

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

(defn sumSquareDigits
  "Sums the digits"
  [digits]
  (reduce + (map #(* % %) digits)))

(defn digits
  "Returns digits of a number"
  [n]
  (loop [res ()
         n n]
    (cond
      (zero? n) res
      :else
      (recur (conj res (mod n 10)) (quot n 10)))))

(defn smallerThanSumDigits
  "This functon finds which elements are smaller than the sum of the squares of it's digits."
  [lst]
  (count (filter #(< (first %) (second %)) (map-indexed vector (map #(sumSquareDigits (digits %)) lst)))))


(defn symmetricDifference
  "Returns the items that belong to one but not both of the two sets."
  [set1 set2]
  (let [s1 (map #(cond
                   (nil? (get set2 %)) %
                   :else
                   nil) set1)
        s2 (map #(cond
                   (nil? (get set1 %)) %
                   :else
                   nil) set2)]
    (disj (set (concat s1 s2)) nil)))


(defn dotProduct
  "Returns the dot product of two sequences s1 and s2"
  [s1 s2]
  (->> (interleave s1 s2) (partition 2) (map vec) (map (fn [a] (* (first a) (second a)))) (reduce +)))


(defn readBinary
  "Receives a list of digits of a binary number and returns the base 2 number."
  [binary]
  (->> (seq binary) (reverse) (map-indexed vector) (map (fn [a] (cond
                                                                  (= (second a) \1) (int (Math/pow 2 (first a)))
                                                                  :else
                                                                  0))) (reduce +)))

(defn pascalsTriangle
  "Receives a floor of the pascals triangle and it computes it."
  [n]
  (cond
    (= n 1) [1]
    :else
    (loop [i 1
           res ()]
      (cond
        (= i (max n)) (first res)
        :else
        (recur (inc i) (conj res (vec (->>
                                       (partition 2 1 (first res))
                                       (map #(reduce + %))
                                       (cons 1)
                                       (reverse)
                                       (cons 1)))))))))

(defn pascalsTriangleEnhanced
  "Receives a floor of the pascals triangle and computes it."
  [n]
  (last (take n (iterate #(map +' `(0 ~@%) `(~@% 0)) [1]))))












