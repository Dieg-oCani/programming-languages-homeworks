(defn compressASequence
  "This function compresses a sequence by identity."
  [coll]
  (->>
    (partition-by identity coll)
    (map first)))

(defn factorialFun
  "Calculates factorials of a given number n."
  [n]
  (->>
    (range 1N (inc n))
    (reduce *)))


;; Sequences
(defn reverse-interleave
  "Reverses the interleave process into x number of subsequences"
  [lst n]
  (->>
    (sort-by #(mod % n) lst)
    (partition (/ (count lst) n))
    (sort-by #(first %))))

(defn rotate-sequence
  "Rotates a sequence in either direction."
  [n lst]
  (cond
    (pos? n) (drop n (take (+ (count lst) n) (cycle lst)))
    :else
    (drop (inc (Math/abs n)) (take (+ (count lst) (inc (Math/abs n))) (cycle lst)))))

(defn flipping-out
  "Flips the order of the arguments of an input function."
  [func]
  (fn [& args] (apply func (reverse args))))


(defn count-a-sequence
  "Counts the total number of elements in a sequence. (Without count)"
  [coll]
  (loop [counter 0
         coll coll]
    (cond
      (empty? coll) counter
      :else
      (recur (inc counter) (rest coll)))))

(defn find-odd-numbers
  "Returns only the odd numbers from a sequence."
  [coll]
  (filter odd? s))


;; unfinished.
(defn count-ocurrences
  "Returns a map containing the number of ocurrences of each distinct element in a collection."
  [coll]
  (map #(zipmap (list (first %)) (list (count %))) (partition-by identity coll)))





;; set-theory
(defn set-intersection
  "Returns the intersection of two sets."
  [s1 s2]
  (disj (set (map #(get s1 %) s2)) nil))





;; unfinished...
(defn power-set
  "Returns the power set of a given set."
  [s]
  (cond
    (= 1 (count s)) #{#{} (set s)}
    :else
    (map #(merge % s) (power-set (rest s)))))




