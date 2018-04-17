(defmacro my-or
  "Evaluates from left to right the operations received in x, or more.
  Returns the first logical true, returns the value from the last expression otherwise."
  ([] nil)
  ([x] `(if ~x ~x ~x))
  ([x & next] `(if ~x ~x (my-or ~@next))))

(defmacro do-loop
  "Implementation of a do-loop, in which every operations in the body is executed
  at least one time before cheking for a test. It can receive an n number of ops
  and a test in the last argument.
  There are two keywords for the test, if you use an :until keyword, the body will
  execute while the condition returns a logical false. If you use the :while keyword
  the body is executed while the condition holds a logicall true.
  The code is prompt to changes, you can add more keywords if you like. :) "
  [& more]
  `(let [key# ~(first (last more))]
     (cond
       (= key# :while)
       (loop []
         ~@(butlast more)
         (if ~(last (last more))
           (recur)))
       :else
       (loop []
         ~@(butlast more)
         (if (not ~(last (last more)))
           (recur))))))