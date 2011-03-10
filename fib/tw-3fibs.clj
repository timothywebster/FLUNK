; little schemers
(defn fib1 [my-n]
(letfn [(fib [n]
    (cond 
      (<= n 0) '()
      (= n 1) '(1)
      (= n 2) '(1 1)
      :else (let [smaller (fib (- n 1))] (cons (apply + (take 2 smaller)) smaller))))]
  (reverse (fib my-n))))

; little schemers w TCO
(defn fib1a [n]
(letfn [(fib [n acc]
  (cond
    (<= n 0) '()
    (= n 1) '(1)
    (= n 2) acc
    :else (recur (- n 1) (cons (apply + (take 2 acc)) acc))))]
  (reverse (fib n '(1 1)))))

; use map
(defn fib2 [my-n]
  (letfn [(fib [n x y]
    (cond (= n 0) x
      :else (recur (- n 1) y (+ x y) )))]
    (map #(fib % 1 1) (range  my-n)) 
    ))

(defn fib2a [my-n]
  (letfn [(fib [n x y]
    (cond (= n 0) x
      :else (recur (- n 1) y (+ x y) )))]
    (pmap #(fib % 1 1) (range  my-n)) 
    ))

(defn fib2b [my-n]
  (letfn [(fib [n x y]
    (cond (= n 0) x
      :else (recur (- n 1) y (+ x y) ))) ]
    (let [mfib (memoize fib)]
    (map #(mfib % 1 1) (range  my-n)) 
    )))

; use lazy sequences
(defn fib3 [n]
  (letfn [(rfib [a b] (lazy-seq (cons a (rfib b (+ a b)))))]
        (take n (rfib 1 1))
))

