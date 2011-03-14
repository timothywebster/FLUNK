(require 'clojure.contrib.string)

; calculate the alphabetical value for each name (ascii value of A is 65)
(defn calc [s] (reduce + (map #(- (int %) 64) (seq s))))

; walk list of names and add position * calc
(defn nl [n t l]                         
     (cond (empty? l) t                    
            :else (recur (inc n) (+ t (* n (calc (first l)))) (rest l) )))

(->> 
  (slurp "names.txt") 
  (clojure.contrib.string/split #"[^\w]+")
  (sort)
  (nl 0 0)
)


