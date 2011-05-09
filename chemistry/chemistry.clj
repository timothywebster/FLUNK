(require 'clojure.contrib.string)

; split a single compound into a list of elements and subscripts
(defn chem-split [s] (re-seq #"\d?[A-Z][a-z]|[A-Z]|\d+" s))

; parse a string into an int if it looks like an int, otherwise leave it be
(defn soft-parse [s] (cond (re-matches #"\d+" s) (Integer/parseInt s) :else s))

; serialize a hash to a string so that we can compare it to other hashes
(defn stringify [h] (reduce str (map #(str (get h %) %) (sort (keys h)))))

; sugar for merge-with
(defn  addKey [x y h] (merge-with + (hash-map x y) h))

; convert a compound (in list format) into a hashtable with element symbols as keys
(defn listToHash [l] 
  (let [
    multi? (number? (first l)) 
    multi (cond multi? (first l) :else 1)
    lst (cond multi? (rest l) :else l)
     ]
    (loop [rst lst acc {}]
      (cond 
        (= 0 (count rst)) acc
        (= 1 (count rst)) (addKey (first rst) multi acc)
        (number? (second rst)) (recur (take-last (- (count rst) 2)  rst) (addKey (first rst) (* multi (second rst)) acc))
        :else (recur (rest rst) (addKey (first rst) multi acc))))))

; take a compound string, split it into a list, convert int strings to ints, and convert the list to a hashtable
(defn chem2hash [s]
	(->>
	  (chem-split s)
	  (map soft-parse)
	  (listToHash)))

; split one side of reaction into individual compounds, put each into a hashtable, merge all hashtables and represent the result as a string
(defn chemlist2string [chem-list]
  (->>
	(clojure.contrib.string/split #"\+" chem-list)
	(map chem2hash)
	(reduce #(merge-with + %1 %2))
	(stringify)))

; compare both sides of a reaction
(defn balanced? [s]
	(->>
	(clojure.contrib.string/split #"=" s)
	(map chemlist2string)
	(apply =)))

; output results in something like the specified format
(defn my-test [s] 
      (println (str s " " (balanced? s))))

;run an example
(my-test "H2O=H2O")
