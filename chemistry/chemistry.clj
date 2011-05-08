(require 'clojure.contrib.string)

(defn chem-split [s] (re-seq #"\d?[A-Z][a-z]|[A-Z]|\d+" s))

(defn soft-parse [s] (cond (re-matches #"\d+" s) (Integer/parseInt s) :else s))

(defn stringify [h] (reduce str (map #(str (get h %) %) (sort (keys h)))))

(defn  addKey [x y h] (merge-with + (hash-map x y) h))

(defn toHash [l] 
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

(defn chem2hash [s]
	(->>
	  (chem-split s)
	  (map soft-parse)
	  (toHash)))

(defn chemlist2string [chem-list]
  (->>
	(clojure.contrib.string/split #"\+" chem-list)
	(map chem2hash)
	(reduce #(merge-with + %1 %2))
	(stringify)))

(defn balanced? [s]
	(->>
	(clojure.contrib.string/split #"=" s)
	(map chemlist2string)
	(apply =)))

