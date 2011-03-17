(au:clear-graph)
(define piano (au:make-node "aumu" "dls " "appl"))
(au:connect-node piano 0 *au:output-node* 0)
(au:update-graph)


(define slide (lambda (n)
(cond ((= 0 n) #f)
     (else (play-sound (+ (* 1000 n) (now)) piano (+ (* .1 n) 60) 100 1000)
     (slide (- n 1))))))

   
(slide 5000)
   
(define random-dur (lambda () (/ (* 2 *second*) (random 1 8))))

(define hammer (lambda (note times) 
(letrec (
    (t (random-dur)) 
    (h (lambda (n)
	 (cond ((= 0 n) #f)
	   (else                      
		  (play-sound (+ (* n t) (now)) piano note 100 t)
		 (h (- n 1))
		 ))))) 
(h times))))

(hammer 50 100)
(hammer 62.5 100)
(hammer 75 100)
(hammer 50.75 100)
