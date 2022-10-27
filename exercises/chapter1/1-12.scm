;;; Soros Liu
;;; Exercise 1.12
;;; https://sicp.sorosliu.xyz/book/book-Z-H-11.html#%_thm_1.12

(define (pascal i j)
  (cond ((= j 1) 1)
	((= j i) 1)
	(else (+
	       (pascal (- i 1) (- j 1))
	       (pascal (- i 1) j)))))

(pascal 1 1)
;Value: 1

(pascal 2 1)
;Value: 1

(pascal 2 2)
;Value: 1

(pascal 3 1)
;Value: 1

(pascal 3 2)
;Value: 2

(pascal 3 3)
;Value: 1

(pascal 4 1)
;Value: 1

(pascal 4 2)
;Value: 3

(pascal 4 3)
;Value: 3

(pascal 4 4)
;Value: 1

(pascal 5 1)
;Value: 1

(pascal 5 2)
;Value: 4

(pascal 5 3)
;Value: 6

(pascal 5 4)
;Value: 4

(pascal 5 5)
;Value: 1
