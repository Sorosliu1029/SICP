;;; Soros Liu
;;; Exercise 1.18
;;; https://sicp.sorosliu.xyz/book/book-Z-H-11.html#%_thm_1.18

(define (double n) (* 2 n))

(define (halve n) (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (multiply a b)
  (define (multiply-iter a b s)
    (cond ((= b 0) s)
	  ((even? b) (multiply-iter (double a) (halve b) s))
	  (else (multiply-iter a (+ b -1) (+ s a)))))
  (multiply-iter a b 0))

(multiply 2 0)
;Value: 0

(multiply 2 1)
;Value: 2

(multiply 2 2)
;Value: 4

(multiply 2 11)
;Value: 22

