;;; Soros Liu
;;; Exercise 1.17
;;; https://sicp.sorosliu.xyz/book/book-Z-H-11.html#%_thm_1.17

(define (double n) (* 2 n))

(define (halve n) (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (multiply a b)
  (cond ((= b 0) 0)
	((even? b) (multiply (double a) (halve b)))
	(else (+ a (multiply a (+ b -1))))))

(multiply 2 0)
;Value: 0

(multiply 2 1)
;Value: 2

(multiply 2 2)
;Value: 4

(multiply 2 11)
;Value: 22
