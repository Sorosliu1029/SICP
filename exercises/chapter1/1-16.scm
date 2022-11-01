;;; Soros Liu
;;; Exercise 1.16
;;; https://sicp.sorosliu.xyz/book/book-Z-H-11.html#%_thm_1.16

(define (fast-expt b n)
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
	  ((even? n) (fast-expt-iter (square b) (/ n 2) a))
	  (else (fast-expt-iter b (- n 1) (* a b)))))
  (fast-expt-iter b n 1))

(define (even? n)
  (= (remainder n 2) 0))

(define (square n) (* n n))

(fast-expt 2 0)
;Value: 1

(fast-expt 2 1)
;Value: 2

(fast-expt 2 2)
;Value: 4

(fast-expt 2 3)
;Value: 8

(fast-expt 2 10)
;Value: 1024