;;; Soros Liu
;;; Exercise 1.19
;;; https://sicp.sorosliu.xyz/book/book-Z-H-11.html#%_thm_1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (even? n)
  (= (remainder n 2) 0))

(define (square n) (* n n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square p) (square q)) ; compute p'
		   (+ (square q) (* 2 p q)) ; compute q'
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))

(fib 0)
;Value: 0

(fib 1)
;Value: 1

(fib 2)
;Value: 1

(fib 3)
;Value: 2

(fib 4)
;Value: 3

(fib 5)
;Value: 5

(fib 6)
;Value: 8

(fib 7)
;Value: 13

(fib 8)
;Value: 21

(fib 9)
;Value: 34

(fib 10)
;Value: 55