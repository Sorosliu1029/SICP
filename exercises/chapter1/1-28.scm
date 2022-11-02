;;; Soros Liu
;;; Exercise 1.28
;;; https://sicp.sorosliu.xyz/book/book-Z-H-11.html#%_thm_1.28

(define (square n) (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (try-discover-nontrivial (expmod base (/ exp 2) m)
				  m))
	(else
	  (remainder (* base (expmod base (- exp 1) m))
		     m))))

(define (try-discover-nontrivial emod n)
  (if (nontrivial? emod n)
      0
      (remainder (square emod) n)))

(define (nontrivial? a n)
  (and (not (= a 1))
       (not (= a (- n 1)))
       (= (remainder (square a) n) 1)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((miller-rabin-test n) (fast-prime? n (- times 1)))
	(else false)))

(fast-prime? 2 5)
;Value: #t

(fast-prime? 3 10)
;Value: #t

(fast-prime? 4 10)
;Value: #f

(fast-prime? 11 10)
;Value: #t

(fast-prime? 13 10)
;Value: #t

(fast-prime? 24 10)
;Value: #f

(fast-prime? 561 10)
;Value: #f

(fast-prime? 1105 10)
;Value: #f

(fast-prime? 1729 10)
;Value: #f

(fast-prime? 2465 10)
;Value: #f

(fast-prime? 2821 10)
;Value: #f

(fast-prime? 6601 10)
;Value: #f

(fast-prime? 559 10)
;Value: #f

(fast-prime? 824633720831 10)
;Value: #t