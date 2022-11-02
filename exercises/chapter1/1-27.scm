;;; Soros Liu
;;; Exercise 1.27
;;; https://sicp.sorosliu.xyz/book/book-Z-H-11.html#%_thm_1.27

(define (square n) (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	  (remainder (* base (expmod base (- exp 1) m))
		     m))))

(define (fermat-fool n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (test-iter a)
    (cond ((= a 0) true)
	  ((try-it a) (test-iter (- a 1)))
	  (else false)))
  (test-iter (- n 1)))

(fermat-fool 561)
;Value: #t

(fermat-fool 1105)
;Value: #t

(fermat-fool 1729)
;Value: #t

(fermat-fool 2465)
;Value: #t

(fermat-fool 2821)
;Value: #t

(fermat-fool 6601)
;Value: #t

(fermat-fool 13)
;Value: #t

(fermat-fool 100)
;Value: #f

(fermat-fool 559)
;Value: #f