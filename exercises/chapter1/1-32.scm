;;; Soros Liu
;;; Exercise 1.32
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;;; verify

(define (sum-cubes a b)
  (sum cube a 1+ b))

(sum-cubes 1 10)
;Value: 3025

(define (factorial n)
  (product (lambda (n) n) 1 1+ n))

(factorial 6)
;Value: 720

;;; b
;;; iterative one

(define (accumulate combiner null-value term a next b)
  (define (iter a acc)
    (if (> a b)
	acc
	(iter (next a) (combiner acc (term a)))))
  (iter a null-value))

(factorial 5)
;Value: 120
