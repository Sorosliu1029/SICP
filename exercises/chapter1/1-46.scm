;;; Soros Liu
;;; Exercise 1.46
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.46

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
	  guess
	  (iter next))))
  iter)

(define (sqrt x)
  (define tolerance 0.00001)
  (define (good-enough? guess next)
    (< (abs (- next guess)) tolerance))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(define (average a b) (/ (+ a b) 2))

(sqrt 2)
;Value: 1.4142156862745097

(sqrt 3)
;Value: 1.7320508100147274

(sqrt 4)
;Value: 2.0000000929222947

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (good-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define improve f)
  ((iterative-improve good-enough? improve) first-guess))

(fixed-point cos 1.0)
;Value: .7390893414033927

(fixed-point (lambda (x) (/ (log 1000) (log x)))
	     2.0)
;Value: 4.555540912917957