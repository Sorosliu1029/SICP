;;; Soros Liu
;;; Exercise 1.7
;;; https://sicp.sorosliu.xyz/book/book-Z-H-10.html#%_thm_1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) .001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;; test
(sqrt 2)
;Value: 1.4142156862745097
;Actual:1.4142135623730951

(sqrt 0.0009)
;Value: .04030062264654547
;Actual:.03
;;; Explanation: for small number, good-enough? convergess too soon,
;;; so loses precision

(sqrt 289)
;Value: 17.000009637317497
;Actual:17.0

(sqrt 289e46)
;Value: 1.7e24
;Actual:1.7e24

;(sqrt 289e48)
;Long-time eval, no result
;Actual: 1.7e25

;;; improvement
(define (abs x)
  (if (< x 0) (- x) x))

(define (new-good-enough? guess x)
  (< (/
      (abs (-
	    (improve guess x)
	    guess))
      guess)
     .001))

(define (new-sqrt-iter guess x)
  (if (new-good-enough? guess x)
      guess
      (new-sqrt-iter (improve guess x)
		     x)))

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

(new-sqrt 2)
;Value: 1.4142156862745097
;Actual:1.4142135623730951

(new-sqrt 0.0009)
;Value: .03002766742182557
;Actual:.03

(new-sqrt 289)
;Value: 17.000009637317497
;Actual:17.0

(new-sqrt 289e46)
;Value: 1.700038906917853e24
;Actual:1.7e24

(new-sqrt 289e48)
;Value: 1.7003787578542446e25
;Actual:1.7e25