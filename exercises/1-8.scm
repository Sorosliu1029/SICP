;;; Soros Liu
;;; Exercise 1.8
;;; https://sicp.sorosliu.xyz/book/book-Z-H-10.html#%_thm_1.8

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x) x)))

(define (improve guess x)
  (/
   (+
    (/ x (square guess))
    (* 2 guess))
   3))

(define (square x) (* x x))

(define (abs x)
  (if (< x 0) (- x) x))

(define (good-enough? guess x)
  (< (/
      (abs (-
	    (improve guess x)
	    guess))
      guess)
     .001))

(define (cube-root x)
  (cube-root-iter 1.0 x))

;;; test
(cube-root 8)
;Value: 2.000004911675504

(cube-root 1)
;Value: 1.

(cube-root 27)
;Value: 3.001274406506175

(cube-root 0.125)
;Value: .5000557786519746

(cube-root 123)
;Value: 4.973220311720365
