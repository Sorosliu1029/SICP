;;; Soros Liu
;;; Exercise 1.41
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.41

(define (double f)
  (lambda (x) (f (f x))))

(define inc 1+)

((double inc) 0) ;;; add 2
;Value: 2

(((double double) inc) 5) ;;; add 4
;Value: 9

;;; using substituion model
;;; =>
(((lambda (x) (double (double x))) inc) 5)
;;; =>
((double (double inc)) 5)
;;; =>
((double (lambda (x) (inc (inc x)))) 5)
;;; =>
((lambda (x) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) x))) 5)
;;; =>
((lambda (x) ((lambda (x) (inc (inc x))) (inc (inc x)))) 5)
;;; =>
((lambda (x) (inc (inc (inc (inc x))))) 5)
;;; =>
(inc (inc (inc (inc 5))))
9

(((double (double double)) inc) 5) ;;; add 16
;Value: 21

;;; using substituion model
;;; =>
(((lambda (x) ((double double) ((double double) x))) inc) 5)
;;; =>
(((double double) ((double double) inc)) 5)

(define inc4 ((double double) inc))
;;; =>
(((double double) inc4) 5)
;;; =>
(inc4 (inc4 (inc4 (inc4 5))))
;;; =>
21