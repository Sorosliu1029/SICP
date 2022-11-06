;;; Soros Liu
;;; Exercise 2.8
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.8

;;; the lower bound of subtraction is lower of first interval minus upper of second interval
;;; the upper bound of subtraction is upper of first interval minus lower of second interval
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))