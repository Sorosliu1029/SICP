;;; Soros Liu
;;; Exercise 2.8
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.8

;;; the lower bound of subtraction is lower of first interval minus upper of second interval
;;; the upper bound of subtraction is upper of first interval minus lower of second interval
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define x (make-interval 3 4))
(define y (make-interval 1 2))

(sub-interval x y)
;Value: (1 . 3)