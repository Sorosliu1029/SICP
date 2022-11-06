;;; Soros Liu
;;; Exercise 2.9
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.9

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define x (make-interval 3 4))
(define y (make-interval 1 2))

;;; the width of the sum of two intervals is a function only of
;;; the widths of the intervals being added
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(width (add-interval x y))
;Value: 1

;;; => (width (make-interval (+ (lower-bound x) (lower-bound y))
;;; => 		      (+ (upper-bound x) (upper-bound y))))
;;; => (/ (-
;;; =>     (upper-bound (make-interval (+ (lower-bound x) (lower-bound y))
;;; => 				(+ (upper-bound x) (upper-bound y))))
;;; =>     (lower-bound (make-interval (+ (lower-bound x) (lower-bound y))
;;; => 				(+ (upper-bound x) (upper-bound y))))
;;; =>     ) 2)
;;; => (/ (-
;;; =>     (+ (upper-bound x) (upper-bound y))
;;; =>     (+ (lower-bound x) (lower-bound y))
;;; =>     ) 2)
;;; => (/ (+ (- (upper-bound x) (lower-bound x))
;;; =>       (- (upper-bound y) (lower-bound y)))
;;; =>    2)
;;; => (+ (/ (- (upper-bound x) (lower-bound x)) 2)
;;; =>    (/ (- (upper-bound y) (lower-bound y)) 2))
;;; => (+ (width x) (width y))

(+ (width x) (width y))
;Value: 1

;;; not true for multiplication
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(width (mul-interval x y))
;Value: 5/2

;;; suppose x and y both is in positive range
;;; => (width (make-interval (* (lower-bound x) (lower-bound y))
;;; =>                       (* (upper-bound x) (upper-bound y))))
;;; => (/ (-
;;; =>     (upper-bound (make-interval (* (lower-bound x) (lower-bound y))
;;; => 				(* (upper-bound x) (upper-bound y))))
;;; =>     (lower-bound (make-interval (* (lower-bound x) (lower-bound y))
;;; => 				(* (upper-bound x) (upper-bound y))))
;;; =>     ) 2)
;;; => (/ (-
;;; =>     (* (upper-bound x) (upper-bound y))
;;; =>     (* (lower-bound x) (lower-bound y))
;;; =>     ) 2)
;;; not fuction only of the widths of the intervals being multiplied