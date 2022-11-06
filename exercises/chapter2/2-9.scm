;;; Soros Liu
;;; Exercise 2.9
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.9

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;; the width of the sum of two intervals is a function only of
;;; the widths of the intervals being added

(width (add-interval x y))
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

;;; not true for multiplication

(width (mul-interval x y))
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