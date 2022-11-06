;;; Soros Liu
;;; Exercise 2.7
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.7

(define (make-interval a b) (cons a b))

(define (upper-bound i) (max (car i) (cdr i)))

(define (lower-bound i) (min (car i) (cdr i)))

(define i (make-interval 1 2))

(upper-bound i)
;Value: 2

(lower-bound i)
;Value: 1