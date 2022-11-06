;;; Soros Liu
;;; Exercise 2.12
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.12

(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define (percent i)
  (/ (width i) (center i)))

(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define x (make-center-percent 6.8 0.1))

x
;Value: (6.12 . 7.4799999999999995)

(percent x)
;Value: .09999999999999996

(center x)
;Value: 6.8