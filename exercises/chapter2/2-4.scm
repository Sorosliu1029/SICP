;;; Soros Liu
;;; Exercise 2.4
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define x 1)
(define y 2)

(car (cons x y))
;Value: 1

;;; (car (cons x y))
;;; => (car (lambda (m) (m x y)))
;;; => ((lambda (m) (m x y)) (lambda (p q) p))
;;; => ((lambda (p q) p) x y)
;;; => x

(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons x y))
;Value: 2