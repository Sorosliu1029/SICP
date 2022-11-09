;;; Soros Liu
;;; Exercise 1.44
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.44

(define (smooth f)
  (define dx 0.00001)
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (average a b c) (/ (+ a b c) 3))

((smooth sin) 1)
;Value: .8414709847798475

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (-1+ n)))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

((n-fold-smooth sin 10) 1)
;Value: .8414709845274063
