;;; Soros Liu
;;; Exercise 1.42
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.42

(define (compose f g)
  (lambda (x) (f (g x))))

(define inc 1+)

((compose square inc) 6)
;Value: 49