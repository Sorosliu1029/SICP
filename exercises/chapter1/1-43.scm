;;; Soros Liu
;;; Exercise 1.43
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.43

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (-1+ n)))))

((repeated square 2) 5)
;Value: 625
