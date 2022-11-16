;;; Soros Liu
;;; Exercise 2.18
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.18

(define (reverse items)
  (define (iter items acc)
    (if (null? items)
      acc
      (iter (cdr items) (cons (car items) acc))))
  (iter items ()))

(reverse (list 1 4 9 16 25))
;Value: (25 16 9 4 1)

(reverse (list 10))
;Value: (10)

(reverse (list ))
;Value: ()