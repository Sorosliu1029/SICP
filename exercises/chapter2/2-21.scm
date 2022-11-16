;;; Soros Liu
;;; Exercise 2.21
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.21

(define nil ())
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;;; definition 1
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4))
;Value: (1 4 9 16)

;;; definition 2
(define (square-list items)
  (map square items))

(square-list (list 1 2 3 4))
;Value: (1 4 9 16)
