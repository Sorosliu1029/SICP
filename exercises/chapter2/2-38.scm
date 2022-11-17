;;; Soros Liu
;;; Exercise 2.38
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.38

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define nil ())

(fold-right / 1 (list 1 2 3))
;Value: 3/2
(fold-left / 1 (list 1 2 3))
;Value: 1/6

(fold-right list nil (list 1 2 3))
;Value: (1 (2 (3 ())))
(fold-left list nil (list 1 2 3))
;Value: (((() 1) 2) 3)

;;; op should satisfy:
;;; - Commutative property, and
;;; - Associative property

(fold-right + 0 (list 1 2 3))
;Value: 6
(fold-left + 0 (list 1 2 3))
;Value: 6

(fold-right * 1 (list 1 2 3))
;Value: 6
(fold-left * 1 (list 1 2 3))
;Value: 6
