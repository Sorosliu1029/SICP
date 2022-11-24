;;; Soros Liu
;;; Exercise 2.61
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.61

(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;;; verify
(adjoin-set 1 '())
;Value: (1)

(adjoin-set 1 '(2 3))
;Value: (1 2 3)

(adjoin-set 1 '(0 1))
;Value: (0 1)

(adjoin-set 1 '(0 2))
;Value: (0 1 2)

(adjoin-set 2 '(0 1))
;Value: (0 1 2)
