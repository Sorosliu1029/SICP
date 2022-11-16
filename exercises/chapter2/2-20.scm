;;; Soros Liu
;;; Exercise 2.20
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.20

(define (same-parity a . others)
  (define (is-same-parity a b) (boolean=? (odd? a) (odd? b)))
  (define (recur items)
    (cond ((null? items) ())
          ((is-same-parity a (car items)) (cons (car items) (recur (cdr items))))
          (else (recur (cdr items)))))
  (cons a (recur others)))

(same-parity 1 2 3 4 5 6 7)
;Value: (1 3 5 7)

(same-parity 2 3 4 5 6 7)
;Value: (2 4 6)
