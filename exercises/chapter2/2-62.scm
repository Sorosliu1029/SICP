;;; Soros Liu
;;; Exercise 2.62
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set (cdr set1) set2)))
                  ((< x2 x1)
                   (cons x2 (union-set set1 (cdr set2)))))))))

;;; verify
(union-set '(0 1 2) '(0 1 2))
;Value: (0 1 2)

(union-set '(0 1 2) '(1 2 3))
;Value: (0 1 2 3)

(union-set '(0 1 2) '(2 3 4))
;Value: (0 1 2 3 4)

(union-set '(0 1 2) '(3 4 5))
;Value: (0 1 2 3 4 5)
