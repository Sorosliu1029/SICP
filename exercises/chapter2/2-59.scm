;;; Soros Liu
;;; Exercise 2.59
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.59

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))

;;; verify
(union-set '(a b c) '(d e f))
;Value: (c b a d e f)

(union-set '(a b c) '(a b c))
;Value: (a b c)

(union-set '(a b c) '(c b d))
;Value: (a c b d)