;;; Soros Liu
;;; Exercise 2.28
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.28

(define (fringe tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

(define x (list (list 1 2) (list 3 4)))

(fringe x)
;Value: (1 2 3 4)

(fringe (list x x))
;Value: (1 2 3 4 1 2 3 4)
