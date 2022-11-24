;;; Soros Liu
;;; Exercise 2.63
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.63

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

;;; a
; same result
; left-branch first, then entry, then right-branch

(define t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
(define t4 '(1 () (2 () (3 () (4 () (5 () (6 () (7 () ()))))))))

(tree->list-1 t1)
;Value: (1 3 5 7 9 11)

(tree->list-1 t2)
;Value: (1 3 5 7 9 11)

(tree->list-1 t3)
;Value: (1 3 5 7 9 11)

(tree->list-1 t4)
;Value: (1 2 3 4 5 6 7)

(tree->list-2 t1)
;Value: (1 3 5 7 9 11)

(tree->list-2 t2)
;Value: (1 3 5 7 9 11)

(tree->list-2 t3)
;Value: (1 3 5 7 9 11)

(tree->list-2 t4)
;Value: (1 2 3 4 5 6 7)

;;; b
; tree->list-1:
; T(n) = T(n/2) + T(n/2) + O(n/2)
; so T(n) = O(n)

; tree->list-2:
; T(n) = T(n/2) + T(n/2)
; so T(n) = O(n)

; They have the same order of growth as O(n), but tree->list-2 grows more slowly
