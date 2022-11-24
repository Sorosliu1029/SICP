;;; Soros Liu
;;; Exercise 2.64
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (make-tree entry left right)
  (list entry left right))

;;; a
; partial-tree recursively
; - take left half of n elts to construct a balanced left branch
; - take the first of remaining elts as entry
; - take right half of n elts to construct a balanced right branch
; - construct a tree with entry, left branch and right branch

(list->tree '(1 3 5 7 9 11))
;Value: (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

;     5
;    / \
;   /   \
;  1     9
;   \   / \
;    3 7   11

;;; b
; T(n) = T(n/2) + T(n/2)
; so T(n) = O(n)