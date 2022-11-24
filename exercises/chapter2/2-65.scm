;;; Soros Liu
;;; Exercise 2.65
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.65

(define (union-set set1 set2)
  (define (union-list list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          (else
            (let ((x1 (car list1)) (x2 (car list2)))
              (cond ((= x1 x2)
                     (cons x1 (union-list (cdr list1) (cdr list2))))
                    ((< x1 x2)
                     (cons x1 (union-list (cdr list1) list2)))
                    ((< x2 x1)
                     (cons x2 (union-list list1 (cdr list2)))))))))
  (list->tree (union-list (tree->list set1)
                          (tree->list set2))))

(define (intersection-set set1 set2)
  (define (intersection-list list1 list2)
    (if (or (null? list1) (null? list2))
        '()
        (let ((x1 (car list1)) (x2 (car list2)))
          (cond ((= x1 x2)
                 (cons x1
                       (intersection-list (cdr list1)
                                         (cdr list2))))
                ((< x1 x2)
                 (intersection-list (cdr list1) list2))
                ((< x2 x1)
                 (intersection-list list1 (cdr list2)))))))
  (list->tree (intersection-list (tree->list set1)
                                 (tree->list set2))))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
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
  (car (partial-tree elements (length elements))))

;;; verify
(define t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define t2 '(4 (2 () ()) (6 () ())))
(define t3 '())
(define t4 '(2 (1 () ()) (3 () (4 () ()))))

(union-set t1 t1)
;Value: (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

(union-set t1 t2)
;Value: (5 (2 (1 () ()) (3 () (4 () ()))) (7 (6 () ()) (9 () (11 () ()))))

(union-set t1 t3)
;Value: (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

(union-set t1 t4)
;Value: (4 (2 (1 () ()) (3 () ())) (7 (5 () ()) (9 () (11 () ()))))

(intersection-set t1 t1)
;Value: (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

(intersection-set t1 t2)
;Value: ()

(intersection-set t1 t3)
;Value: ()

(intersection-set t1 t4)
;Value: (1 () (3 () ()))

;;;
; all of list->tree, tree->list, union-list, intersection-list are of O(n),
; so union-set and intersection-set are of O(n)