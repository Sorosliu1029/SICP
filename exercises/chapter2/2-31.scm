;;; Soros Liu
;;; Exercise 2.31
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map proc sub-tree)
           (proc sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;Value: (1 (4 (9 16) 25) (36 49))

(define (scale-tree tree factor) (tree-map (lambda (x) (* x factor)) tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)
;Value: (10 (20 (30 40) 50) (60 70))
