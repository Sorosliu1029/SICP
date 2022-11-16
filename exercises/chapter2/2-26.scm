;;; Soros Liu
;;; Exercise 2.26
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
;Value: (1 2 3 4 5 6)

(cons x y)
;Value: ((1 2 3) 4 5 6)

(list x y)
;Value: ((1 2 3) (4 5 6))
