;;; Soros Liu
;;; Exercise 2.24
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.24

(list 1 (list 2 (list 3 4)))
;Value: (1 (2 (3 4)))

;;; box-and-pointer structure
; (1 (2 (3 4))) -> |*|*|->|*|/|
;                   |      |
;                   1      ->|*|*|->|*|/|
;                             |      |
;                             2      ->|*|*|->|*|/|
;                                       |      |
;                                       3      4

;;; tree structure
; (1 (2 (3 4)))
;    /    \
;   /      \
;  1     (2 (3 4))
;          /  \
;         /    \
;        2    (3 4)
;              /  \
;             /    \
;            3      4