;;; Soros Liu
;;; Exercise 2.71
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.71

;;; n=5
;      w31
;     /  \
;    /    \
;   16    w15
;        /  \
;       /    \
;      8     w7
;           /  \
;          /    \
;         4     w3
;              /  \
;             /    \
;            2      1

;;; n=10
;    w1023
;    /  \
;   /    \
; 512   w511
;       /  \
;      /    \
;    256   w255
;          /  \
;         /    \
;        128  w127
;             /  \
;            /    \
;           64   w63
;                /  \
;               /    \
;              32    w31
;                    /  \
;                   /    \
;                  16   w15
;                       /  \
;                      /    \
;                     8     w7
;                          /  \
;                         /    \
;                        4     w3
;                              / \
;                             /   \
;                            2     1

;;; for general n
; bits for most frequent symbol: 1
; bits for least frequent symbol: n-1