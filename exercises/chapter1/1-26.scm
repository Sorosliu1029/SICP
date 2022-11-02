;;; Soros Liu
;;; Exercise 1.26
;;; https://sicp.sorosliu.xyz/book/book-Z-H-11.html#%_thm_1.26

; when using square, each recursion step only needs to compute `(expmod base (/ exp 2) m)` once,
; which cuts the problem size into half, so it is o(logn)

; while with explict multiplication, each recusion step needs to compute
; `(expmod base (/ exp 2) m)` twice, and that keeps the problem size as exact same,
; so it is o(n)
