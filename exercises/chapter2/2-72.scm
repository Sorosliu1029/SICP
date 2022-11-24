;;; Soros Liu
;;; Exercise 2.72
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.72

; order of growth to encode the most frequent symbol: O(1)

; order of growth to encode the least frequent symbol: O(n^2)
; on level 1: contain? takes n, cons takes 1
; on level 2: contain? takes n-1, cons takes 1
; on level 3: contain? takes n-2, cons takes 1
; ...
; on the final leaf level: contain? takes 2, cons takes 1

; so the total T(n) = n+1 + (n-1)+1 + (n-2)+1 ... + 2+1
;                   = (n+2)(n-1)/2 + log(n)
;                   = O(n^2)
