;;; Soros Liu
;;; Exercise 2.17
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.17

(define (last-pair items)
  (if (null? (cdr items))
    items
    (last-pair (cdr items))))

(last-pair (list 23 72 149 34))
;Value: (34)

(last-pair (list 23))
;Value: (23)