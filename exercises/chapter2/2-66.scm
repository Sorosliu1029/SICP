;;; Soros Liu
;;; Exercise 2.66
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.66

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (entry set-of-records))) (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))))
