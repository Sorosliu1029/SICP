;;; Soros Liu
;;; Exercise 2.44
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.44

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (-1+ n))))
      (below painter (beside smaller smaller)))))
