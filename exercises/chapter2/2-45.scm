;;; Soros Liu
;;; Exercise 2.45
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.45

(define (split outter-op inner-op)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller ((split outter-op inner-op) painter (-1+ n))))
        (outter-op painter (inner-op smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))
