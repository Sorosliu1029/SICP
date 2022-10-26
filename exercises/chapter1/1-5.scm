;;; Soros Liu
;;; Exercise 1.5
;;; https://sicp.sorosliu.xyz/book/book-Z-H-10.html#%_thm_1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;;; evaluate
(test 0 (p))

;;; Applicative-order evaluation: (evaluate the arguments and then apply)
;;;   expression will be in a infinite recursive loop of evaluating `(p)`
;;;   interpreter need first to evaluate both of the arguments `0` and `(p)`,
;;;   and evaluating `(p)` results into a infinite recursive loop of evaluating `(p)`

;;; Normal-order evaluation: (fully expand and then reduce)
;;;   expression will return value `0`
;;;   `(test 0 (p))` will be expanded to `(if (= x 0) 0 (p))`,
;;;   and this if expressoin will reduce to its consequent expression, which is `0`
