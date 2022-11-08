;;; Soros Liu
;;; Exercise 1.34
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.34

(define (f g)
  (g 2))

(f square)
;Value: 4

(f (lambda (z) (* z (+ z 1))))
;Value: 6

(f f)
;The object 2 is not applicable.

;;; (f f) results an error
(f f)
;;; =>
(f 2)
;;; =>
(2 2)
;;; => error: object 2 is not applicable