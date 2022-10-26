;;; Soros Liu
;;; Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;;; Behavior: the value of a plus the absolute value of b

;;; Test
(a-plus-abs-b 0 0)
;Value: 0

(a-plus-abs-b 1 2)
;Value: 3

(a-plus-abs-b 1 -1)
;Value: 2