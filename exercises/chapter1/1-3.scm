;;; Soros Liu
;;; Exercise 1.3
;;; https://sicp.sorosliu.xyz/book/book-Z-H-10.html#%_thm_1.3

(define (square x)
  (* x x))

(define (sum-of-square-of-2-larger a b c)
  (cond ((and (< a b) (< a c)) (+ (square b) (square c)))
	((and (< b a) (< b c)) (+ (square a) (square c)))
	(else (+ (square a) (square b)))))

;;; Test
(sum-of-square-of-2-larger 1 2 3)
;Value: 13

(sum-of-square-of-2-larger 2 2 1)
;Value: 8

(sum-of-square-of-2-larger 3 2 1)
;Value: 13

(sum-of-square-of-2-larger 2 2 2)
;Value: 8

(sum-of-square-of-2-larger 4 3 5)
;Value: 41