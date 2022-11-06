;;; Soros Liu
;;; Exercise 2.2
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.2

(define (make-segment sp ep)
  (cons sp ep))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment seg)
  (let ((sp (start-segment seg))
	(ep (end-segment seg)))
    (make-point (average (x-point sp) (x-point ep))
		(average (y-point sp) (y-point ep)))))

(define (average a b)
  (/ (+ a b) 2.0))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define a (make-point 1 2))
(define b (make-point 3 4))
(define s (make-segment a b))

(print-point (midpoint-segment s))
; (2.,3.)
;Unspecified return value