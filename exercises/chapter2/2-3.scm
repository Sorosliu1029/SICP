;;; Soros Liu
;;; Exercise 2.3
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.3

(define (make-segment sp ep) (cons sp ep))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
(define (length seg)
  (let ((sp (start-segment seg))
	(ep (end-segment seg)))
    (sqrt (+ (square (- (x-point sp) (x-point ep)))
	     (square (- (y-point sp) (y-point ep)))))))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;;; represent with two adjacent segments

(define (make-rect s1 s2) (cons s1 s2))

(define (perimeter rect) (* 2 (+ (length (car rect)) (length (cdr rect)))))

(define (area rect) (* (length (car rect)) (length (cdr rect))))

(define p1 (make-point 0 0))
(define p2 (make-point 0 3))
(define p3 (make-point 4 3))
(define p4 (make-point 4 0))

(define rect (make-rect (make-segment p1 p2)
			(make-segment p1 p4)))

(perimeter rect)
;Value: 14

(area rect)
;Value: 12

;;; represent with three points

(define (make-rect p1 p2 p3)
  (list p1 p2 p3))

(define (perimeter rect)
  (let ((p1 (first rect))
	(p2 (second rect))
	(p3 (third rect))
	(len1 (length (make-segment p1 p2)))
	(len2 (length (make-segment p1 p3)))
	(len3 (length (make-segment p2 p3))))
    (cond ((and (< len2 len1) (< len3 len1))
	   (* 2 (+ len2 len3)))
	  ((and (< len1 len2) (< len3 len2))
	   (* 2 (+ len1 len3)))
	  (else
	   (* 2 (+ len1 len2))))))

(define (area rect)
  (let ((p1 (first rect))
	(p2 (second rect))
	(p3 (third rect))
	(len1 (length (make-segment p1 p2)))
	(len2 (length (make-segment p1 p3)))
	(len3 (length (make-segment p2 p3))))
    (cond ((and (< len2 len1) (< len3 len1))
	   (* len2 len3))
	  ((and (< len1 len2) (< len3 len2))
	   (* len1 len3))
	  (else
	   (* len1 len2)))))

(define rect (make-rect p1 p2 p3))

(perimeter rect)
;Value: 14

(area rect)
;Value: 12