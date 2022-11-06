;;; Soros Liu
;;; Exercise 2.10
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.10

(define (div-interval x y)
  (define (span-zero? x)
    (= (- (upper-bound x) (lower-bound x)) 0))
  (if (span-zero? y)
      (error "divied by zero-span interval" y)
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define x (make-interval 1 2))
(define y (make-interval 3 4))
(define z (make-interval 5 5))

(div-interval x y)
;Value: (.25 . .6666666666666666)

(div-interval x z)
;divied by zero-span interval (5 . 5)