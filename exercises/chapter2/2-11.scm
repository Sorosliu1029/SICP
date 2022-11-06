;;; Soros Liu
;;; Exercise 2.11
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.11

(define (mul-interval x y)
  (cond ((positive? x)
         (cond ((positive? y) (make-interval (* (lower-bound x) (lower-bound y))
                                             (* (upper-bound x) (upper-bound y))))
               ((negative? y) (make-interval (* (upper-bound x) (lower-bound y))
                                             (* (lower-bound x) (upper-bound y))))
               (else (make-interval (* (upper-bound x) (lower-bound y))
                                    (* (upper-bound x) (upper-bound y))))))
        ((negative? x)
         (cond ((positive? y) (make-interval (* (lower-bound x) (upper-bound y))
                                             (* (upper-bound x) (lower-bound y))))
               ((negative? y) (make-interval (* (upper-bound x) (upper-bound y))
                                             (* (lower-bound x) (lower-bound y))))
               (else (make-interval (* (lower-bound x) (upper-bound y))
                                    (* (lower-bound x) (lower-bound y))))))
        (else
	 (cond ((positive? y) (make-interval (* (lower-bound x) (upper-bound y))
					     (* (upper-bound x) (upper-bound y))))
	       ((negative? y) (make-interval (* (upper-bound x) (lower-bound y))
					     (* (lower-bound x) (lower-bound y))))
	       (else (make-interval (min (* (upper-bound x) (lower-bound y))
					 (* (lower-bound x) (upper-bound y)))
				    (max (* (upper-bound x) (upper-bound y))
					 (* (lower-bound x) (lower-bound y)))))))))

(define (positive? i)
  (> (lower-bound i) 0))

(define (negative? i)
  (< (upper-bound i) 0))

(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define x1 (make-interval 1 2))
(define x2 (make-interval -4 -3))
(define x3 (make-interval -5 6))
(define y1 (make-interval 1 2))
(define y2 (make-interval -4 -3))
(define y3 (make-interval -5 6))

;;; [1 2]*[1 2] => [1 4]
(mul-interval x1 y1)
;Value: (1 . 4)

;;; [1 2]*[-4 -3] => [-8 -3]
(mul-interval x1 y2)
;Value: (-8 . -3)

;;; [1 2]*[-5 6] => [-10 12]
(mul-interval x1 y3)
;Value: (-10 . 12)

;;; [-4 -3]*[1 2] => [-8 -3]
(mul-interval x2 y1)
;Value: (-8 . -3)

;;; [-4 -3]*[-4 -3] => [9 16]
(mul-interval x2 y2)
;Value: (9 . 16)

;;; [-4 -3]*[-5 6] => [-24 20]
(mul-interval x2 y3)
;Value: (-24 . 20)

;;; [-5 6]*[1 2] => [-10 12]
(mul-interval x3 y1)
;Value: (-10 . 12)

;;; [-5 6]*[-4 -3] => [-24 20]
(mul-interval x3 y2)
;Value: (-24 . 20)

;;; [-5 6]*[-5 6] => [-30 36]
(mul-interval x3 y3)
;Value: (-30 . 36)
