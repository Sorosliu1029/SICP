;;; Soros Liu
;;; Exercise 1.37
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.37

;;; a

(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
	0
	(/ (n i)
	   (+ (d i) (recur (1+ i))))))
  (recur 1))

(define (approximate k)
  (cont-frac (lambda (i) 1.0)
	     (lambda (i) 1.0)
	     k))

;;; 1/phi ~= 0.61803398875

(approximate 1)
;Value: 1.

(approximate 10)
;Value: .6179775280898876

(approximate 11)
;Value: .6180555555555556

;;; k should be at least 11

;;; b
;;; iterative process

(define (cont-frac n d k)
  (define (iter i r)
    (if (= i 0)
	r
	(iter (-1+ i) (/ (n i)
			 (+ (d i) r)))))
  (iter k 0))

(approximate 10)
;Value: .6179775280898876

(approximate 11)
;Value: .6180555555555556
