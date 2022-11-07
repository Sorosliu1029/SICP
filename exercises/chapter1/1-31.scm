;;; Soros Liu
;;; Exercise 1.31
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.31

;;; a

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (factorial n)
  (define (identity x) x)
  (product identity 1 1+ n))

(factorial 3)
;Value: 6

(factorial 6)
;Value: 720

(define (pi-product n)
  (* 4.0 (product (lambda (i) (* (/ (-1+ i) i) (/ (1+ i) i)))
		  3
		  (lambda (a) (+ a 2))
		  n)))

(pi-product 3)
;Value: 3.5555555555555554

(pi-product 101)
;Value: 3.1570301764551676

(pi-product 1001)
;Value: 3.1431607055322663

(pi-product 100001)
;Value: 3.1416083612781764

;;; b
;;; iterative one

(define (product term a next b)
  (define (iter a p)
    (if (> a b)
	p
	(iter (next a) (* p (term a)))))
  (iter a 1))

(define (factorial n)
  (define (identity x) x)
  (product identity 1 1+ n))

(factorial 3)
;Value: 6

(factorial 6)
;Value: 720

(define (pi-product n)
  (* 4.0 (product (lambda (i) (* (/ (-1+ i) i) (/ (1+ i) i)))
		  3
		  (lambda (a) (+ a 2))
		  n)))

(pi-product 3)
;Value: 3.5555555555555554

(pi-product 101)
;Value: 3.1570301764551676

(pi-product 1001)
;Value: 3.1431607055322663

(pi-product 100001)
;Value: 3.1416083612781764
