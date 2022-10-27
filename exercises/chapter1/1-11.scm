;;; Soros Liu
;;; Exercise 1.11
;;; https://sicp.sorosliu.xyz/book/book-Z-H-11.html#%_thm_1.11

;;; recursive process
(define (f n)
  (if (< n 3)
      n
      (+
       (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(f 1)
;Value: 1

(f 2)
;Value: 2

(f 3)
;Value: 4

(f 4)
;Value: 11

(f 10)
;Value: 1892

;;; iterative process
(define (f n)
  (define (f-iter i fn-1 fn-2 fn-3)
    (if (> i n)
	fn-1
	(f-iter
	 (+ i 1)
	 (+
	  fn-1
	  (* 2 fn-2)
	  (* 3 fn-3))
	 fn-1
	 fn-2)))
  (if (< n 3) n (f-iter 3 2 1 0)))

(f 1)
;Value: 1

(f 2)
;Value: 2

(f 3)
;Value: 4

(f 4)
;Value: 11

(f 10)
;Value: 1892






























