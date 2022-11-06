;;; Soros Liu
;;; Exercise 2.10
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.10

(define (div-interval x y)
  (if (span-zero? y)
      (error "divied by zero-span interval" y)
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

(define (span-zero? x)
  (= (- (upper-bound x) (lower-bound y)) 0))