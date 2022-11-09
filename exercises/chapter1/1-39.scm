;;; Soros Liu
;;; Exercise 1.39
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.39

(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
	0
	(/ (n i)
	   (+ (d i) (recur (1+ i))))))
  (recur 1))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1) x (- (square x))))
             (lambda (i) (-1+ (* 2 i)))
             k))

;;; tan(1) ~= 1.55740772465

(tan-cf 1.0 10)
;Value: 1.557407724654902
