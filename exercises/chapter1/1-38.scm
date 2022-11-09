;;; Soros Liu
;;; Exercise 1.38
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.38

(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
	0
	(/ (n i)
	   (+ (d i) (recur (1+ i))))))
  (recur 1))

(define (approximate-e k)
  (+ 2
     (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (if (= (remainder i 3) 2)
		      (* 2 (/ (1+ i) 3))
		      1))
                k)))

(approximate-e 10)
;Value: 2.7182817182817183