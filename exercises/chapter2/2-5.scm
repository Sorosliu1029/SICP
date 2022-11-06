;;; Soros Liu
;;; Exercise 2.5
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.5

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car n)
  (if (even? n)
      (1+ (car (/ n 2)))
      0))

(define (cdr n)
  (cond ((= n 1) 0)
	((even? n) (cdr (/ n 2)))
	(else (1+ (cdr (/ n 3))))))

(car (cons 1 2))
;Value: 1

(cdr (cons 1 2))
;Value: 2

(car (cons 41 42))
;Value: 41

(cdr (cons 41 42))
;Value: 42
