;;; Soros Liu
;;; Exercise 2.41
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.41

(define (triple-sum-equal s n)
  (filter (lambda (p) (= s (+ (car p) (cadr p) (caddr p))))
          (unique-triples n)))

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (p) (cons i p)) (unique-pairs (-1+ i))))
           (enumerate-interval 1 n)))

(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define nil ())

(define accumulate fold-right)

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;;; verify

(unique-triples 3)
;Value: ((3 2 1))

(unique-triples 4)
;Value: ((3 2 1) (4 2 1) (4 3 1) (4 3 2))

(unique-triples 5)
;Value: ((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1) (5 3 2) (5 4 1) (5 4 2) (5 4 3))

(triple-sum-equal 10 6)
;Value: ((5 3 2) (5 4 1) (6 3 1))
