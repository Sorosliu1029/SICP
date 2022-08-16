
;;;; PSUTIL.SCM -- Utilities for use with power-series stuff.


;;; Stream utilities

;;; General stream printer

(define (print-stream s)
  (print (head s))
  (print-stream (tail s)))


;;; Prints streams with rationals floated

(define (print-stream-real s)
  (print (rat->real (head s)))
  (print-stream-real (tail s)))

(define (nth-stream s n)
  (if (= n 0)
      (head s)
      (nth-stream (tail s) (- n 1))))

(define (map-stream f s)
  (cons-stream (f (head s))
	       (map-stream f (tail s))))

(define (map-2-streams f s1 s2)
  (cons-stream (f (head s1) (head s2))
	       (map-2-streams f (tail s1) (tail s2))))


;;; List utilities

(define map2
  (lambda (f l1 l2)
    (if (or (null? l1) (null? l2))
	'()
	(cons (f (car l1) (car l2))
	      (map2 f (cdr l1) (cdr l2))))))

(define reduce
  (lambda (f l)
    (if (null? (cdr l))
	(car l)
	(reduce f
		(cons (f (car l) (cadr l))
		      (cddr l))))))
	      