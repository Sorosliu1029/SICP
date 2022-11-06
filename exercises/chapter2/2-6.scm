;;; Soros Liu
;;; Exercise 2.6
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
;;; =>
(add-1 (lambda (f) (lambda (x) x)))
;;; =>
(lambda (f) (lambda (x) (f (
			    ((lambda (f) (lambda (x) x)) f)
			    x))))
;;; =>
(lambda (f) (lambda (x) (f 
			 ((lambda (x) x) x)
			 )))
;;; =>
(lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

(add-1 one)
;;; =>
(add-1 (lambda (f) (lambda (x) (f x))))
;;; =>
(lambda (f) (lambda (x) (f (
			    ((lambda (f) (lambda (x) (f x))) f)
			    x))))
;;; =>
(lambda (f) (lambda (x) (f 
			 ((lambda (x) (f x)) x)
			 )))
;;; =>
(lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (+ n1 n2)
  (lambda (f) (lambda (x) ((n1 f) ((n2 f) x)))))

;;; Verify

;;; 0+0 => 0
(+ zero zero)
;;; =>
(lambda (f) (lambda (x) (
			 ((lambda (f) (lambda (x) x)) f) 
			 (((lambda (f) (lambda (x) x)) f) x))))
;;; =>
(lambda (f) (lambda (x) (
			 (lambda (x) x)
			 ((lambda (x) x) x))))
;;; =>
(lambda (f) (lambda (x) (
			 (lambda (x) x) x)))
;;; =>
(lambda (f) (lambda (x) x))
;;; =>
zero

;;; 0+1 => 1
(+ zero one)
;;; =>
(lambda (f) (lambda (x) (
			 ((lambda (f) (lambda (x) x)) f)
			 (((lambda (f) (lambda (x) (f x))) f) x))))
;;; =>
(lambda (f) (lambda (x) (
			 (lambda (x) x)
			 ((lambda (x) (f x)) x))))
;;; =>
(lambda (f) (lambda (x) ((lambda (x) x) (f x))))
;;; =>
(lambda (f) (lambda (x) (f x)))
;;; =>
one

;;; 1+0 => 1
(+ one zero)
;;; =>
(lambda (f) (lambda (x) (
			 ((lambda (f) (lambda (x) (f x))) f)
			 (((lambda (f) (lambda (x) x)) f) x))))
;;; =>
(lambda (f) (lambda (x) (
			 (lambda (x) (f x))
			 ((lambda (x) x) x))))
;;; =>
(lambda (f) (lambda (x) ((lambda (x) (f x)) ((lambda (x) x) x))))
;;; =>
(lambda (f) (lambda (x) ((lambda (x) (f x)) x)))
;;; =>
(lambda (f) (lambda (x) (f x)))
;;; =>
one

;;; 1+1 => 2
(+ one one)
;;; =>
(lambda (f) (lambda (x) (
			 ((lambda (f) (lambda (x) (f x))) f)
			 (((lambda (f) (lambda (x) (f x))) f) x))))
;;; =>
(lambda (f) (lambda (x) (
			 (lambda (x) (f x))
			 ((lambda (x) (f x)) x))))
;;; =>
(lambda (f) (lambda (x) ((lambda (x) (f x)) (f x))))
;;; =>
(lambda (f) (lambda (x) (f (f x))))
;;; =>
two

;;; 0+2 => 2
(+ zero two)
;;; =>
(lambda (f) (lambda (x) (
			 ((lambda (f) (lambda (x) x)) f)
			 (((lambda (f) (lambda (x) (f (f x)))) f) x))))
;;; =>
(lambda (f) (lambda (x) (
			 (lambda (x) x)
			 ((lambda (x) (f (f x))) x))))
;;; =>
(lambda (f) (lambda (x) ((lambda (x) x) (f (f x)))))
;;; =>
(lambda (f) (lambda (x) (f (f x))))
;;; =>
two