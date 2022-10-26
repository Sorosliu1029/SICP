;;; Soros Liu
;;; Project 0

;;; Part 1

(pwd)
;Value: #[pathname 12 "/home/soros/SICP/6.001/"]

(cd "project0")
;Value: #[pathname 13 "/home/soros/SICP/6.001/project0/"]

;;; Part 2

-37
;Value: -37

(* 3 4)
;Value: 12

(> 10 9.7)
;Value: #t

(- (if (> 3 4)
	7
	10)
   (/ 16 10))
;Value: 42/5

(* (- 25 10)
   (+ 6 3))
;Value: 135

+
;Value: #[arity-dispatched-procedure 14]

(define double (lambda (x) (* 2 x)))
;Value: double

double
;Value: #[compound-procedure 15 double]

(define c 4)
;Value: c

c
;Value: 4

(double c)
;Value: 8

c
;Value: 4

(double (double (+ c 5)))
;Value: 36

(define times-2 double)
;Value: times-2

(times-2 c)
;Value: 8

(define d c)
;Value: d

(= c d)
;Value: #t

(cond ((>= c 2) d)
      ((= c (- d 5)) (+ c d))
      (else (abs (- c d))))
;Value: 4

;;; Part 3

;;; use C-j
(define abs
  (lambda (a)
    (if (> a 0)
	a
	(- a))))

;;; use Enter key followed by the Tab key
(define abs
  (lambda (a)
    (if (> a 0)
	a
	(- a))))
