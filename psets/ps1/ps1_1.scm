;;; This is the code for ps1.

;;; part 2

(- 8 9)
;Value: -1

(> 3.7 4.4)
;Value: #f

(- (if (> 3 4) 7 10) (/ 16 10))
;Value: 42/5

(define b 13)
;Value: b

13
;Value: 13

b
;Value: 13

>
;Value: #[arity-dispatched-procedure 12]

(define square (lambda (x) (* x x)))
;Value: square

square
;Value: #[compound-procedure 13 square]

(square 13)
;Value: 169

(square b)
;Value: 169

(square (square (/ b 1.3)))
;Value: 10000.

(define multiply-by-itself square)
;Value: multiply-by-itself

(multiply-by-itself b)
;Value: 169

(define a b)
;Value: a

(= a b)
;Value: #t

(if (= (* b a) (square 13))
    (< a b)
    (- a b))
;Value: #f

(cond ((>= a 2) b)
      ((< (square b) (multiply-by-itself a)) (/ 1 0))
      (else (abs (- (square a) b))))
;Value: 13

;;; part 3

(define p1
  (lambda (x y)
    (+ (p2 x y) (p3 x y))))

(define p2
  (lambda (z w)
    (* z w)))

(define p3
  (lambda (a b)
    (+ (p2 a) (p2 b))))

(p1 1 2)

;;; part 4

;;; exercise 1
(define fold
  (lambda (x y)
    (* (spindle x)
       (+ (mutilate y)
	  (spindle x)))))

(define spindle
  (lambda (w) (* w w)))

(define mutilate
  (lambda (z)
    (+ (spindle z) z)))

(fold 1 2)
;Value: 7

(pp fold)
(named-lambda (fold x y)
  (* (spindle x) (+ (mutilate y) (spindle x))))
;Unspecified return value

;;; exercise 2
(define fact
  (lambda (n)
    (if (= n 0)
	(* n (fact (- n 1))))))

(fact 5) ; should return 120
;Unspecified return value

(define fact
  (lambda (n)
    (if (= n 0)
	1
	(* n (fact (- n 1))))))

(fact 5)
;Value: 120

(fact 243)
;Value: 57651072073405564859932599378988824389544612769748785289578514753791226660795447787952561780489668440613028916503471522241703645767996810695135226278296742637606115134300787052991319431412379312540230792060250137088708811794424564833107085173464718985508999858791970609491066045711874321516918150905413944789377156315207186998055591451670633898714567745386826936678840548225648089961727875705444538167142818292862812160000000000000000000000000000000000000000000000000000000000

;;; exercise 3
(define (comb n k)
  (/ (fact n) (* (fact k) (fact (- n k)))))

(comb 243 90)
;Value: 193404342391239489855973693417880600543891038618846567058277413638164

;;; exercise 4
COPYING
-------

This tutorial descends from a long line of Emacs tutorials
starting with the one written by Stuart Cracraft for the original Emacs.

This version of the tutorial, like GNU Emacs, is copyrighted, and
comes with permission to distribute copies on certain conditions:

Copyright (c) 1985 Free Software Foundation

   Permission is granted to anyone to make or distribute verbatim copies
   of this document as received, in any medium, provided that the
   copyright notice and permission notice are preserved,
   and that the distributor grants the recipient permission
   for further redistribution as permitted by this notice.

   Permission is granted to distribute modified versions
   of this document, or of portions of it,
   under the above conditions, provided also that they
   carry prominent notices stating who last altered them.

The conditions for copying Emacs itself are slightly different
but in the same spirit.  Please read the file COPYING and then
do give copies of GNU Emacs to your friends.
Help stamp out software obstructionism ("ownership") by using,
writing, and sharing free software!

;;; exercise 5
;;; unavailable under MIT-Scheme v11.2

;;; exercise 6
;;; unavailable under MIT-Scheme v11.2

;;; exercise 7
;;; unavailable under MIT-Scheme v11.2

;;; exercise 8
(define test-procedure
  (lambda (a b)
    (cond ((>= a 2) b)
	  ((< (square b)
	      (multiply-by-itself a))
	   (/ 1 0))
	  (else (abs (- (square a) b))))))

;;; commnd is `indent-sexp`
;;; description of `indent-sexp`:
;;;   `Indent each line of the expression starting just after the point.`
;;; steps to find it:
;;;   1. `C-h f` to find function for identation
;;;   2. this function `indent-sexp` does exact this thing
;;;   3. `M-x indent-sexp` to do it

;;; exercise 9
Sun Nov 13 04:16:58 PM CST 2022

;;; exercise 10
;;; `finger` program is unavailable

;;; exercise 11
(define foo1 
  (lambda (x)
    (* x x)))

(foo1 (sqrt 3))
;Value: 2.9999999999999996

(define foo2 
  (lambda (x y)
    (/ x y)))

(foo2 6 2)
;Value: 3

(define foo3
  (lambda (x)
    (lambda (y) 
      (/ x y))))

((foo3 6) 2)
;Value: 3

(define foo4 
  (lambda (x)
    (x 3))) 

(foo4 (lambda (x) x))
;Value: 3

(define foo5 
  (lambda (x)
    (cond ((= x 2)
	   (lambda () x))
	  (else
	   (lambda () (* x 3))))))

((foo5 1))
;Value: 3

(define foo6
  (lambda (x)
    (x (lambda (y) (y y)))))

(foo6 (lambda (x) 3))
;Value: 3
