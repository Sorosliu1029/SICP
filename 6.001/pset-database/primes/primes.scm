;;; Copyright (c) 1990 Massachusetts Institute of Technology
;;; 
;;; This material was developed by the Scheme project at the Massachusetts
;;; Institute of Technology, Department of Electrical Engineering and
;;; Computer Science.  Permission to copy this material, to redistribute
;;; it, and to use it for any non-commercial purpose is granted, subject
;;; to the following restrictions and understandings.
;;; 
;;; 1. Any copy made of this material must include this copyright notice
;;; in full.
;;; 
;;; 2. Users of this material agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions that
;;; they make, so that these may be included in future releases; and (b)
;;; to inform MIT of noteworthy uses of this material.
;;; 
;;; 3. All materials developed as a consequence of the use of this
;;; material shall duly acknowledge such use, in accordance with the usual
;;; standards of acknowledging credit in academic research.
;;; 
;;; 4. MIT has made no warrantee or representation that this material
;;; (including the operation of software contained therein) will be
;;; error-free, and MIT is under no obligation to provide any services, by
;;; way of maintenance, update, or otherwise.
;;; 
;;; 5. In conjunction with products arising from the use of this material,
;;; there shall be no use of the name of the Massachusetts Institute of
;;; Technology nor of any adaptation thereof in any advertising,
;;; promotional, or sales literature without prior written consent from
;;; MIT in each case. 

;;; This is the file PRIMES.SCM containing code for the 
;;; prime numbers problem set.

;;; Primality test that searches for divisors

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond 
   ((> (square test-divisor) n) n)
   ((divides? test-divisor n) test-divisor)
   (else (find-divisor n (+ test-divisor 1)))))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

;;; The Fermat test for primality

; Changed to use big-random instead of random
(define (fermat-test n)
  (let ((a (+ 2 (big-random (- n 2)))))
    (= (expmod a n n) a)))

(define (fast-prime? n times)
  (cond ((= times 0) t)
	((fermat-test n)
         (fast-prime? n (- times 1)))
        (else #f)))

(define (expmod b e m)
  (cond ((= e 0) 1)
        ((even? e)
         (remainder (square (expmod b (/ e 2) m))
                    m))
        (else
         (remainder (* b (expmod b (- e 1) m))
                    m))))        

(define (big-random n)
  (random (min n (expt 10 10))))

; From exercise 1.17 in book
; but some prints changed to princ
(define (timed-prime-test n)
  (let ((start-time (runtime)))
    (let ((found-prime? (prime? n)))
      (let ((elapsed-time (- (runtime) start-time)))
	(print n)
	(cond (found-prime?
	       (princ " *** ")
	       (princ elapsed-time)))))))
