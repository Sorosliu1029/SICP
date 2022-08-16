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

;;; This is the file RSA.SCM
;;; It contains the code listed at the end of Problem Set 2

;;; Fall '87 - Written by Ruth Shyu
;;; 6/11/89 - Cleaned up by Lyn 

;;; This is the answer to Ex. 1.11 for you to work on Ex. 1.13

(define (new-exp b n)
  (define (square x) (* x x))
  (define (iter-exp multiplier product exponent)
    (cond ((= exponent 0) product)
          ((even? exponent)
           (iter-exp (square multiplier) product (/ exponent 2)))
          (else (iter-exp multiplier
                          (* product multiplier)
                          (- exponent 1)))))
  (iter-exp b 1 n))

;;; Procedures needed to implement fermat's test of primality

(define square (lambda (x) (* x x)))

(define big-random
  (lambda (n)
    (random (min n (expt 10 10)))))

(define fermat-test
  (lambda (n)
    (let ((a (big-random n)))
      (= (expmod-3 a n n) a))))

(define fast-prime?
  (lambda (n times)
    (cond ((= times 0) #t)
	  ((fermat-test n) (fast-prime? n (- times 1)))
	  (else #f))))

;;; Note - This primality test is probabilistic and can be fooled
;;; in rare cases
(define prime?
  (lambda (n) (fast-prime? n 2)))

;;; Louis Reasoner's 3 versions of expmod

(define expmod-1
  (lambda (b e m)
    (cond ((= e 0) 1)
          (else (remainder (* b
                              (expmod-1 b (-1+ e) m))
                           m)))))

(define expmod-2
  (lambda (b e m)
    (define exp-helper
      (lambda (times)
        (cond ((= times 0) 1)
              ((even? times)
               (square (exp-helper (/ times 2))))
	      (else (* b (exp-helper (-1+ times)))))))
    (remainder (exp-helper e) m)))

(define expmod-3
  (lambda (b e m)
    (cond ((= e 0) 1)
          ((even? e)
           (remainder (square (expmod-3 b (/ e 2) m))
                      m))
          (else
           (remainder (* b (expmod-3 b (-1+ e) m))
                      m)))))

(define timed-exp
  (lambda (f b e m)
    (let ((start-time (runtime)))
      (let ((result (f b e m)))
	(print-info "Time taken: " (- (runtime) start-time) " seconds.")
	result))))  ; Return result after printing timing info 


;;; CIA's key selection procedures

;;; SELECT-PRIME randomly generates a prime via the formula
;;; N^2 + N + 41.  This formula happens to be prime 
;;; for all 0 <= N < 40.  The test for PRIME?
;;; is included for cases where the upper bound on 
;;; N is raised to 40 or above.

(define select-prime
  (lambda ()
    (let ((n (random-between 2 20)))
      (let ((p (+ (square n) n 41)))
	(if (prime? p)
	    p
	    (select-prime))))))

(define select-keys
  (lambda ()
    (let ((start-time (runtime)))
      (let ((p (select-prime))
	    (q (select-prime)))
	(if (= p q)       
	    (select-keys) ; RSA algorithm won't work if p = q, so try again
	    (let ((n (* p q))
		  (m (* (- p 1) (- q 1))))
	      (let ((e (select-e m)))
		(print-info "Your first public key (n) is: " n "")
		(print-info "Your second public key (e) is: " e "")
		(print-info "The elapsed time is: "
			    (- (runtime) start-time)
			    " seconds."))))))))

(define select-e
  (lambda (m)
    (let ((e (random 1000)))
      (if (= (gcd e m) 1)
          e
          (select-e m)))))

(define gcd
  (lambda (a b)
    (if (= b 0)
        a
        (gcd b (remainder a b)))))


;;; Encryption and Decryption procedures (really simple)

(define encrypt
  (lambda (message e n)
    (expmod-3 message e n)))

(define decrypt
  (lambda (message d n)
    (expmod-3 message d n)))


;;; Utilities

;;; Generates a random number between lower bound and upper bound (inclusive)
(define (random-between lower-bound upper-bound)
  (+ lower-bound
     (random (1+ (- upper-bound lower-bound)))))

(define (print-info before-string object after-string)
  (newline)
  (princ before-string)
  (princ object)
  (princ after-string))

