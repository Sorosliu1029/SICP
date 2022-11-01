;;; Soros Liu
;;; Exercise 1.22
;;; https://sicp.sorosliu.xyz/book/book-Z-H-11.html#%_thm_1.22

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square n) (* n n))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t)

(timed-prime-test 824633720831)
; 824633720831 *** .6799999999999997
;Value: #t

(define (timed-prime-test-short n)
  (start-prime-test n (runtime)))

(define (print-prime-and-search-next n c)
  (display " find: ")
  (display n)
  (newline)
  (search-for-primes (+ n 2) (- c 1)))

(define (search-for-primes n c)
  (cond ((= 0 c) (display " Done "))
	((timed-prime-test-short n) (print-prime-and-search-next n c))
	(else (search-for-primes (+ n 2) c))))

(search-for-primes 1001 3)
;  *** 0. find: 1009
;  *** 0. find: 1013
;  *** 0. find: 1019
;  Done 
;Unspecified return value

(search-for-primes 10001 3)
;  *** 0. find: 10007
;  *** 0. find: 10009
;  *** 0. find: 10037
;  Done 
;Unspecified return value

(search-for-primes 100001 3)
;  *** 0. find: 100003
;  *** 0. find: 100019
;  *** 0. find: 100043
;  Done 
;Unspecified return value

(search-for-primes 1000001 3)
;  *** 0. find: 1000003
;  *** 1.0000000000000009e-2 find: 1000033
;  *** 0. find: 1000037
;  Done 
;Unspecified return value

(search-for-primes 10000001 3)
;  *** 0. find: 10000019
;  *** 0. find: 10000079
;  *** 1.0000000000000009e-2 find: 10000103
;  Done 
;Unspecified return value

(search-for-primes 100000001 3)
;  *** 1.0000000000000009e-2 find: 100000007
;  *** 0. find: 100000037
;  *** 0. find: 100000039
;  Done 
;Unspecified return value

(search-for-primes 1000000001 3)
;  *** 2.0000000000000018e-2 find: 1000000007
;  *** 2.0000000000000018e-2 find: 1000000009
;  *** 1.9999999999999796e-2 find: 1000000021
;  Done 
;Unspecified return value

(search-for-primes 10000000001 3)
;  *** .07000000000000006 find: 10000000019
;  *** .07999999999999985 find: 10000000033
;  *** .07000000000000028 find: 10000000061
;  Done 
;Unspecified return value

(search-for-primes 100000000001 3)
;  *** .23999999999999977 find: 100000000003
;  *** .22999999999999998 find: 100000000019
;  *** .2400000000000002 find: 100000000057
;  Done 
;Unspecified return value

(search-for-primes 1000000000001 3)
;  *** .7400000000000002 find: 1000000000039
;  *** .8499999999999996 find: 1000000000061
;  *** .9700000000000006 find: 1000000000063
;  Done 
;Unspecified return value

(search-for-primes 10000000000001 3)
;  *** 2.4499999999999993 find: 10000000000037
;  *** 2.3400000000000007 find: 10000000000051
;  *** 2.33 find: 10000000000099
;  Done 
;Unspecified return value

(search-for-primes 100000000000001 3)
;  *** 7.369999999999999 find: 100000000000031
;  *** 7.400000000000002 find: 100000000000067
;  *** 7.43 find: 100000000000097
;  Done 
;Unspecified return value

; timing data grows at rate of sqrt(10)

; timing data is compatible with the notion that
; programs run in time proportional to the number of steps
; required for the compuation