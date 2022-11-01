;;; Soros Liu
;;; Exercise 1.23
;;; https://sicp.sorosliu.xyz/book/book-Z-H-11.html#%_thm_1.23

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (square n) (* n n))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

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
; 824633720831 *** .43000000000000016
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
;  *** 0. find: 1000033
;  *** 0. find: 1000037
;  Done 
;Unspecified return value

(search-for-primes 10000001 3)
;  *** 0. find: 10000019
;  *** 9.999999999999787e-3 find: 10000079
;  *** 0. find: 10000103
;  Done 
;Unspecified return value

(search-for-primes 100000001 3)
;  *** 0. find: 100000007
;  *** 1.0000000000000231e-2 find: 100000037
;  *** 0. find: 100000039
;  Done 
;Unspecified return value

(search-for-primes 1000000001 3)
;  *** 2.0000000000000018e-2 find: 1000000007
;  *** 9.999999999999787e-3 find: 1000000009
;  *** 9.999999999999787e-3 find: 1000000021
;  Done 
;Unspecified return value

(search-for-primes 10000000001 3)
;  *** 4.0000000000000036e-2 find: 10000000019
;  *** .04999999999999982 find: 10000000033
;  *** .04999999999999982 find: 10000000061
;  Done 
;Unspecified return value

(search-for-primes 100000000001 3)
;  *** .14000000000000012 find: 100000000003
;  *** .13999999999999968 find: 100000000019
;  *** .13999999999999968 find: 100000000057
;  Done 
;Unspecified return value

(search-for-primes 1000000000001 3)
;  *** .45999999999999996 find: 1000000000039
;  *** .5999999999999996 find: 1000000000061
;  *** .45999999999999996 find: 1000000000063
;  Done 
;Unspecified return value

(search-for-primes 10000000000001 3)
;  *** 1.46 find: 10000000000037
;  *** 1.5699999999999994 find: 10000000000051
;  *** 1.4700000000000006 find: 10000000000099
;  Done 
;Unspecified return value

(search-for-primes 100000000000001 3)
;  *** 4.629999999999999 find: 100000000000031
;  *** 4.66 find: 100000000000067
;  *** 4.690000000000001 find: 100000000000097
;  Done 
;Unspecified return value

; NOT quite twice as fast
; observed ratio: ~1.59
; MAYBE becuase remainder operation costs DIFFERENT compuation power
; on odd number and even number
; odd number take MORE compuation power than even number