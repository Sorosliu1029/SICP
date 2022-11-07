;;; Soros Liu
;;; Exercise 1.33
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.33

(define (filtered-accumulate combiner null-value term a next b filter?)
  (cond ((> a b) null-value)
        ((filter? a) (combiner (term a)
                               (filtered-accumulate combiner null-value term (next a) next b filter?)))
        (else (filtered-accumulate combiner null-value term (next a) next b filter?))))

;;; a

(define (fa a b)
  (filtered-accumulate
   +
   0
   square
   a
   1+
   b
   prime?))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

;;; verify

(fa 2 10)
;Value: 87

;;; b

(define (fb n)
  (filtered-accumulate
   *
   1
   identity
   1
   1+
   (-1+ n)
   (lambda (i) (= 1 (gcd i n)))))

(define (identity n) n)
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;; verify

(fb 10)
;Value: 189