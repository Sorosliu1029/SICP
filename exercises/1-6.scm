;;; Soros Liu
;;; Exercise 1.6
;;; https://sicp.sorosliu.xyz/book/book-Z-H-10.html#%_thm_1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)
;Value: 5

(new-if (= 1 1) 0 5)
;Value: 0

;;; use if
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) .001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 2)
;Value: 1.4142156862745097

;;; use new-if
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(define (new-sqrt x)
  (sqrt-iter 1.0 x))

(new-sqrt 2)
;Aborting!: maximum recursion depth exceeded

;;; Explanation:
;;; To evaluate an if expression, the interpreter starts by evaluating the <predicate> part of the expression.
;;; If the <predicate> evaluates to a true value, the interpreter then evaluates the <consequent>
;;; and returns its value.
;;; Otherwise it evaluates the <alternative> and returns its value.
;;; WHILE with new-if, it is a procedure, and according to adaptive-order evaluation, all arguments will be evaluated first,
;;; so the expression reduces into a recursive `sqrt-iter` sub-expression, and continues this recursion,
;;; which results into a situation that `maximum recursion depth exceeded`