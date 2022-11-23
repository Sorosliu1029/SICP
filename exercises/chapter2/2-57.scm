;;; Soros Liu
;;; Exercise 2.57
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.57

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (make-sum a1 . as)
  (cond ((null? as) a1)
        ((=number? a1 0) (apply make-sum (car as) (cdr as)))
        ((and (number? a1) (number? (car as)))
         (apply make-sum (+ a1 (car as)) (cdr as)))
        (else 
          (let ((sum-as (apply make-sum (car as) (cdr as))))
            (if (=number? sum-as 0)
              a1
              (list '+ a1 sum-as))))))

(define (addend s) (cadr s))

(define (augend s) 
  (let ((as (cddr s)))
    (apply make-sum (car as) (cdr as))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (make-product m1 . ms)
  (cond ((=number? m1 0) 0)
        ((null? ms) m1)
        ((=number? m1 1) (apply make-product (car ms) (cdr ms)))
        ((and (number? m1) (number? (car ms)))
         (apply make-product (* m1 (car ms)) (cdr ms)))
        (else 
          (let ((product-ms (apply make-product (car ms) (cdr ms))))
            (cond ((=number? product-ms 0) 0)
                  ((=number? product-ms 1) m1)
                  (else (list '* m1 product-ms)))))))

(define (multiplier p) (cadr p))

(define (multiplicand p) 
  (let ((ms (cddr p)))
    (apply make-product (car ms) (cdr ms))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;;; verify
(make-sum 0 1 'x)
;Value: (+ 1 x)

(make-sum 1 'x 0)
;Value: (+ 1 x)

(make-sum 'x 0 1)
;Value: (+ x 1)

(make-sum 'x 0 0)
;Value: x

(deriv '(+ x 1) 'x)
;Value: 1

(deriv '(+ x y z) 'x)
;Value: 1

(deriv '(+ a b c d 0) 'x)
;Value: 0

(make-product 'x 0 1)
;Value: 0

(make-product 'x 1 1)
;Value: x

(make-product 1 'x 0)
;Value: 0

(make-product 'x 2 3)
;Value: (* x 6)

(make-product 2 3 'x)
;Value: (* 6 x)

(make-product 2 'x 3)
;Value: (* 2 (* x 3))

(deriv '(* x 1) 'x)
;Value: 1

(deriv '(* x y (+ x 3)) 'x)
;Value: (+ (* x y) (* y (+ x 3)))