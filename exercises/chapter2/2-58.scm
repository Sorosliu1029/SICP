;;; Soros Liu
;;; Exercise 2.58
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.58

;;; a
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
  (and (pair? x) (eq? (second x) '+)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (addend s) (first s))

(define (augend s) (third s))

(define (product? x)
  (and (pair? x) (eq? (second x) '*)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (multiplier p) (first p))

(define (multiplicand p) (third p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;;; verify
(deriv '(x + (3 * (x + (y + 2)))) 'x)
;Value: 4

(deriv '(x + 3) 'x)
;Value: 1

(deriv '(x * y) 'x)
;Value: y

(deriv '((x * y) * (x + 3)) 'x)
;Value: ((x * y) + (y * (x + 3)))

;;; b
(define (sum? x)
  (and (pair? x) 
       (eq? (find (lambda (i) (eq? i '+)) x) '+)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (addend s) 
  (define (recur s)
    (if (or (null? s) (eq? (car s) '+)) 
      '() 
      (cons (car s) (recur (cdr s)))))
  (eval-exp (recur s)))

(define (augend s) 
  (eval-exp (cdr (memq '+ s))))

(define (product? x)
  (and (pair? x)
       (eq? (find (lambda (i) (eq? i '*)) x) '*)
       (not (eq? (find (lambda (i) (eq? i '+)) x) '+))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (multiplier p) 
  (define (recur p)
    (if (or (null? p) (eq? (car p) '*))
      '()
      (cons (car p) (recur (cdr p)))))
  (eval-exp (recur p)))

(define (multiplicand p) 
  (eval-exp (cdr (memq '* p))))

(define (eval-exp exp)
  (cond ((number? exp) exp)
        ((variable? exp) exp)
        ((= (length exp) 1) (eval-exp (car exp)))
        ((sum? exp) (make-sum (addend exp)
                              (augend exp)))
        ((product? exp) (make-product (multiplier exp)
                                      (multiplicand exp)))
        (else
          (error "unknown expression type -- EVAL" exp))))

;;; verify
(deriv '(x + 3 * (x + y + 2)) 'x)
;Value: 4

(deriv '(x + 3) 'x)
;Value: 1

(deriv '(x * y) 'x)
;Value: y

(deriv '(x * y * (x + 3)) 'x)
;Value: ((x * y) + (y * (x + 3)))

(deriv '(x * y * x + 3) 'x)
;Value: ((x * y) + (y * x))

(deriv '(x * x * x) 'x)
;Value: ((x * (x + x)) + (x * x))