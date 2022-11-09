;;; Soros Liu
;;; Exercise 1.40
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.40

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (zero-cubic a b c)
  (newtons-method (cubic a b c) 1.0))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(zero-cubic 1 1 1) ;;; x = -1
;Value: -.9999999999997796

(zero-cubic -1 -1 1) ;;; x = 1
;Value: 1.

(zero-cubic 2 1 2) ;;; x = -2
;Value: -1.9999999999806546