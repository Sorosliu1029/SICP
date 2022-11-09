;;; Soros Liu
;;; Exercise 1.35
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.35

;;; golden ratio phi: phi^2 = phi + 1
;;; for function f(x) = 1 + 1/x, when x = phi
;;; f(phi) = 1 + 1/phi = 1 + 1/(phi^2-1) = (phi^2) / (phi^2-1)
;;; = (phi^2) / phi = phi

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

(fixed-point (lambda (x) (1+ (/ 1 x)))
             1.0)
;Value: 1.6180327868852458
