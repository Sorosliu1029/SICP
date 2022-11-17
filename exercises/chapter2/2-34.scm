;;; Soros Liu
;;; Exercise 2.34
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(horner-eval 2 (list 1 3 0 5 0 1))
;Value: 79