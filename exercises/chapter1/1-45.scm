;;; Soros Liu
;;; Exercise 1.45
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.45

(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (-1+ n)))))
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (average a b) (/ (+ a b) 2))

(define (n-fold-average-damp f n)
  ((repeated average-damp n) f))

;;; 2-th root
(define (sqrt x)
  (fixed-point (n-fold-average-damp (lambda (y) (/ x y)) 1)
               1.0))

(sqrt 2)
;Value: 1.4142135623746899

;;; 3-th root
(define (cube-root x)
  (fixed-point (n-fold-average-damp (lambda (y) (/ x (square y))) 1)
               1.0))

(cube-root 2)
;Value: 1.259923236422975
;;; 3-th root, 1 average damps

;;; 4-th root
(define (fourth-root x)
  (fixed-point (n-fold-average-damp (lambda (y) (/ x (cube y))) 2)
               1.0))

(fourth-root 2)
;Value: 1.189207115002721
;;; 4-th root, 2 average damps

(define (try-nth-root n k)
  (lambda (x) (fixed-point (n-fold-average-damp (lambda (y) (/ x (expt y (-1+ n)))) k)
			   1.0)))

((try-nth-root 5 2) 2)
;Value: 1.1486967244204176
;;; 5-th root, 2 average damps

((try-nth-root 6 2) 2)
;;; 6-th root, 2 average damps
;Value: 1.1224648393618204

((try-nth-root 7 2) 2)
;;; 7-th root, 2 average damps
;Value: 1.1040857488809648

((try-nth-root 8 3) 2)
;;; 8-th root, 3 average damps
;Value: 1.090507732665258

(define (nth-root n x)
  (define (log2 x) (/ (log x) (log 2)))
  (fixed-point (n-fold-average-damp (lambda (y) (/ x (expt y (-1+ n)))) (floor (log2 n)))
               1.0))

(nth-root 2 2)
;Value: 1.4142135623746899
(nth-root 3 2)
;Value: 1.259923236422975
(nth-root 4 2)
;Value: 1.189207115002721
(nth-root 5 2)
;Value: 1.1486967244204176
(nth-root 6 2)
;Value: 1.1224648393618204
(nth-root 7 2)
;Value: 1.1040857488809648
(nth-root 8 2)
;Value: 1.090507732665258
(nth-root 9 2)
;Value: 1.0800601441048037
(nth-root 10 2)
;Value: 1.0717742428174573
(nth-root 11 2)
;Value: 1.065039586617723
(nth-root 12 2)
;Value: 1.059461368044972
(nth-root 100 2)
;Value: 1.006958277553065