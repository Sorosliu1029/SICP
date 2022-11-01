;;; Soros Liu
;;; Exercise 1.15
;;; https://sicp.sorosliu.xyz/book/book-Z-H-11.html#%_thm_1.15

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15)
;Value: -.39980345741334

(define (sine-with-count-p angle count)
  (define (display-return count angle)
    (display "; p called ") (display count) (display " times")
    angle)

  (if (not (> (abs angle) 0.1))
      (display-return count angle)
      (p (sine-with-count-p (/ angle 3.0) (+ count 1)))))

(sine-with-count-p 12.15 0)
; p called 5 times
;Value: -.39980345741334

; 5 times that procedure p applied when (sine 12.15) is evaluated

; space: O(a)
; number of steps: O(a)
