;;; Soros Liu
;;; Exercise 2.1
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.1

(define (make-rat n d)
  (if (boolean=? (positive? n) (positive? d))
      (cons (abs n) (abs d))
      (cons (- (abs n)) (abs d))))

(define (positive? n) (> n 0))

(make-rat 1 2)
;Value: (1 . 2)

(make-rat -2 -3)
;Value: (2 . 3)

(make-rat -3 4)
;Value: (-3 . 4)

(make-rat 4 -5)
;Value: (-4 . 5)
  
