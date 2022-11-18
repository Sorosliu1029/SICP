;;; Soros Liu
;;; Exercise 2.50
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.50

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (repeated rotate90 2))

(define (rotate270 painter)
  (repeated rotate90 3))
