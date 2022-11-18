;;; Soros Liu
;;; Exercise 2.47
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.47

;;; constructor 1
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

;;; constructor 2
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame cddr)

