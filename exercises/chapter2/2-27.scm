;;; Soros Liu
;;; Exercise 2.27
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.27

(define x (list (list 1 2) (list 3 4)))
x
;Value: ((1 2) (3 4))

;;; reverse
(define (reverse items)
  (define (iter items acc)
    (if (null? items)
      acc
      (iter (cdr items) (cons (car items) acc))))
  (iter items ()))

(reverse x)
;Value: ((3 4) (1 2))

;;; deep-reverse
(define (deep-reverse items)
  (define (iter x acc)
    (cond ((null? x) acc)
          ((not (pair? x)) x)
          (else (iter (cdr x) (cons (deep-reverse (car x)) acc)))))
  (iter items ()))

(deep-reverse x)
;Value: ((4 3) (2 1))

(deep-reverse (list ))
;Value: ()
(deep-reverse (list 1))
;Value: (1)
(deep-reverse (list 1 2 (list 3 4)))
;Value: ((4 3) 2 1)
(deep-reverse (list 1 (list 2 (list 3 4))))
;Value: (((4 3) 2) 1)