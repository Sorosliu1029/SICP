;;; Soros Liu
;;; Exercise 2.54
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.54

(define (equal? a b)
  (or (and (not (pair? a))
           (not (pair? b))
           (eq? a b))
      (and (pair? a)
           (pair? b)
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))))

(equal? '(this is a list) '(this is a list))
;Value: #t

(equal? '(this is a list) '(this (is a) list))
;Value: #f

(equal? '() '())
;Value: #t

(equal? '() 'a)
;Value: #f

(equal? 'a 'a)
;Value: #t

(equal? 1 1)
;Value: #t