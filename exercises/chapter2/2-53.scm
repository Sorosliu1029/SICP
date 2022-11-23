;;; Soros Liu
;;; Exercise 2.53
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.53

(list 'a 'b 'c)
;Value: (a b c)

(list (list 'george))
;Value: ((george))

(cdr '((x1 x2) (y1 y2)))
;Value: ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;Value: (y1 y2)

(pair? (car '(a short list)))
;Value: #f

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'red '((red shoes) (blue socks)))
;Value: #f

(memq 'red '(red shoes blue socks))
;Value: (red shoes blue socks)
