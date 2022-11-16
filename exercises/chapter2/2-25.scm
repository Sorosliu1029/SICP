;;; Soros Liu
;;; Exercise 2.25
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.25

; (1 3 (5 7) 9)
(define a (list 1 3 (list 5 7) 9))
a
;Value: (1 3 (5 7) 9)
(car (cdr (car (cdr (cdr a)))))
;Value: 7

; ((7))
(define b (list (list 7)))
b
;Value: ((7))
(car (car b))
;Value: 7

; (1 (2 (3 (4 (5 (6 7))))))
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
c
;Value: (1 (2 (3 (4 (5 (6 7))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))
;Value: 7
