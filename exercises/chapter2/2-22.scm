;;; Soros Liu
;;; Exercise 2.22
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.22

(define nil ())

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

(square-list (list 1 2 3 4))
;Value: (16 9 4 1)

;;; as `answer` accumulating, it collects car of items first, then append rest in front

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list (list 1 2 3 4))
;Value: ((((() . 1) . 4) . 9) . 16)

;;; `answer` is a list, but (cons list element) does not produce a list
