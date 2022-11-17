;;; Soros Liu
;;; Exercise 2.39
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.39

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

(define nil ())

(define l (list 1 2 3 4))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(reverse l)
;Value: (4 3 2 1)

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
(reverse l)
;Value: (4 3 2 1)
