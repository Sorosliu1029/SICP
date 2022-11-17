;;; Soros Liu
;;; Exercise 2.35
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.35

(define (count-leaves t)
  (accumulate + 0 (map (lambda (i) 1) (enumerate-tree t))))

(define nil ())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define x (cons (list 1 2) (list 3 4)))

(count-leaves x)
;Value: 4