;;; Soros Liu
;;; Exercise 2.37
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mr)
           (accumulate (lambda (col acc)
                         (cons (dot-product mr col) acc))
                       nil
                       cols))
         m)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define nil ())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;; verify
(define v (list 1 2 3 4))
(define w (list 6 7 8 9))

(dot-product v w)
;Value: 80

(define m (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))

(matrix-*-vector m v)
;Value: (30 56 80)

(transpose m)
;Value: ((1 4 6) (2 5 7) (3 6 8) (4 6 9))

(matrix-*-matrix m (transpose m))
;Value: ((30 56 80) (56 113 161) (80 161 230))
