;;; Soros Liu
;;; Exercise 2.33
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.33

(define nil ())

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (1+ y)) 0 sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define seq1 (list 1 2 3 4))
(define seq2 (list 5 6 7 8))

(map square seq1)
;Value: (1 4 9 16)
(append seq1 seq2)
;Value: (1 2 3 4 5 6 7 8)
(length seq1)
;Value: 4