;;; Soros Liu
;;; Exercise 2.36
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.36

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

(define s (list (list 1 2 3)
                (list 4 5 6)
                (list 7 8 9)
                (list 10 11 12)))

(accumulate-n + 0 s)
;Value: (22 26 30)