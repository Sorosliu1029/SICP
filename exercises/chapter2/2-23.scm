;;; Soros Liu
;;; Exercise 2.23
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.23

(define (for-each proc items)
  (cond ((not (null? items)) 
         (proc (car items))
         (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
57
321
88
;Unspecified return value
