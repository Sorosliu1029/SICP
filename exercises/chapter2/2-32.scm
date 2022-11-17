;;; Soros Liu
;;; Exercise 2.32
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.32

(define nil ())

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (set) (cons (car s) set)) rest)))))

(subsets (list 1 2 3))
;Value: (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;;; types:
;;; set = List[Sch-Num]
;;; set of all subsets = List[List[Sch-Num]]
;;; so
;;; subsets: List[Sch-Num] -> List[List[Sch-Num]]
;;; rest: List[List[Sch-Num]]
;;; `append` operator should accept the second argument of type: List[List[Sch-Num]]
;;; so `map`'s proc argument should be of type: List[Sch-Num] -> List[Sch-Num]
;;;
;;; on the other hand, divide this problem with recursion strategy:
;;; - base case: empty set, subsets should be a list of empty list
;;; - otherwise: take the first element out of the set, get all subsets of the rest elements as `rest`
;;;              and put this first element into each set of `rest`.
;;;              The final result should be `rest` and this resulting sets
