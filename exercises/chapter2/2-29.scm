;;; Soros Liu
;;; Exercise 2.29
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; part a
(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)

;; part b
(define (branch-weight branch)
  (let* ((b-structure (branch-structure branch))
         (is-branch-mobile (pair? b-structure)))
    (if is-branch-mobile (total-weight b-structure) b-structure)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define m1 (make-mobile (make-branch 1 1)
                        (make-branch 1 2)))
(total-weight m1)
;Value: 3

;; part c
(define (branch-balanced? branch)
  (let* ((b-structure (branch-structure branch))
         (is-branch-mobile (pair? b-structure)))
    (if is-branch-mobile (balanced? b-structure) true)))

(define (balanced? mobile)
  (let ((l (left-branch mobile))
        (r (right-branch mobile)))
    (and (branch-balanced? l)
         (branch-balanced? r)
         (= (* (branch-length l) (branch-weight l))
            (* (branch-length r) (branch-weight r))))))

(balanced? m1)
;Value: #f

(define m2 (make-mobile (make-branch 1 2)
                        (make-branch 2 1)))
(balanced? m2)
;Value: #t

(define m3 (make-mobile (make-branch 10 m1)
                        (make-branch 10 m2)))
(balanced? m3)
;Value: #f

(define m4 (make-mobile (make-branch 2 m2)
                        (make-branch 1 (make-mobile (make-branch 1 m2)
                                                    (make-branch 1 m2)))))
(balanced? m4)
;Value: #t

;; part d
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;;; changes that need to make
(define right-branch cdr)
(define branch-structure cdr)

;;; verify
(define m1 (make-mobile (make-branch 1 1)
                        (make-branch 1 2)))
(total-weight m1)
;Value: 3

(balanced? m1)
;Value: #f

(define m2 (make-mobile (make-branch 1 2)
                        (make-branch 2 1)))
(balanced? m2)
;Value: #t

(define m3 (make-mobile (make-branch 10 m1)
                        (make-branch 10 m2)))
(balanced? m3)
;Value: #f

(define m4 (make-mobile (make-branch 2 m2)
                        (make-branch 1 (make-mobile (make-branch 1 m2)
                                                    (make-branch 1 m2)))))
(balanced? m4)
;Value: #t
