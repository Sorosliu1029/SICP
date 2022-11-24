;;; Soros Liu
;;; Exercise 2.60
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.60

(define (element-of-set? x set)
  (cond ((null? set) false) 
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 set2))

;;; verify
(define s1 '(a b c c b))
(define s2 '(d e e f))
(define s3 '(b c d b d))

(element-of-set? 'a s1)
;Value: #t

(element-of-set? 'a s2)
;Value: #f

(element-of-set? 'e s2)
;Value: #t

(adjoin-set 'a s1)
;Value: (a a b c c b)

(adjoin-set 'a s2)
;Value: (a d e e f)

(intersection-set s1 s2)
;Value: ()

(intersection-set s1 s3)
;Value: (b c c b)

(intersection-set s3 s2)
;Value: (d d)

(union-set s1 s2)
;Value: (a b c c b d e e f)

(union-set s3 s2)
;Value: (b c d b d d e e f)

(union-set s3 s1)
;Value: (b c d b d a b c c b)

;;; efficiency
; element-of-set?
; - non-duplicate: O(n)
; - duplicate: O(n), but with a duplication factor

; adjoin-set
; - non-duplicate: O(n)
; - duplicate: O(1)

; union-set
; - non-duplicate: O(n^2)
; - duplicate: O(n)

; intersection-set
; - non-duplicate: O(n^2)
; - duplicate: O(n^2), but same as element-of-set?, with a duplication factor

; prefer duplicate one when:
; - need fast adjoin-set operation
; - need fast union-set operation