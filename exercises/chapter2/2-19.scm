;;; Soros Liu
;;; Exercise 2.19
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.19

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define first-denomination car)

(define except-first-denomination cdr)

(define no-more? null?)

(cc 100 us-coins)
;Value: 292

(cc 100 (list 25 10 1 5 50))
;Value: 292

;;; no, the order of coin-values does not matter
;;; since the recursive nature will take each denomination into account