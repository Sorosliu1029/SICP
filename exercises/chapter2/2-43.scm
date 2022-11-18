;;; Soros Liu
;;; Exercise 2.43
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.43

(define accumulate fold-right)
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define empty-board (list ))
(define nil ())

(define make-position cons)
(define position-row car)
(define position-col cdr)

;;; positions type: List[Position]
(define (adjoin-position r c positions)
  (cons (make-position r c) positions))

(define (safe? c positions)
  (define (same-row? p1 p2) (= (position-row p1) (position-row p2)))
  (define (same-col? p1 p2) (= (position-col p1) (position-col p2)))
  (define (diagonal? p1 p2) (= (abs (- (position-row p1) (position-row p2)))
                               (abs (- (position-col p1) (position-col p2)))))
  (define (at-col? c p) (= c (position-col p)))
  (define (find-position-with-col c positions)
    (first (filter (lambda (p) (at-col? c p)) positions)))
  (define (remove-position-with-col c positions)
    (filter (lambda (p) (not (at-col? c p))) positions))
  (let ((k-position (find-position-with-col c positions))
        (other-positions (remove-position-with-col c positions)))
    (accumulate boolean/and
                true
                (map (lambda (p)
                       (not (or (same-row? p k-position) (same-col? p k-position) (diagonal? p k-position))))
                     other-positions))))

(define (queens-quick board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (queens-slow board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(define (time-it queens-func board-size)
  (let ((start (runtime)))
    (queens-func board-size)
    (- (runtime) start)))

(define q6 (time-it queens-quick 6))
q6
;Value: 1.9999999999999574e-2
(define s6 (time-it queens-slow 6))
s6
;Value: .8000000000000007

(define q7 (time-it queens-quick 7))
q7
;Value: .05999999999999872
(define s7 (time-it queens-slow 7))
s7
;Value: 13.810000000000002

(define q8 (time-it queens-quick 8))
q8
;Value: .28999999999999915
(define s8 (time-it queens-slow 8))
s8
;Value: 306.91

;;; big O analysis
;;; queen-cols in queens-quick:
;;; T(k) = T(k-1) + n =>
;;; T(n) is O(n^2)
;;;
;;; queen-cols in queens-slow:
;;; T(k) = T(k-1)*n + n =>
;;; T(n) is O(n^n)

;;; for n = 8, n^2=64, n^n=16777216
;;; so queens-slow will approximately take T*(n^n / n^2) = 262144*T