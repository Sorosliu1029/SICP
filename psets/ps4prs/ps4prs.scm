;;; code for PS4.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The PLAY-LOOP procedure takes as its arguments two prisoner's 
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds. A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays. The procedure 
;;  returns either the symbol C (for "cooperate") or D ("defect").
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
          (else (let ((result0 (strat0 history0 history1))
                      (result1 (strat1 history1 history0)))
                  (play-loop-iter strat0 strat1 (1+ count)
                                  (extend-history result0 history0)
                                  (extend-history result1 history1)
                                  limit)))))
  (play-loop-iter strat0 strat1 0 '() '() (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following procedures are used to compute and
;; print out the players' scores at the end of an
;; iterated game.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (display "; Player 1 Score: ")
    (display (/ (car scores) number-of-games))
    (newline)
    (display "; Player 2 Score: ")
    (display (/ (cadr scores) number-of-games))
    (newline)))


(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0) (list score0 score1))
          (else (let ((game (make-game (most-recent-play history0)
                                       (most-recent-play history1))))
                  (get-scores-helper (rest-of-plays history0)
                                     (rest-of-plays history1)
                                     (+ (get-player-points 0 game) score0)
                                     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define (get-point-list game)
  (cadr (assoc game *game-association-list*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This list provides the "game matrix". It is used
;; by the scorekeeping procedures above.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *game-association-list*
  '(((c c) (3 3))
    ((c d) (0 5))
    ((d c) (5 0))
    ((d d) (1 1))))

;; Some selectors and constructors

(define make-game list)

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

;; A sampler of strategies

(define (all-defect my-history other-history)
  'd)

(define (poor-trusting-fool my-history other-history)
  'c)

(define (random-strategy my-history other-history)
  (if (= (random 2) 0) 'c 'd))

(define (go-by-majority my-history other-history)
  (define (count-instances-of symbol hist)
    (cond ((empty-history? hist) 0)
          ((eq? (most-recent-play hist) symbol)
           (1+ (count-instances-of symbol (rest-of-plays hist))))
          (else (count-instances-of symbol (rest-of-plays hist)))))
  (let ((ds (count-instances-of 'd other-history))
        (cs (count-instances-of 'c other-history)))
    (if (> ds cs) 'd 'c)))


(define (tit-for-tat my-history other-history)
  (if (empty-history? my-history)
      'c
      (most-recent-play other-history)))


;;; other possibly useful procedures

(define (get-nth-from-last-play n history)
  (list-ref history n))

(define (get-players-action player-no game)
  (list-ref game player-no))

(define (get-nth-from-last-game n my-history other-history)
  (make-game (get-nth-from-last-play n my-history)
             (get-nth-from-last-play n other-history)))

;;; problem 0
(play-loop poor-trusting-fool tit-for-tat)
; Player 1 Score: 3
; Player 2 Score: 3
;Unspecified return value

(play-loop poor-trusting-fool poor-trusting-fool)
; Player 1 Score: 3
; Player 2 Score: 3
;Unspecified return value

(play-loop poor-trusting-fool all-defect)
; Player 1 Score: 0
; Player 2 Score: 5
;Unspecified return value

(play-loop tit-for-tat tit-for-tat)
; Player 1 Score: 3
; Player 2 Score: 3
;Unspecified return value

(play-loop tit-for-tat all-defect)
; Player 1 Score: 95/96
; Player 2 Score: 25/24
;Unspecified return value

(play-loop random-strategy random-strategy)
; Player 1 Score: 245/94
; Player 2 Score: 175/94
;Unspecified return value

;;; problem 1
;;; a
; 'go-by-majority' takes O(n),
; while other strategies take O(1)

;;; b
(define (go-by-majority my-history other-history)
  (define (majority-loop cs ds hist)
    (cond ((empty-history? hist) (if (> ds cs) 'd 'c))
          ((eq? (most-recent-play hist) 'c)
           (majority-loop (1+ cs) ds (rest-of-plays hist)))
          (else
            (majority-loop cs (1+ ds) (rest-of-plays hist)))))
  (majority-loop 0 0 other-history))

; order of growth (in time) is same, but newer version can be faster by a small factor

;;; problem 2
(define (tit-for-two-tats my-history other-history)
  (let ((len (length other-history)))
    (if (or (< len 2) 
            (eq? (get-nth-from-last-play (- len 1) other-history) 'c)
            (eq? (get-nth-from-last-play (- len 2) other-history) 'c))
      'c
      'd)))

(play-loop tit-for-two-tats poor-trusting-fool)
; Player 1 Score: 3
; Player 2 Score: 3
;Unspecified return value

(play-loop tit-for-two-tats all-defect)
; Player 1 Score: 47/48
; Player 2 Score: 13/12
;Unspecified return value

(play-loop tit-for-two-tats tit-for-two-tats)
; Player 1 Score: 3
; Player 2 Score: 3
;Unspecified return value

;;; problem 3
(define (make-tit-for-n-tats n)
  (define (at-least-one-cooperate hist k)
    (if (or (zero? k) (empty-history? hist))
	false
	(or (eq? (most-recent-play hist) 'c)
	    (at-least-one-cooperate (rest-of-plays hist) (-1+ k)))))
  (lambda (my-history other-history)
    (if (or (< (length other-history) n) (at-least-one-cooperate other-history n))
	'c
	'd)))

(play-loop (make-tit-for-n-tats 1) all-defect)
; Player 1 Score: 103/104
; Player 2 Score: 27/26
;Unspecified return value

(play-loop (make-tit-for-n-tats 2) all-defect)
; Player 1 Score: 49/50
; Player 2 Score: 27/25
;Unspecified return value

;;; problem 4
;;; a 
(define (make-dual-strategy strat0 strat1 switch-point)
  (lambda (my-history other-history)
    (if (< (length my-history) switch-point)
	(strat0 my-history other-history)
	(strat1 my-history other-history))))

;;; b
(define (make-triple-strategy strat0 strat1 strat2 switch-point1 switch-point2)
  (make-dual-strategy strat0
                      (make-dual-strategy strat1 strat2 (- switch-point2 switch-point1))
                      switch-point1))

;;; problem 5
(define (niceify strat niceness-factor)
  (lambda (my-history other-history)
    (let ((r (strat my-history other-history)))
      (if (eq? r 'c)
	  r
	  (if (< (random 1.0) niceness-factor)
	      'c
	      r)))))

(play-loop (niceify tit-for-tat 0) all-defect) ;;; exactly the same as strat
; Player 1 Score: 101/102
; Player 2 Score: 53/51
;Unspecified return value

(play-loop (niceify tit-for-tat 1) all-defect) ;;; same as poor-trusting-fool
; Player 1 Score: 0
; Player 2 Score: 5
;Unspecified return value

(define slightly-nice-all-defect (niceify all-defect 0.1))
(define slightly-nice-tit-for-tat (niceify tit-for-tat 0.1))

(play-loop slightly-nice-tit-for-tat
           slightly-nice-all-defect)
; Player 1 Score: 121/101
; Player 2 Score: 176/101
;Unspecified return value

;;; problem 6
(define (play-loop strat0 strat1 strat2)
  (define (play-loop-iter strat0 strat1 strat2 count history0 history1 history2 limit)
    (cond ((= count limit) (print-out-results history0 history1 history2 limit))
          (else (let ((result0 (strat0 history0 history1 history2))
                      (result1 (strat1 history1 history2 history0))
                      (result2 (strat2 history2 history0 history1)))
                  (play-loop-iter strat0 strat1 strat2 (1+ count)
                                  (extend-history result0 history0)
                                  (extend-history result1 history1)
                                  (extend-history result2 history2)
                                  limit)))))
  (play-loop-iter strat0 strat1 strat2 0 '() '() '() (+ 90 (random 21))))

(define (print-out-results history0 history1 history2 number-of-games)
  (let ((scores (get-scores history0 history1 history2)))
    (newline)
    (display "; Player 1 Score: ")
    (display (/ (first scores) number-of-games))
    (newline)
    (display "; Player 2 Score: ")
    (display (/ (second scores) number-of-games))
    (newline)
    (display "; Player 3 Score: ")
    (display (/ (third scores) number-of-games))
    (newline)))

(define (get-scores history0 history1 history2)
  (define (get-scores-helper history0 history1 history2 score0 score1 score2)
    (cond ((empty-history? history0) (list score0 score1 score2))
          (else (let ((game (make-game (most-recent-play history0)
                                       (most-recent-play history1)
                                       (most-recent-play history2))))
                  (get-scores-helper (rest-of-plays history0)
                                     (rest-of-plays history1)
                                     (rest-of-plays history2)
                                     (+ (get-player-points 0 game) score0)
                                     (+ (get-player-points 1 game) score1)
                                     (+ (get-player-points 2 game) score2))))))
  (get-scores-helper history0 history1 history2 0 0 0))

(define *game-association-list*
  '(((c c c) (7 7 7))
    ((c c d) (3 3 9))
    ((c d c) (3 9 3))
    ((d c c) (9 3 3))
    ((c d d) (0 5 5))
    ((d c d) (5 0 5))
    ((d d c) (5 0 5))
    ((d d d) (1 1 1))))

;;; problem 7
;;; a
(define (all-defect-3 my-history second-history third-history)
  'd)

(define (poor-trusting-fool-3 my-history second-history third-history)
  'c)

(define (random-strategy-3 my-history second-history third-history)
  (if (= (random 2) 0) 'c 'd))

(play-loop all-defect-3 poor-trusting-fool-3 random-strategy-3)
; Player 1 Score: 669/97
; Player 2 Score: 138/97
; Player 3 Score: 393/97
;Unspecified return value

;;; b
(define (tough-tit-for-tat my-history second-history third-history)
  (if (empty-history? my-history)
      'c
      (if (or (eq? (most-recent-play second-history) 'd)
	      (eq? (most-recent-play third-history) 'd))
	  'd
	  'c)))

(define (soft-tit-for-tat my-history second-history third-history)
  (if (empty-history? my-history)
      'c
      (if (and (eq? (most-recent-play second-history) 'd)
	       (eq? (most-recent-play third-history) 'd))
	  'd
	  'c)))

(play-loop all-defect-3 tough-tit-for-tat soft-tit-for-tat)
; Player 1 Score: 109/97
; Player 2 Score: 98/97
; Player 3 Score: 103/97
;Unspecified return value

(play-loop poor-trusting-fool-3 tough-tit-for-tat soft-tit-for-tat)
; Player 1 Score: 7
; Player 2 Score: 7
; Player 3 Score: 7
;Unspecified return value

;;; problem 8
(define (get-probability-of-c hist-0 hist-1 hist-2)
  (define make-count-total cons)
  (define ct-count car)
  (define ct-total cdr)
  (define (inc-ct ct play)
    (make-count-total (+ (ct-count ct) (if (play-c play) 1 0))
                      (1+ (ct-total ct))))
  (define (avg ct) 
    (if (zero? (ct-total ct))
      '()
      (/ (* 1.0 (ct-count ct)) (ct-total ct))))

  (define (play-c play) (eq? play 'c))

  (define (c-count hist-0 hist-1 hist-2)
    (if (empty-history? hist-0)
      (list (make-count-total 0 0) (make-count-total 0 0) (make-count-total 0 0))
      (let* ((r (c-count (rest-of-plays hist-0)
                        (rest-of-plays hist-1)
                        (rest-of-plays hist-2)))
             (ct0 (first r))
             (ct1 (second r))
             (ct2 (third r))
             (play-0 (most-recent-play hist-0))
             (play-1 (most-recent-play hist-1))
             (play-2 (most-recent-play hist-2)))
        (cond ((and (play-c play-1) (play-c play-2))
               (list (inc-ct ct0 play-0) ct1 ct2))
              ((or (play-c play-1) (play-c play-2))
               (list ct0 (inc-ct ct1 play-0) ct2))
              (else
                (list ct0 ct1 (inc-ct ct2 play-0)))))))

  (let* ((len (length hist-0))
         (r (c-count (take hist-0 (-1+ len))
                     (drop hist-1 1)
                     (drop hist-2 1))))
    (list (avg (first r)) (avg (second r)) (avg (third r)))))

(get-probability-of-c '(c c c c) '(d d d c) '(d d c c))
;Value: (1. 1. 1.)

(get-probability-of-c '(c c c d c) '(d c d d c) '(d c c c c))
;Value: (.5 1. ())

(define (is-he-a-fool? hist0 hist1 hist2)
  (equal? '(1. 1. 1.) (get-probability-of-c hist0 hist1 hist2)))

(is-he-a-fool? '(c c c c) '(d d d c) '(d d c c))
;Value: #t
(is-he-a-fool? '(c c c d c) '(d c d d c) '(d c c c c))
;Value: #f

(define (could-he-be-a-fool? hist0 hist1 hist2)
  (equal? (list true true true)
          (map (lambda (elt) (or (null? elt) (eqv? elt 1.0)))
               (get-probability-of-c hist0 hist1 hist2))))

(could-he-be-a-fool? '(c c c c) '(d d d c) '(d d c c))
;Value: #t
(could-he-be-a-fool? '(c c c d c) '(d c d d c) '(d c c c c))
;Value: #f

(define (is-soft-tit-for-tat? hist0 hist1 hist2)
  (equal? '(1. 1. 0.) (get-probability-of-c hist0 hist1 hist2)))

(define (dont-tolerate-fools my-history second-history third-history)
  (if (< (length my-history) 10)
    'c
    (if (and (could-he-be-a-fool? second-history my-history third-history)
             (could-he-be-a-fool? third-history my-history second-history))
      'd
      'c)))

(play-loop dont-tolerate-fools poor-trusting-fool-3 poor-trusting-fool-3)
; Player 1 Score: 229/26
; Player 2 Score: 44/13
; Player 3 Score: 44/13
;Unspecified return value

;;; problem 9
(define (make-combined-strategies strat0 strat1 combining)
  (lambda (my-history second-history third-history)
    (let ((r0 (strat0 my-history second-history))
          (r1 (strat1 my-history third-history)))
      (combining r0 r1))))

(define combined-tough-tit-for-tat 
  (make-combined-strategies tit-for-tat
                            tit-for-tat
                            (lambda (r1 r2)
                              (if (or (eq? r1 'd) (eq? r2 'd))
                                'd
                                'c))))

(play-loop all-defect-3 combined-tough-tit-for-tat soft-tit-for-tat)
; Player 1 Score: 109/97
; Player 2 Score: 98/97
; Player 3 Score: 103/97
;Unspecified return value

(define combined-tit-majority-random
  (make-combined-strategies tit-for-tat
                            go-by-majority
                            (lambda (r1 r2)
                              (if (= (random 2) 0) r1 r2))))

(play-loop all-defect-3
           combined-tough-tit-for-tat
           combined-tit-majority-random)
; Player 1 Score: 35/31
; Player 2 Score: 94/93
; Player 3 Score: 33/31
;Unspecified return value

(play-loop combined-tit-majority-random 
           combined-tit-majority-random
           combined-tit-majority-random)
; Player 1 Score: 7
; Player 2 Score: 7
; Player 3 Score: 7
;Unspecified return value

(play-loop all-defect-3
           poor-trusting-fool-3
           combined-tit-majority-random)
; Player 1 Score: 317/45
; Player 2 Score: 23/15
; Player 3 Score: 179/45
;Unspecified return value