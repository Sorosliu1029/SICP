;;; Copyright (c) 1990 Massachusetts Institute of Technology
;;; 
;;; This material was developed by the Scheme project at the Massachusetts
;;; Institute of Technology, Department of Electrical Engineering and
;;; Computer Science.  Permission to copy this material, to redistribute
;;; it, and to use it for any non-commercial purpose is granted, subject
;;; to the following restrictions and understandings.
;;; 
;;; 1. Any copy made of this material must include this copyright notice
;;; in full.
;;; 
;;; 2. Users of this material agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions that
;;; they make, so that these may be included in future releases; and (b)
;;; to inform MIT of noteworthy uses of this material.
;;; 
;;; 3. All materials developed as a consequence of the use of this
;;; material shall duly acknowledge such use, in accordance with the usual
;;; standards of acknowledging credit in academic research.
;;; 
;;; 4. MIT has made no warrantee or representation that this material
;;; (including the operation of software contained therein) will be
;;; error-free, and MIT is under no obligation to provide any services, by
;;; way of maintenance, update, or otherwise.
;;; 
;;; 5. In conjunction with products arising from the use of this material,
;;; there shall be no use of the name of the Massachusetts Institute of
;;; Technology nor of any adaptation thereof in any advertising,
;;; promotional, or sales literature without prior written consent from
;;; MIT in each case. 

\subhd{The Two-Player Prisoner's Dilemma Scheme program}

{\beginscheme%
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
    (princ "Player 1 Score: ")
    (princ (/ (car scores) number-of-games))
    (newline)
    (princ "Player 2 Score: ")
    (princ (/ (cadr scores) number-of-games))
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

(define (unforgiving my-history other-history)
  (define (did-other-guy-defect? other-history)
    (cond ((empty-history? other-history) false)
          ((eq? (most-recent-play other-history) 'd) true)
          (else (did-other-guy-defect? (rest-of-plays other-history)))))
  (if (did-other-guy-defect? other-history) 'd 'c))

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

