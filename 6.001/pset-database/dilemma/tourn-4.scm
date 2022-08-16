;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  This is code for actually playing out a tournament
;;

(enable-language-features)
(declare (usual-integrations))

;;; Added by Lyn, for Chipmunks
(define make-vector vector-cons)

(define *PROCEDURE-ENTRIES* '())

(define *PROCEDURE-VECTOR* (make-vector 65 '()))

(define *TOURNAMENT-SCHEDULE* '())

(define *GAMES-PLAYED-SO-FAR* '())

(define *REMAINING-GAMES* '())


(define (get-strat-entry entry-no)
  (vector-ref *PROCEDURE-VECTOR* entry-no))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This stuff is basic to running prisoner's dilemma games.
;; It contains the game association matrix, various useful
;; selectors, and some standard two- and three-person strategies.

;; Game structures as created by running the tournament

(define (make-game-structure strat0-no strat1-no strat2-no
			     history0 history1 history2 number-of-rounds)
  (let ((scores (get-scores history0 history1 history2)))
    (list number-of-rounds
	  (list (list strat0-no (car scores))
		(list strat1-no (cadr scores))
		(list strat2-no (caddr scores))))))

(define (number-of-rounds game-structure)
  (car game-structure))

(define (players game-structure)
  (map (lambda (player-and-score) (car player-and-score))
       (cdr game-structure)))

(define (scores game-structure)
  (map (lambda (player-and-score) (cadr player-and-score))
       (cdr game-structure)))

(define (players-n-scores game-structure)
  (cadr game-structure))

;; Scorekeeping code. Occasional strategies might use this.

(define (get-scores history0 history1 history2)
  (define (get-scores-helper hist0 hist1 hist2 score0 score1 score2)
    (cond ((empty-history? hist0) (list score0 score1 score2))
          (else (let ((game (make-game (most-recent-play hist0)
                                       (most-recent-play hist1)
                                       (most-recent-play hist2))))
                  (get-scores-helper
                    (rest-of-plays hist0)
                    (rest-of-plays hist1)
                    (rest-of-plays hist2)
                    (+ (get-player-points 0 game) score0)
                    (+ (get-player-points 1 game) score1)
                    (+ (get-player-points 2 game) score2))))))
  (get-scores-helper history0 history1 history2 0 0 0))



(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define (get-point-list game)
  (cadr (assoc game *game-association-list*)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This list provides the "game matrix". It is used
;; by the scorekeeping procedures above. Also, some
;; strategies might use it.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *game-association-list*
  '(
    ((c c c) (7 7 7))
    ((c c d) (3 3 9))
    ((c d c) (3 9 3))
    ((d c c) (9 3 3))
    ((c d d) (0 5 5))
    ((d c d) (5 0 5))
    ((d d c) (5 5 0))
    ((d d d) (1 1 1))))
  


;; Some selectors and constructors

(define make-game list)


(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)


;; A sampler of strategies

;;; 2-person strategies

(define (all-defect h0 h1) 'd)
(define (poor-trusting-fool h0 h1) 'c)
(define (random-strategy h0 h1)
  (if (= (random 2) 0) 'c 'd))

(define (tit-for-tat h0 h1)
  (cond ((empty-history? h0) 'c)
	(else (most-recent-play h1))))

(define (tit-for-two-tats h0 h1)
  (cond ((< (history-length h0) 2) 'c)
	((or (eq? (most-recent-play h1) 'c)
	     (eq? (most-recent-play
		   (rest-of-plays h1)) 'c))
	 'c)
	(else 'd)))
	  
(define (unforgiving h0 h1)
  (define (defected-yet? hist)
    (cond ((empty-history? hist) false)
	  ((eq? 'd (most-recent-play hist)) true)
	  (else (defected-yet? (rest-of-plays hist)))))
  (cond ((empty-history? h0) 'c)
	((defected-yet? h1) 'd)
	(else 'c)))

(define (meanify strategy meanness-factor)
  (let ((comparison (* 1000 meanness-factor)))
    (lambda (h0 h1)
      (let ((default-result (strategy h0 h1)))
	(if (and (eq? default-result 'c)
		 (< (random 1000) comparison))
	    'd
	    default-result)))))
      
  
;;; 3-person strategies

(define (all-defect-3 my-history other-guy-1 other-guy-2)
  'd)

(define (poor-trusting-fool-3 my-history other-guy-1 other-guy-2)
  'c)

(define (random-strategy-3 my-history other-guy-1 other-guy-2)
  (if (= (random 2) 0) 'c 'd))


(define (tough-tit-for-tat my-history other-guy-1 other-guy-2)
  (cond ((empty-history? my-history) 'c)
	((or (eq? 'd (most-recent-play other-guy-1))
	     (eq? 'd (most-recent-play other-guy-2)))
	 'd)
	(else 'c)))


(define (soft-tit-for-tat my-history other-guy-1 other-guy-2)
  (cond ((empty-history? my-history) 'c)
	((and (eq? 'd (most-recent-play other-guy-1))
	      (eq? 'd (most-recent-play other-guy-2)))
	 'd)
	(else 'c)))


(define (make-combined-strategies 2pstrat1 2pstrat2 combiner)
  (lambda (h0 h1 h2)
    (let ((r1 (2pstrat1 h0 h1))
	  (r2 (2pstrat2 h0 h2)))
      (combiner r1 r2))))


;;; other possibly useful procedures

(define history-length length)

(define (get-nth-from-last-play n history)
  (list-ref history n))

(define (get-players-action player-no game)
  (list-ref game player-no))

(define (get-nth-from-last-game n my-history other-guy-1 other-guy-2)
  (make-game (get-nth-from-last-play n my-history)
             (get-nth-from-last-play n other-guy-1)
	     (get-nth-from-last-play n other-guy-2)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TOURNAMENT INITIALIZATION AND RUNNING



;; First step: set the list *PROCEDURE-ENTRIES* to a
;; list of tested entry procedures. One possible method
;; would be to create a file named ENTRIES.SCM, which
;; consists of all the DEFINE expressions defining the
;; entered procedures in the tournament. That file should
;; conclude with an expression like:
;;  (set! *PROCEDURE-ENTRIES* (list john-doe will-botti jill-wohl...)
;;
;; Similarly, the list *TOURNAMENT-SCHEDULE* should be set
;; to the desired starting schedule.
;;
;; The code for both testing procedure entries and generating
;; the schedule can be found in TOURN-3.SCM.


;; Second step: once all the entries are in the list
;; *PROCEDURE-ENTRIES*, create a vector of the appropriate
;; length and place the entered procedures in this vector


(define (place-entries-in-vector)
  (define (loop lis ct)
    (if (null? lis) 'done
	(sequence
	  (vector-set! *PROCEDURE-VECTOR* ct (car lis))
	  (loop (cdr lis) (1+ ct)))))
  (set! *PROCEDURE-VECTOR*
	(make-vector (length *PROCEDURE-ENTRIES*) '()))
  (loop *PROCEDURE-ENTRIES* 0))



;; We now have a vector containing all the entered procedures.
;; We want to initialize a few of the global variables.


(define (initialize-tournament-variables)
  (set! *REMAINING-GAMES* *TOURNAMENT-SCHEDULE*)
  (set! *GAMES-PLAYED-SO-FAR* '()))



;; Playing the games of the tournament

(define (play-tournament-game strat0-number strat1-number strat2-number)
  (define (play-loop-iter
             strat0 strat1 strat2 count hist0 hist1 hist2 limit)
    (cond ((= count limit) 
	   (make-game-structure strat0-number strat1-number strat2-number
				hist0 hist1 hist2 limit))
          (else (let ((result0 (strat0 hist0 hist1 hist2))
                      (result1 (strat1 hist1 hist2 hist0))
                      (result2 (strat2 hist2 hist0 hist1)))
                  (play-loop-iter
                    strat0 strat1 strat2
                    (1+ count)
                    (extend-history result0 hist0)
                    (extend-history result1 hist1)
                    (extend-history result2 hist2)
                    limit)))))
  (play-loop-iter
    (get-strat-entry strat0-number)
    (get-strat-entry strat1-number) 
    (get-strat-entry strat2-number)
    0 '() '() '() (+ 90 (random 21))))

(define (play-tournament-games count)
  (cond ((null? *REMAINING-GAMES*) 'DONE)
	(else
	 (let* ((next-triplet (car *REMAINING-GAMES*))
		(next-game
		 (play-tournament-game
		  (car next-triplet) (cadr next-triplet) (caddr next-triplet))))
	   (set! *REMAINING-GAMES* (cdr *REMAINING-GAMES*))
	   (set! *GAMES-PLAYED-SO-FAR* (cons next-game
					     *GAMES-PLAYED-SO-FAR*))
	   (print-out-next-game count next-game)
	   (play-tournament-games (1+ count))))))

(define (print-out-next-game count game)
  (newline)
  (newline)
  (princ "The results of game ")
  (princ count)
  (princ ":  ")
  (princ game)
  )



