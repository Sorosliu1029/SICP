;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; TOURN-3.SCM
;;;
;;  Setup for 3-person prisoner's dilemma tournaments

(enable-language-features)
(declare (usual-integrations))

;;; Added by Lyn, for Chipmunks
(define make-vector vector-cons)

(define *TESTED-ENTRIES* '()) ;will eventually hold tested entries
(define *UNTESTED-ENTRIES* '()) ;holds entries as they are typed in

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The PLAY-LOOP procedure takes as its arguments three prisoner's 
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds. A strategy is a procedure that takes
;;  three arguments: a history of the player's previous plays and 
;;  a history of the other players' previous plays. The procedure 
;;  returns either the symbol C (for "cooperate") or D ("defect").
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (play-loop strat0 strat1 strat2)
  (define (play-loop-iter
             strat0 strat1 strat2 count hist0 hist1 hist2 limit)
    (cond ((= count limit) (print-out-results hist0 hist1 hist2 limit))
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
    strat0 strat1 strat2 0 '() '() '() (+ 90 (random 21))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following procedures are used to compute and
;; print out the players' scores at the end of an
;; iterated game.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 history2 number-of-games)
  (let ((scores (get-scores history0 history1 history2)))
    (newline)
    (princ "Player 1 Score: ")
    (princ (/ (car scores) number-of-games))            
    (newline)
    (princ "Player 2 Score: ")
    (princ (/ (cadr scores) number-of-games))            
    (newline)
    (princ "Player 3 Score: ")
    (princ (/ (caddr scores) number-of-games))
    (newline)))


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
;; by the scorekeeping procedures above.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TESTING AN ENTRY
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following code is used to test an entry for the
;; three-person tournament, just to ensure that it is not
;; blatantly and obviously buggy. The idea is that we
;; will call PLAY-LOOP on the new entry with a bunch of
;; standard opponents, including the entry itself.


(define *test-battery-list*
  (list 
   (list random-strategy-3 random-strategy-3)
   ))

(define (run-battery-of-tests new-entry)
  (newline)
  (princ new-entry)
  (newline)
  (for-each (lambda (pair-of-opponents)
	      (play-loop new-entry
			 (car pair-of-opponents)
			 (cadr pair-of-opponents)))
	    *test-battery-list*)
  (princ 'TEST-COMPLETE))



(define (test-entry my-history other-guy-1 other-guy-2)
  (cond ((empty-history? my-history) 'c)
	((and (eq? 'd (most-recent-play other-guy-1))
	      (eq? 'd (most-recent-play other-guy-2)))
	 'd)
	((empty-history? (rest-of-plays my-history)) 'c)
	((and (eq? 'd (most-recent-play other-guy-1))
	      (eq? 'd (most-recent-play
		       (rest-of-plays other-guy-1))))
	 'd)
	((and (eq? 'd (most-recent-play other-guy-2))
	      (eq? 'd (most-recent-play
		       (rest-of-plays other-guy-2))))
	 'd)
	(else 'c)))
		   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TOURNAMENT GENERATION
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following section contains procedures used to generate
;; the schedule of players for the three-person tournament.
;; The TOURNAMENT procedure (with eventual modification) will
;; take a list of players (or entry numbers) and generate a
;; list of triplets in which every player takes place in some
;; fixed number of games, and no player faces the same pair
;; twice.
;;
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Useful list procedures
;;
;;

(define (delete elt lis)
  (cond ((null? lis) '())
	((eq? elt (car lis)) (delete elt (cdr lis)))
	(else (cons (car lis)
		    (delete elt (cdr lis))))))

(define (select-random-elt lis)
  (list-ref lis (random (length lis))))

(define (shuffle lis)
  (cond ((null? lis) '())
	(else (let ((some-elt (select-random-elt lis)))
		(cons some-elt
		      (shuffle (delete some-elt lis)))))))


;This next procedure assumes lis1 and lis2 are of equal length 
;and contain no duplicates.

(define (same-elts? lis1 lis2) 
  (cond ((null? lis1) true)
	((memv (car lis1) lis2)
	 (same-elts? (cdr lis1) lis2))
	(else false)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Most of the work is done with lists of entry-and-index elements.
;; For now, each of these is simply a two-element list (of entry and
;; index, naturally!) in which the index refers to a vector keeping
;; count of the number of games allotted to this particular entry
;; thus far. 

(define (vector-index entry-and-index)
  (cadr entry-and-index))

(define (entry entry-and-index)
  (car entry-and-index))

(define (games-left ent-and-index vect)
  (vector-ref vect (vector-index ent-and-index)))

(define (decrement-vector-entry entry-and-index vect)
  (vector-set! vect (vector-index entry-and-index)
	       (-1+ (games-left entry-and-index vect))))

(define (make-entry-index-list entry-lis)
  (define (loop lis count)
    (cond ((null? lis) '())
	  (else (cons (list (car lis) count)
		      (loop (cdr lis) (1+ count))))))
  (loop entry-lis 0))


(define (make-initial-count-vector entry-lis no-of-games)
  (make-vector (length entry-lis) no-of-games))

(define (delete-entries-with-zero-count ent-and-ind-lis vect)
  (cond ((null? ent-and-ind-lis) '())
	((zero? (games-left (car ent-and-ind-lis) vect))
	 (delete-entries-with-zero-count (cdr ent-and-ind-lis) vect))
	(else (cons (car ent-and-ind-lis)
		    (delete-entries-with-zero-count
		     (cdr ent-and-ind-lis) vect)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The heart of the triplet-generation program

(define (get-n-if-possible entry-and-ind-lis vect max-games-left chosen-so-far n)
  (cond ((null? entry-and-ind-lis) '())
	((= (games-left (car entry-and-ind-lis) vect) max-games-left)
	 (if (= chosen-so-far (-1+ n))
	     (list (car entry-and-ind-lis))
	     (cons (car entry-and-ind-lis)
		   (get-n-if-possible (cdr entry-and-ind-lis)
				      vect max-games-left (1+ chosen-so-far) n))))
	(else (get-n-if-possible
	       (cdr entry-and-ind-lis) vect max-games-left chosen-so-far n))))


(define (get-triplets e-and-i-lis vect max-games games-so-far repeat-count)
  (define (decrement-vector-entries ent-and-ind-lis)
    (for-each (lambda (e-and-i) (decrement-vector-entry e-and-i vect))
	      ent-and-ind-lis))
  (cond ((game-over? vect max-games) games-so-far)
	((= repeat-count 3) 'FAILED)
	(else (let ((try-for-three (get-n-if-possible (shuffle e-and-i-lis) vect max-games 0 3)))
		(cond ((= (length try-for-three) 3)
		       (if (mem-elt? try-for-three games-so-far)
			   (get-triplets e-and-i-lis vect max-games games-so-far (1+ repeat-count))
			   (sequence (decrement-vector-entries try-for-three)
				     (get-triplets
				      e-and-i-lis vect max-games (cons try-for-three games-so-far) 0))))
		      (else (let* ((games-gotten (length try-for-three))
				   (try-for-rest (get-n-if-possible (shuffle e-and-i-lis) vect (-1+ max-games) 0
								    (- 3 games-gotten)))
				   (net-games (append try-for-three try-for-rest)))
			      (if (mem-elt? net-games games-so-far)
				  (get-triplets e-and-i-lis vect max-games games-so-far (1+ repeat-count))
				  (sequence (decrement-vector-entries net-games)
					    (get-triplets e-and-i-lis vect (-1+ max-games)
							  (cons net-games games-so-far) 0))))))))))



(define (just-entries triplet-lis)
  (cond ((eq? triplet-lis 'FAILED) triplet-lis)
	(else (map (lambda (triplet)
		     (map (lambda (ent-and-ind) (entry ent-and-ind))
			  triplet))
		   triplet-lis))))

(define (game-over? vect max-games)
  (cond ((<= max-games 0) true)
	((= max-games 1) (all-zeros? vect))
	(else false)))

(define (all-zeros? vect)
  (let ((vlen (vector-length vect)))
    (define (loop count)
      (cond ((= count vlen) true)
	    ((zero? (vector-ref vect count))
	     (loop (1+ count)))
	    (else false)))
    (loop 0)))


(define (mem-elt? e-and-i-triplet e-and-i-triplet-lis)
  (cond ((null? e-and-i-triplet-lis) '())
	((same-elts? e-and-i-triplet (car e-and-i-triplet-lis)) true)
	(else (mem-elt? e-and-i-triplet
			(cdr e-and-i-triplet-lis)))))

(define (find-duplicate-games game-lis)
  (cond ((null? game-lis) '())
	((mem-elt? (car game-lis) (cdr game-lis))
	 (cons (car game-lis)
	       (find-duplicate-games (cdr game-lis))))
	(else (find-duplicate-games (cdr game-lis)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; What follows is a second algorithm for creating all
;; possible triplets from a list of distinct elements.


(define (all-possible-sets-of n lis)
  (cond ((= n 0) (list '()))
	((null? lis) '())
	(else (append
	       (map (lambda (sublis) (cons (car lis) sublis))
		    (all-possible-sets-of (-1+ n) (cdr lis)))
	       (all-possible-sets-of n (cdr lis))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ENTERING TESTED ENTRIES INTO THE LIST *TESTED-ENTRIES*

;; First step: Type in entries and add them into the
;; list *UNTESTED-ENTRIES* as they are typed. Then run
;; a few tests on them.

;; When procedures are typed in, their names should be
;; that of the person submitting them.

;; Example

(define (duck-entry my-history other-guy-1 other-guy-2)
  (cond ((empty-history? my-history) 'c)
	((and (eq? 'd (most-recent-play other-guy-1))
	      (eq? 'd (most-recent-play other-guy-2)))
	 'd)
	((empty-history? (rest-of-plays my-history)) 'c)
	((and (eq? 'd (most-recent-play other-guy-1))
	      (eq? 'd (most-recent-play
		       (rest-of-plays other-guy-1))))
	 'd)
	((and (eq? 'd (most-recent-play other-guy-2))
	      (eq? 'd (most-recent-play
		       (rest-of-plays other-guy-2))))
	 'd)
	(else 'c)))

(define (test-and-add-new-entry entry)
  (run-battery-of-tests entry)
  (set! *TESTED-ENTRIES* (cons entry *TESTED-ENTRIES*)))

(define (add-new-entry entry)
  (set! *TESTED-ENTRIES* (cons entry *TESTED-ENTRIES*)))

;; This procedure will add the untested entries into the
;; tested entry list, one by one.

(define (fill-procedure-entries)
  (for-each (lambda (entry) (test-and-add-new-entry entry))
	    *UNTESTED-ENTRIES*))


;; We create a tournament schedule based on the number of
;; procedures entered. If there are fewer than, say, 20
;; entries we can generate a complete tournament, using
;; the ALL-POSSIBLE-SETS-OF procedure.

(define *temp-entry-index-list* '())
(define *count-vector* )

(define (set-up-complete-tournament)
  (set! *TOURNAMENT-SCHEDULE*
	(all-possible-sets-of
	 3 
	 (make-integer-list (length *TESTED-ENTRIES*)))))
			      
(define (set-up-tournament no-of-games-each)
  (let* ((no-of-entries (length *TESTED-ENTRIES*))
	 (number-list (make-integer-list no-of-entries)))
    (set! *temp-entry-index-list*
	  (make-entry-index-list number-list))
    (set! *count-vector*
	  (make-initial-count-vector *temp-entry-index-list*
				     number-of-games-each))
    (set! *TOURNAMENT-SCHEDULE*
	  (get-entry-triplets no-of-games-each))))


(define (get-entry-triplets num)
  (just-entries (get-triplets *temp-entry-index-list* *count-vector*
			      num '() 0)))
	   
(define (make-integer-list no-of-entries)
  (define (loop ct list-so-far)
    (cond ((< ct 0) list-so-far)
	  (else (loop (-1+ ct) (cons ct list-so-far)))))
  (loop (-1+ no-of-entries) '()))


;; We used to think this was a good idea
(define (get-the-right-number no-of-entries)
  (let ((rough-number (/ 24000 no-of-entries)))
    (- rough-number (remainder rough-number 3))))



