(enable-language-features)
(declare (usual-integrations))

;;; Added by Lyn, for Chipmunks
(define make-vector vector-cons)

;;; ANALYSIS AND POST-MORTEM FOR PRISONER'S DILEMMA TOURNAMENTS

(define *SCORES-VECTOR* '())
;(define *GAMES-PLAYED* '())
(define *FINAL-STANDINGS* '())


;; Game structures as created by running the tournament
;; This stuff is useful for what follows.

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


;; First step: Set *GAMES-PLAYED* to the correct value -- i.e., a list
;; of game structures as created by the tournament code of TOURN-4.SCM.


;; This procedure starts things off.

(define (initialize-post-mortem no-of-entries)
  (set! *SCORES-VECTOR*
	(make-vector no-of-entries (list 0 0))))


;; Totaling scores from the *GAMES-PLAYED* list.

(define (calculate-scores)
  (map (lambda (game)
	 (map (lambda (player-n-score)
		(let* ((player-no (car player-n-score))
		       (player-points (cadr player-n-score))
		       (current-score (vector-ref *SCORES-VECTOR* player-no)))
		  (vector-set! *SCORES-VECTOR* player-no
			       (list (+ (number-of-rounds game)
					(car current-score))
				     (+ player-points
					(cadr current-score))))))
	      (players-n-scores game)))
       *GAMES-PLAYED*)
  'DONE)

(define (calculate-scores-2) ;normalizes games to 1 round each
  (map (lambda (game)
	 (map (lambda (player-n-score)
		(let* ((player-no (car player-n-score))
		       (player-points (cadr player-n-score))
		       (current-score (vector-ref *SCORES-VECTOR* player-no)))
		  (vector-set! *SCORES-VECTOR* player-no
			       (list (1+ (car current-score))
				     (+ (/ player-points
					   (number-of-rounds game))
					(cadr current-score))))))
	      (players-n-scores game)))
       *GAMES-PLAYED*)
  'DONE)


;; Sorting scores into *FINAL-STANDINGS*

(define (get-total-scores)
  (set! *FINAL-STANDINGS*
	(sort-by-scores (make-average-list))))

(define (make-average-list)
  (define (loop av-lis count result)
    (cond ((null? av-lis) result)
	  (else (loop (cdr av-lis) (1+ count)
		      (cons (list count (car av-lis))
			    result)))))
  (loop
   (map-over-vector (lambda (rounds-n-total-points)
		      (/ (cadr rounds-n-total-points)
			 (car rounds-n-total-points)))
		    *SCORES-VECTOR*)
   0 '()))

(define (sort-by-scores av-lis)
  (define (insert-by-score element sorted-av-lis)
    (cond ((null? sorted-av-lis) (list element))
	  ((> (cadr element) (cadar sorted-av-lis)) ;score > first sorted score
	   (cons element sorted-av-lis))
	  (else (cons (car sorted-av-lis)
		      (insert-by-score element (cdr sorted-av-lis))))))
  (cond ((null? av-lis) '())
	(else (insert-by-score (car av-lis)
			       (sort-by-scores (cdr av-lis))))))


;;; Utilities

(define (map-over-vector proc vect)
  (define (loop counter)
    (cond ((= counter (vector-length vect)) '())
	  (else (cons (proc (vector-ref vect counter))
		      (loop (1+ counter))))))
  (loop 0))
