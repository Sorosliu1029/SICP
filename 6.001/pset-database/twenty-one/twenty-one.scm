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

;;;This is the code for the Twenty-One problem set

(define (twenty-one player house)
  (let ((house-initial-hand (hand-create (deal))))
    (let ((player-hand (play-hand player
				  (hand-create (deal))
				  (hand-up-card house-initial-hand))))
      (if (> (hand-total player-hand) 21)
	  0						; player loses
	  (let ((house-hand (play-hand house
				       house-initial-hand
				       (hand-up-card player-hand))))
	    (cond ((> (hand-total house-hand) 21) 1)	; house loses
		  ((> (hand-total player-hand)
		      (hand-total house-hand)) 1)	; house loses
		  (else 0)				; player loses
		  ))))))


(define (play-hand player our-hand opponent-up-card)
  (cond ((> (hand-total our-hand) 21) our-hand)
	((player our-hand opponent-up-card)
	 (play-hand player
		    (hand-add-card our-hand (deal))
		    opponent-up-card))
	(else our-hand)))


(define (deal) (+ 1 (random 10)))

;
; The Hand Data Abstraction
;

(define (hand-create up-card)
  (list up-card (card-set-create up-card)))

(define (hand-up-card hand)
  (car hand))

(define (hand-total hand)
  (card-set-total (cadr hand)))

(define (hand-print hand)
  (card-set-print (cadr hand)))

(define (hand-add-card hand card)
  (list (hand-up-card hand)
	(card-set-add (cadr hand) card)))

;
; Card Set Data Abstraction
;

(define (card-set-create first-card)
  first-card)

(define (card-set-print set)
  (princ set))

(define (card-set-total set)
  set)

(define (card-set-add set card)
  (+ set card))


;
; Strategy Procedure
;

(define (hit? our-hand opponent-up-card)
  (newline)
  (princ "Opponent up card ")
  (princ opponent-up-card)
  (newline)
  (princ "Your Hand: ")
  (hand-print our-hand)
  (newline)
  (princ "Hit? ")
  (user-says-y?))

(define (user-says-y?)
  (= (tyi) (ascii 'Y)))
