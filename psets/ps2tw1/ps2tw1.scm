;;; Scheme code for Twenty-One Simulator [PS2 Fall '90]

(define (twenty-one player-strategy house-strategy)
  (let ((house-initial-hand (make-new-hand (deal))))
    (let ((player-hand
           (play-hand player-strategy
                      (make-new-hand (deal))
                      (hand-up-card house-initial-hand))))
      (if (> (hand-total player-hand) 21)
          0                                ; ``bust'': player loses
          (let ((house-hand 
                 (play-hand house-strategy
                            house-initial-hand
                            (hand-up-card player-hand))))
            (cond ((> (hand-total house-hand) 21)
                   1)                      ; ``bust'': house loses
                  ((> (hand-total player-hand)
                      (hand-total house-hand))
                   1)                      ; house loses
                  (else 0)))))))           ; player loses

(define (play-hand strategy my-hand opponent-up-card)
  (cond ((> (hand-total my-hand) 21) my-hand) ; I lose... give up
        ((strategy my-hand opponent-up-card) ; hit?
         (play-hand strategy
                    (hand-add-card my-hand (deal))
                    opponent-up-card))
        (else my-hand)))                ; stay


(define (deal) (+ 1 (random 10)))

(define (make-new-hand first-card)
  (make-hand first-card first-card))

(define (make-hand up-card total)
  (cons up-card total))

(define (hand-up-card hand)
  (car hand))

(define (hand-total hand)
  (cdr hand))

(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
             (+ new-card (hand-total hand))))

(define (hit? your-hand opponent-up-card)
  (newline)
  (display "; Opponent up card ")
  (display opponent-up-card)
  (newline)
  (display "; Your Total: ")
  (display (hand-total your-hand))
  (prompt-for-confirmation "Hit? "))

;;; problem 1

(twenty-one hit? hit?)
; Opponent up card 10
; Your Total: 8
; Opponent up card 10
; Your Total: 17
; Opponent up card 8
; Your Total: 10
; Opponent up card 8
; Your Total: 14
;Value: 1

;;; problem 2

(define (stop-at n)
  (lambda (my-hand opponent-up-card)
    (< (hand-total my-hand) n)))

(twenty-one hit? (stop-at 16))
; Opponent up card 5
; Your Total: 1
; Opponent up card 5
; Your Total: 2
; Opponent up card 5
; Your Total: 8
; Opponent up card 5
; Your Total: 15
;Value: 0

(twenty-one hit? (stop-at 16))
; Opponent up card 6
; Your Total: 5
; Opponent up card 6
; Your Total: 14
; Opponent up card 6
; Your Total: 19
;Value: 1

;;; problem 3

(define (test-strategy player-strategy house-strategy n)
  (if (= n 0)
      0
      (+ (test-strategy player-strategy house-strategy (-1+ n))
	 (twenty-one player-strategy house-strategy))))

(test-strategy (stop-at 16) (stop-at 15) 10)
;Value: 3
(test-strategy (stop-at 16) (stop-at 15) 10)
;Value: 7
(test-strategy (stop-at 16) (stop-at 15) 10)
;Value: 5

;;; problem 4

(define (watch-player strategy)
  (lambda (my-hand opponent-up-card)
    (let ((result (strategy my-hand opponent-up-card)))
      (newline)
      (display "; Hand's total: ")
      (display (hand-total my-hand))
      (display " Opponent up card: ")
      (display opponent-up-card)
      (display ", result: ")
      (display (if result "Hit" "Stay"))
      result)))

(test-strategy (watch-player (stop-at 16))
               (watch-player (stop-at 15))
               2)
; Hand's total: 2 Opponent up card: 7, result: Hit
; Hand's total: 6 Opponent up card: 7, result: Hit
; Hand's total: 13 Opponent up card: 7, result: Hit
; Hand's total: 17 Opponent up card: 7, result: Stay
; Hand's total: 7 Opponent up card: 2, result: Hit
; Hand's total: 11 Opponent up card: 2, result: Hit
; Hand's total: 17 Opponent up card: 2, result: Stay
; Hand's total: 9 Opponent up card: 6, result: Hit
; Hand's total: 13 Opponent up card: 6, result: Hit
; Hand's total: 15 Opponent up card: 6, result: Hit
; Hand's total: 20 Opponent up card: 6, result: Stay
; Hand's total: 6 Opponent up card: 9, result: Hit
; Hand's total: 15 Opponent up card: 9, result: Stay
;Value: 1

;;; problem 5

(define (louis my-hand opponent-up-card)
  (let ((total (hand-total my-hand)))
    (cond ((< total 12) true)
          ((> total 16) false)
          ((= total 12) (< opponent-up-card 4))
          ((= total 16) (not (= opponent-up-card 10)))
          (else (> opponent-up-card 6)))))

(test-strategy louis (stop-at 15) 10)
;Value: 2
(test-strategy louis (stop-at 16) 10)
;Value: 4
(test-strategy louis (stop-at 17) 10)
;Value: 1

;;; problem 6

(define (both sa sb)
  (lambda (my-hand opponent-up-card)
    (and (sa my-hand opponent-up-card) (sb my-hand opponent-up-card))))

(test-strategy (both (stop-at 19) hit?) (stop-at 20) 2)
; Opponent up card 6
; Your Total: 10
; Opponent up card 6
; Your Total: 14
; Opponent up card 9
; Your Total: 6
; Opponent up card 9
; Your Total: 9
; Opponent up card 9
; Your Total: 10
; Opponent up card 9
; Your Total: 16
;Value: 0

;;; tutorial exercise 1

;;; card suits
(define diamods 'd)
(define clubs 'c)
(define hearts 'h)
(define spades 's)

;;; card
(define (make-card suit value) (cons suit value))
(define (card-suit card) (car card))
(define (card-value card) (cdr card))

;;; card set
(define (card-set-add card set) (cons card set))
(define (make-new-set card) (card-set-add card ()))
(define (first-card set) (car set))
(define (rest-set set) (cdr set))
(define (empty? set) (null? set))
(define (total set)
  (if (empty? set)
      0
      (+ (card-value (first-card set))
	 (total (rest-set set)))))

;;; hand
(define (make-hand up-card hand-cards) (cons up-card hand-cards))
(define (make-new-hand first-card) (make-hand first-card (make-new-set first-card)))
(define (hand-up-card hand) (car hand))
(define (hand-cards hand) (cdr hand))
(define (hand-total hand) (total (hand-cards hand)))
(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
             (card-set-add new-card (hand-cards hand))))

;;; other changes
(define (deal) (make-card (random-suit) (random-value)))
(define (suit idx)
  (cond ((= idx 0) diamods)
	((= idx 1) clubs)
	((= idx 2) hearts)
	(else spades)))
(define (random-suit) (suit (random 4)))
(define (random-value) (1+ (random 10)))

;;; let's play
(twenty-one hit? hit?)
; Opponent up card (s . 2)
; Your Total: 3
; Opponent up card (s . 2)
; Your Total: 8
; Opponent up card (s . 2)
; Your Total: 10
; Opponent up card (s . 2)
; Your Total: 17
; Opponent up card (c . 3)
; Your Total: 2
; Opponent up card (c . 3)
; Your Total: 3
; Opponent up card (c . 3)
; Your Total: 9
; Opponent up card (c . 3)
; Your Total: 16
;Value: 1

(twenty-one (watch-player (stop-at 16))
            (watch-player (stop-at 20)))
; Hand's total: 7 Opponent up card: (s . 9), result: Hit
; Hand's total: 11 Opponent up card: (s . 9), result: Hit
; Hand's total: 18 Opponent up card: (s . 9), result: Stay
; Hand's total: 9 Opponent up card: (c . 7), result: Hit
; Hand's total: 15 Opponent up card: (c . 7), result: Hit
; Hand's total: 17 Opponent up card: (c . 7), result: Hit
;Value: 1

;;; tutorial exercise 2

;;; a
(define (fresh-deck kind-of-cards)
  (define (recur idx)
    (if (> idx (* 4 kind-of-cards)) 
	()
	(let ((v (min 10 (ceiling (/ idx 4))))
	      (s (suit (remainder idx 4))))
	  (card-set-add (make-card s v)
			(recur (1+ idx))))))
  (recur 1))

(fresh-deck 10)
;Value: ((c . 1) (h . 1) (s . 1) (d . 1) (c . 2) (h . 2) (s . 2) (d . 2) (c . 3) (h . 3) (s . 3) (d . 3) (c . 4) (h . 4) (s . 4) (d . 4) (c . 5) (h . 5) (s . 5) (d . 5) (c . 6) (h . 6) (s . 6) (d . 6) (c . 7) (h . 7) (s . 7) (d . 7) (c . 8) (h . 8) (s . 8) (d . 8) (c . 9) (h . 9) (s . 9) (d . 9) (c . 10) (h . 10) (s . 10) (d . 10))

(fresh-deck 13)
;Value: ((c . 1) (h . 1) (s . 1) (d . 1) (c . 2) (h . 2) (s . 2) (d . 2) (c . 3) (h . 3) (s . 3) (d . 3) (c . 4) (h . 4) (s . 4) (d . 4) (c . 5) (h . 5) (s . 5) (d . 5) (c . 6) (h . 6) (s . 6) (d . 6) (c . 7) (h . 7) (s . 7) (d . 7) (c . 8) (h . 8) (s . 8) (d . 8) (c . 9) (h . 9) (s . 9) (d . 9) (c . 10) (h . 10) (s . 10) (d . 10) (c . 10) (h . 10) (s . 10) (d . 10) (c . 10) (h . 10) (s . 10) (d . 10) (c . 10) (h . 10) (s . 10) (d . 10))

;;; b
(define (shuffle-by-combine deck combine)
  (define (cut-half deck)
    (let ((len (length deck)))
      (if (odd? len)
	  (error "deck cards not even, cannot cut half")
	  (cons (take deck (/ len 2))
		(drop deck (/ len 2))))))
  (let ((halves (cut-half deck)))
    (combine (car halves) (cdr halves))))

;;; deterministic combine: alternately choose a card from eacho of the two halves
(define (deterministic-combine h1 h2)
  (cond ((empty? h1) h2)
        ((empty? h2) h1)
        (else (card-set-add (first-card h1)
                            (card-set-add (first-card h2)
                                          (deterministic-combine (rest-set h1) (rest-set h2)))))))

(define (shuffle deck) (shuffle-by-combine deck deterministic-combine))

(shuffle (fresh-deck 10))
;Value: ((c . 1) (c . 6) (h . 1) (h . 6) (s . 1) (s . 6) (d . 1) (d . 6) (c . 2) (c . 7) (h . 2) (h . 7) (s . 2) (s . 7) (d . 2) (d . 7) (c . 3) (c . 8) (h . 3) (h . 8) (s . 3) (s . 8) (d . 3) (d . 8) (c . 4) (c . 9) (h . 4) (h . 9) (s . 4) (s . 9) (d . 4) (d . 9) (c . 5) (c . 10) (h . 5) (h . 10) (s . 5) (s . 10) (d . 5) (d . 10))

(shuffle (fresh-deck 13))
;Value: ((c . 1) (s . 7) (h . 1) (d . 7) (s . 1) (c . 8) (d . 1) (h . 8) (c . 2) (s . 8) (h . 2) (d . 8) (s . 2) (c . 9) (d . 2) (h . 9) (c . 3) (s . 9) (h . 3) (d . 9) (s . 3) (c . 10) (d . 3) (h . 10) (c . 4) (s . 10) (h . 4) (d . 10) (s . 4) (c . 10) (d . 4) (h . 10) (c . 5) (s . 10) (h . 5) (d . 10) (s . 5) (c . 10) (d . 5) (h . 10) (c . 6) (s . 10) (h . 6) (d . 10) (s . 6) (c . 10) (d . 6) (h . 10) (c . 7) (s . 10) (h . 7) (d . 10))

;;; c
(define (random-combine h1 h2)
  (define (random-number) (1+ (random 5)))
  (define (first-n n set)
    (if (or (= n 0) (empty? set))
	()
	(card-set-add (first-card set)
		      (first-n (-1+ n) (rest-set set)))))
  (define (remain-after-n n set)
    (if (or (= n 0) (empty? set))
	set
	(remain-after-n (-1+ n) (rest-set set))))
  (define (cards-add cards set)
    (if (empty? cards)
	set
	(card-set-add (first-card cards)
		      (cards-add (rest-set cards) set))))
  (cond ((empty? h1) h2)
        ((empty? h2) h1)
        (else 
	 (let ((h1-n (random-number))
	       (h2-n (random-number)))
	   (cards-add (first-n h1-n h1)
		      (cards-add (first-n h2-n h2)
				 (random-combine (remain-after-n h1-n h1) (remain-after-n h2-n h2))))))))

(define (random-shuffle deck) (shuffle-by-combine deck random-combine))

(random-shuffle (fresh-deck 10))
;Value: ((c . 1) (c . 6) (h . 6) (h . 1) (s . 1) (d . 1) (c . 2) (h . 2) (s . 6) (d . 6) (s . 2) (d . 2) (c . 3) (h . 3) (c . 7) (h . 7) (s . 7) (d . 7) (c . 8) (s . 3) (d . 3) (c . 4) (h . 4) (h . 8) (s . 8) (s . 4) (d . 4) (c . 5) (h . 5) (d . 8) (c . 9) (h . 9) (s . 5) (d . 5) (s . 9) (d . 9) (c . 10) (h . 10) (s . 10) (d . 10))

(random-shuffle (fresh-deck 13))
;Value: ((c . 1) (h . 1) (s . 1) (d . 1) (c . 2) (s . 7) (h . 2) (s . 2) (d . 2) (c . 3) (h . 3) (d . 7) (s . 3) (d . 3) (c . 4) (h . 4) (c . 8) (s . 4) (d . 4) (h . 8) (c . 5) (h . 5) (s . 5) (s . 8) (d . 8) (c . 9) (h . 9) (s . 9) (d . 5) (c . 6) (h . 6) (d . 9) (c . 10) (s . 6) (d . 6) (h . 10) (s . 10) (d . 10) (c . 7) (c . 10) (h . 10) (s . 10) (d . 10) (c . 10) (h . 7) (h . 10) (s . 10) (d . 10) (c . 10) (h . 10) (s . 10) (d . 10))

;;; d
(define deal first-card)
(define after-deal rest-set)
(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (-1+ n)))))

(define (test-strategy player-strategy house-strategy n)
  (define (debug k deck)
    (cond ((= k n) (display "; ") (display deck))
	  (else (display "")))
    (newline)
    (display "; k: ")
    (display k)
    (display " deck size: ")
    (display (length deck))
    (display " first card: ")
    (display (if (empty? deck) "NULL" (first-card deck))))
  (define (test-strategy-helper k deck) ;;; deck may be empty
    (debug k deck)
    (if (or (= k 0) (empty? deck))
	0
	(twenty-one player-strategy
		    house-strategy
		    deck
		    (lambda (deck) (test-strategy-helper (-1+ k) deck)))))
  (define shuffle-times 3)
  (test-strategy-helper n
			((repeated random-shuffle shuffle-times) (fresh-deck 13))))

(define (twenty-one player-strategy house-strategy deck ret) ;;; deck cannot be empty
  (let ((house-initial-hand (make-new-hand (deal deck)))
        (deck-after-house-deal (after-deal deck)))
    (if (empty? deck-after-house-deal)
	(ret deck-after-house-deal)
	(let ((player-hand-with-deck
	       (play-hand player-strategy
			  (make-new-hand (deal deck-after-house-deal))
			  (hand-up-card house-initial-hand)
			  (after-deal deck-after-house-deal))))
	  (if (> (hand-total (car player-hand-with-deck)) 21)
	      (ret (cdr player-hand-with-deck))
	      (let ((house-hand-with-deck
		     (play-hand house-strategy
				house-initial-hand
				(hand-up-card (car player-hand-with-deck))
				(cdr player-hand-with-deck))))
		(cond ((> (hand-total (car house-hand-with-deck)) 21)
		       (1+ (ret (cdr house-hand-with-deck))))
		      ((> (hand-total (car player-hand-with-deck)) (hand-total (car house-hand-with-deck)))
		       (1+ (ret (cdr house-hand-with-deck))))
		      (else (ret (cdr house-hand-with-deck))))))))))

(define (play-hand strategy my-hand opponent-up-card deck)
  (cond ((empty? deck) (cons my-hand deck))
        ((> (hand-total my-hand) 21) (cons my-hand deck))
        ((strategy my-hand opponent-up-card)
         (play-hand strategy
                    (hand-add-card my-hand (deal deck))
                    opponent-up-card
                    (after-deal deck)))
        (else (cons my-hand deck))))

(test-strategy (stop-at 16) (stop-at 20) 1)
; ((c . 1) (h . 1) (h . 4) (d . 8) (s . 1) (d . 1) (s . 7) (d . 7) (c . 8) (h . 6) (s . 4) (c . 2) (d . 10) (c . 10) (h . 10) (d . 10) (c . 10) (s . 6) (d . 6) (d . 4) (c . 9) (h . 3) (h . 9) (s . 9) (c . 5) (d . 9) (c . 7) (c . 10) (h . 2) (s . 2) (s . 3) (d . 2) (c . 3) (h . 10) (s . 10) (d . 3) (h . 7) (h . 10) (h . 8) (h . 5) (s . 5) (s . 10) (s . 10) (d . 5) (c . 6) (s . 8) (c . 4) (d . 10) (c . 10) (h . 10) (s . 10) (d . 10))
; k: 1 deck size: 52 first card: (c . 1)
; k: 0 deck size: 45 first card: (d . 7)
;Value: 0

(test-strategy (watch-player (stop-at 16))
	       (watch-player (stop-at 20))
	       1)
; ((c . 1) (h . 1) (s . 1) (c . 3) (s . 7) (d . 7) (h . 4) (s . 4) (h . 3) (s . 3) (h . 9) (d . 3) (d . 10) (d . 10) (c . 5) (d . 1) (c . 2) (h . 2) (h . 5) (c . 8) (c . 10) (h . 10) (s . 10) (d . 4) (s . 5) (c . 4) (s . 9) (d . 9) (d . 10) (h . 8) (s . 8) (c . 10) (d . 8) (c . 10) (h . 10) (c . 10) (c . 9) (d . 5) (s . 10) (s . 2) (c . 6) (h . 6) (d . 2) (h . 10) (s . 10) (s . 6) (h . 10) (s . 10) (d . 10) (d . 6) (c . 7) (h . 7))
; k: 1 deck size: 52 first card: (c . 1)
; Hand's total: 1 Opponent up card: (c . 1), result: Hit
; Hand's total: 2 Opponent up card: (c . 1), result: Hit
; Hand's total: 5 Opponent up card: (c . 1), result: Hit
; Hand's total: 12 Opponent up card: (c . 1), result: Hit
; Hand's total: 19 Opponent up card: (c . 1), result: Stay
; Hand's total: 1 Opponent up card: (h . 1), result: Hit
; Hand's total: 5 Opponent up card: (h . 1), result: Hit
; Hand's total: 9 Opponent up card: (h . 1), result: Hit
; Hand's total: 12 Opponent up card: (h . 1), result: Hit
; Hand's total: 15 Opponent up card: (h . 1), result: Hit
; k: 0 deck size: 41 first card: (d . 3)
;Value: 1

(define (louis my-hand opponent-up-card)
  (let ((total (hand-total my-hand))
        (opponent-up-card-value (card-value opponent-up-card)))
    (cond ((< total 12) true)
          ((> total 16) false)
          ((= total 12) (< opponent-up-card-value 4))
          ((= total 16) (not (= opponent-up-card-value 10)))
          (else (> opponent-up-card-value 6)))))

(test-strategy (watch-player louis)
               (watch-player (stop-at 17))
               5)
; ((c . 1) (h . 1) (s . 1) (c . 9) (h . 9) (c . 10) (d . 1) (s . 10) (d . 10) (c . 10) (h . 10) (s . 7) (h . 10) (d . 7) (c . 8) (h . 8) (s . 9) (c . 3) (h . 3) (s . 3) (c . 2) (s . 4) (h . 2) (s . 2) (d . 2) (d . 3) (s . 10) (d . 10) (s . 8) (c . 5) (d . 9) (d . 8) (c . 10) (d . 4) (h . 10) (s . 10) (h . 5) (s . 5) (s . 10) (c . 10) (d . 10) (d . 10) (d . 5) (c . 6) (h . 10) (h . 6) (s . 6) (d . 6) (c . 7) (c . 4) (h . 4) (h . 7))
; k: 5 deck size: 52 first card: (c . 1)
; Hand's total: 1 Opponent up card: (c . 1), result: Hit
; Hand's total: 2 Opponent up card: (c . 1), result: Hit
; Hand's total: 11 Opponent up card: (c . 1), result: Hit
; Hand's total: 20 Opponent up card: (c . 1), result: Stay
; Hand's total: 1 Opponent up card: (h . 1), result: Hit
; Hand's total: 11 Opponent up card: (h . 1), result: Hit
; Hand's total: 12 Opponent up card: (h . 1), result: Hit
; k: 4 deck size: 44 first card: (d . 10)
; Hand's total: 10 Opponent up card: (d . 10), result: Hit
; Hand's total: 20 Opponent up card: (d . 10), result: Stay
; Hand's total: 10 Opponent up card: (c . 10), result: Hit
; Hand's total: 17 Opponent up card: (c . 10), result: Stay
; k: 3 deck size: 40 first card: (h . 10)
; Hand's total: 7 Opponent up card: (h . 10), result: Hit
; Hand's total: 15 Opponent up card: (h . 10), result: Hit
; k: 2 deck size: 36 first card: (s . 9)
; Hand's total: 3 Opponent up card: (s . 9), result: Hit
; Hand's total: 6 Opponent up card: (s . 9), result: Hit
; Hand's total: 9 Opponent up card: (s . 9), result: Hit
; Hand's total: 11 Opponent up card: (s . 9), result: Hit
; Hand's total: 15 Opponent up card: (s . 9), result: Hit
; Hand's total: 17 Opponent up card: (s . 9), result: Stay
; Hand's total: 9 Opponent up card: (c . 3), result: Hit
; Hand's total: 11 Opponent up card: (c . 3), result: Hit
; Hand's total: 13 Opponent up card: (c . 3), result: Hit
; Hand's total: 16 Opponent up card: (c . 3), result: Hit
; k: 1 deck size: 25 first card: (d . 10)
; Hand's total: 8 Opponent up card: (d . 10), result: Hit
; Hand's total: 13 Opponent up card: (d . 10), result: Hit
; k: 0 deck size: 21 first card: (d . 8)
;Value: 3

(test-strategy (stop-at 21) (stop-at 17) 100) ;;; run out of cards
; ((c . 1) (c . 10) (s . 2) (d . 2) (c . 5) (h . 1) (s . 1) (d . 1) (h . 7) (h . 5) (d . 10) (s . 8) (d . 8) (c . 9) (h . 9) (s . 5) (c . 2) (h . 10) (s . 10) (c . 3) (s . 7) (d . 5) (c . 6) (h . 6) (d . 7) (c . 10) (d . 10) (c . 10) (h . 10) (h . 10) (s . 10) (s . 10) (s . 6) (c . 8) (d . 6) (s . 9) (h . 3) (d . 10) (h . 8) (c . 7) (h . 2) (c . 10) (h . 10) (s . 10) (s . 3) (d . 10) (d . 9) (d . 3) (c . 4) (h . 4) (s . 4) (d . 4))
; k: 100 deck size: 52 first card: (c . 1)
; k: 99 deck size: 41 first card: (s . 8)
; k: 98 deck size: 37 first card: (s . 5)
; k: 97 deck size: 33 first card: (c . 3)
; k: 96 deck size: 28 first card: (d . 7)
; k: 95 deck size: 24 first card: (h . 10)
; k: 94 deck size: 20 first card: (s . 6)
; k: 93 deck size: 16 first card: (h . 3)
; k: 92 deck size: 12 first card: (h . 2)
; k: 91 deck size: 8 first card: (s . 3)
; k: 90 deck size: 4 first card: (c . 4)
; k: 89 deck size: 0 first card: NULL
;Value: 2

(test-strategy (stop-at 22) (stop-at 10) 100) ;;; player's strategy never win
; ((c . 1) (h . 1) (c . 2) (h . 2) (s . 2) (h . 10) (s . 7) (d . 2) (h . 5) (s . 5) (d . 5) (d . 7) (s . 10) (s . 9) (d . 9) (c . 10) (c . 8) (c . 10) (c . 4) (d . 10) (c . 6) (h . 10) (s . 10) (h . 10) (h . 4) (s . 4) (s . 1) (c . 3) (h . 6) (s . 6) (d . 1) (h . 8) (d . 6) (c . 7) (h . 3) (d . 4) (s . 10) (d . 10) (c . 10) (h . 7) (s . 3) (d . 3) (d . 10) (c . 5) (s . 8) (d . 8) (h . 10) (s . 10) (d . 10) (c . 10) (c . 9) (h . 9))
; k: 100 deck size: 52 first card: (c . 1)
; k: 99 deck size: 45 first card: (d . 2)
; k: 98 deck size: 40 first card: (s . 10)
; k: 97 deck size: 36 first card: (c . 8)
; k: 96 deck size: 32 first card: (c . 6)
; k: 95 deck size: 28 first card: (h . 4)
; k: 94 deck size: 20 first card: (d . 6)
; k: 93 deck size: 15 first card: (d . 10)
; k: 92 deck size: 10 first card: (d . 10)
; k: 91 deck size: 5 first card: (s . 10)
; k: 90 deck size: 1 first card: (h . 9)
; k: 89 deck size: 0 first card: NULL
;Value: 0