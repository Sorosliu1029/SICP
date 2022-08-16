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

;;;		     MASSACHUSETTS INSTITUTE OF TECHNOLOGY
;;;	   Department of Electrical Engineering and Computer Science
;;;	   6.001---Structure and Interpretation of Computer Programs
;;;			      Fall Semester, 1989
;;;				 Problem Set 6
;;;
;;;			    Code file PS6-WORLD.SCM


;;;
;;; You can add more definitions here to make them part of the world.


(initialize-clock-list)

;; Here we define the places in our world

(define Bldg-36 (make-place 'Bldg-36))
(define gerry-office (make-place 'gerry-office))
(define nikhil-office (make-place 'nikhil-office))
(define Tech-Square (make-place 'Tech-Square))
(define computer-lab (make-place 'computer-lab))
(define EGG-Atrium (make-place 'EGG-Atrium))
(define Bldg-10 (make-place 'Bldg-10))
(define dormitory (make-place 'dormitory))
(define heaven (make-place 'heaven))
(define dungeon (make-place 'dungeon))
(define dean-office (make-place 'dean-office))

;; One-way paths connect individual places in the world.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to))

(can-go Bldg-36 'up computer-lab)
(can-go Bldg-36 'north Tech-Square)
(can-go Bldg-36 'west EGG-Atrium)
(can-go Tech-Square 'south Bldg-36)
(can-go Tech-Square 'up gerry-office)
(can-go gerry-office 'down Tech-Square)
(can-go computer-lab 'up nikhil-office)
(can-go nikhil-office 'down computer-lab)
(can-go computer-lab 'down Bldg-36)
(can-go dormitory 'east Bldg-10)
(can-go Bldg-10 'west dormitory)
(can-go Bldg-10 'north EGG-Atrium)
(can-go dungeon 'up EGG-Atrium)
(can-go EGG-Atrium 'south Bldg-10)
(can-go EGG-Atrium 'east Bldg-36)
(can-go dean-office 'west dormitory)
(can-go dean-office 'down Bldg-10)

;; We define persons as follows:


(define gerry (make-person 'gerry nikhil-office 2))
(install-person gerry)

(define nikhil (make-person 'nikhil nikhil-office 3))
(install-person nikhil)

(define grendel (make-troll 'grendel dungeon 5))
(install-person grendel)

(define myself (make-person 'person dormitory 100))
(install-person myself)

(define late-homework (make-thing 'late-homework))
(ask dormitory 'appear late-homework)

;(define grendel-fu (make-troll 'grendel-fu dungeon 5))
;(install-person grendel-fu)

;(define troll1 (make-troll 'grendel dungeon 5))
;(install-person troll1)

;(define gerry1 (make-person 'gerry gerry-office 2))
;(install-person gerry1)

;;;;;;;;;;;;;
;; Problem 7
(define (make-beer)
  (let ((p (make-thing 'beer)))
    (lambda (message)
      (cond ((eq? message 'beer?) (lambda (self) true))
            (else (get-method p message))))))


;;;;;;;;;;;
;; Problem 8
(define (make-dean name place threshold)
  (let ((p (make-person name place threshold)))
    (lambda (message)
      (cond ((eq? message 'act)
	     (lambda (self)
	       (let ((place (ask self 'place)))
		 (let ((beers (filter (lambda (object) (is-a object 'beer?))
				      (ask place 'things))))
		   (if (not (null? beers))
		       (sequence (ask self 'say
				      '("I will not tolerate beer on campus!"))
				 (for-each (lambda (beer)
					     (smash-beer beer place)) 
					   beers)
				 (ask self 'say '("I have smashed the beer.")))
		       ((get-method p 'act) self))))))
	     (else (get-method p message))))))

(define (smash-beer beer place)
  (if (ask beer 'owned?)
      (let ((owner (ask beer 'owner)))
        (ask owner 'lose beer)
        (ask owner 'have-fit))
      'done)
  (ask place 'gone beer)
  'smashed)

(define shirley (make-dean 'shirley nikhil-office 2))
(install-person shirley)

(define beer1 (make-beer))
(define beer2 (make-beer))
(ask nikhil-office 'appear beer1)
(ask nikhil-office 'appear beer2)
(ask nikhil 'take beer1)
(ask gerry 'take beer2)
(ask gerry 'list-possessions)
(ask nikhil 'list-possessions)

;;;;;;;;;;;;;
;;  Problem 10
(define (make-party-troll name place threshold)
  (let ((p (make-troll name place threshold)))
    (lambda (message)
      (cond ((eq? message 'eat-person)
	     (lambda (self person)
	       (let ((pizzas-in-hand                   ; find all the pizzas
		      (filter (lambda (object)         ; that person has
				(if (eq? (ask object 'name)
					 'pizza)
				    true
                                    false))
                              (ask person 'possessions))))
                 (if (null? pizzas-in-hand)  ; no pizzas, tough luck for person
                     (sequence 
                       (ask self 'say
                            (append 
                             '("What?? No pizzas? Growl.... I'm going to eat you,")
                             (list (ask person 'name))))
                       (go-to-heaven person)
		       (ask self 'say
			    (append '("Chomp chomp.") (list (ask person 'name))
				    '("tastes almost as good as a pizza!")))
		       'eaten)
                     (let ((pizza (car pizzas-in-hand))) ; person has a pizza
                       (ask self 'say
                            (append
                             '("Growl.... I will eat your pizza instead,")
                             (list (ask person 'name))
                             '(" ... Chomp, chomp, burp!")))
                       (ask person 'lose pizza)          ; pizza must be lost
                       (ask place 'gone pizza))))))      ; and gone
            (else (get-method p message))))))

;;test filter
;; tests OK
(define (pizzas? person)
  (filter (lambda (object)
	    (if (eq? (ask object 'name)
		     'pizza)
		true
		false))
	  (ask person 'possessions)))


(define spuds (make-party-troll 'spuds nikhil-office 3))
(install-person spuds)


;;;;;;;;;;
;; Magic wand
;; is a kind of ownable thing, but with special properties
;; we want to place INVOKE actually in with PERSON, and then pass it off
;; to the method here
;;
(define (make-wand power)
  (let ((thg (make-thing 'wand))
	(invokable? false))
    (lambda (message)
      (cond ((eq? message 'shazam)
	     (lambda (self)
	       (set! invokable? true)))
	    ((eq? message 'invoke)
	     (lambda (self)
	       (let ((person-name (ask (ask self 'owner) 'name))
		     (person (ask self 'owner)))
		 (cond ((not invokable?)
			(display-message '("Sorry, I can't be used yet")))
		       ((eq? person-name 'no-one)
			(display-message '("Sorry, nobody owns me")))
		       ((> power 0)
			(display-message
			 (append
			  '("The wand is magically sending you to the EGG-ATRIUM,")
			  (list person-name)))
			(ask person 'move-to egg-atrium)
			(set! power (-1+ power))
			'invoked)
		       (else 
			(display-message '("Sorry, I can't be used now")))))))
	    (else (get-method thg message))))))


;;;;
;; Modifications to make-person to add INVOKE for WAND
;;
(define (make-person-new name place threshold)
  (let ((possessions '())
	(restlessness 0)
	(n-o (make-named-object name)))
    (lambda (message)
      (cond ((eq? message 'person?) (lambda (self) true))
	    ((eq? message 'place) (lambda (self) place))
	    ((eq? message 'possessions) (lambda (self) possessions))
	    ((eq? message 'exits) (lambda (self) (ask place 'exits)))
	    ((eq? message 'move)
	     (lambda (self)
	       (if (>= restlessness threshold)
		   (sequence (ask self 'act)
			     (set! restlessness 0))
		   (set! restlessness (1+ restlessness)))
	       'moved))
	    ((eq? message 'say)
	     (lambda (self list-of-stuff)
	       (display-message
		(append '("At") (list (ask place 'name)) '(":")
			(list name) '("says --") list-of-stuff))))
	    ((eq? message 'look-around)
	     (lambda (self)
	       (let ((other-things
		      (mapcar (lambda (thing) (ask thing 'name))
			      (delq self (ask place 'things)))))
		 (ask self 'say
		      (append '("I see")
			      (if (null? other-things)
				  '("nothing")
				  other-things))))))
	    ((eq? message 'take)
	     (lambda (self thing)
	       (if (if (memq thing (ask place 'things))
		       (is-a thing 'ownable?)
		       false)
		   (let ((owner (ask thing 'owner)))
		     (if (ask thing 'owned?)
			 (sequence (ask owner 'lose thing)
				   (ask owner 'have-fit))
			 'unowned)
		     (ask thing 'change-owner self)
		     (set! possessions (cons thing possessions))
		     (ask self 'say
			  (append '("I take") (list (ask thing 'name)))))
		   (append '(you can not take) (list (ask thing 'name))))))
	    ((eq? message 'lose)
	     (lambda (self thing)
	       (if (eq? self (ask thing 'owner))
		   (sequence
		     (set! possessions (delq thing possessions))
		     (ask thing 'change-owner 'no-one)
		     (ask self 'say
			  (append '("I lose") (list (ask thing 'name)))))
		   (append (list name) '(does not own) (ask thing 'name)))))
	    ((eq? message 'list-possessions)
	     (lambda (self)
	       (ask self 'say
		    (append '("I have")
			    (mapcar (lambda (p) (ask p 'name)) possessions)))))
	    ((eq? message 'go)
	     (lambda (self direction)
	       (let ((new-place (ask place 'neighbor-towards direction)))
		 (if (not (null? new-place))
		     (ask self 'move-to new-place)
		     (append '(you cannot go) (list direction) '(from)
			     (list (ask place 'name)))))))
	    ((eq? message 'act)
	     (lambda (self)
	       (let ((new-place (random-place place)))
		 (if (not (null? new-place))
		     (ask self 'move-to new-place)))))
	    ((eq? message 'have-fit)
	     (lambda (self)
	       (ask self 'say '("Yaaaah! I am upset!"))))
	    ((eq? message 'move-to)
	     (lambda (self new-place)
	       (announce-move name place new-place)
	       (ask place 'gone self)
	       (for-each (lambda (p)
			   (ask place 'gone p)
			   (ask new-place 'appear p))
			 possessions)
	       (ask new-place 'appear self)
	       (set! place new-place)
	       (greet-people self (other-people-at-place self new-place))
	       'moved))
;; new method for moving on
;; this seems to work if called directly
	    ((eq? message 'go-to-heaven)
	     (lambda (self)
	       (for-each (lambda (item) (ask self 'lose item))
			 (ask self 'possessions))
	       (ask self 'say '("It is a far, far, better place I go to!"))
	       (ask self 'move-to heaven)
	       (remove-from-clock-list self)))
;;end of new method
;;
;; new method for INVOKE for some object, like a WAND
;; we check that SELF owns the object first, then pass
;; the duty off to the object itself
	    ((eq? message 'invoke)
	     (lambda (self object)
		 (if (and (eq? (ask object 'name)
			       'wand)
			  (eq? (ask object 'owner)
			       self))
		     (ask object 'invoke)
		     (display-message
		      (append '("You can't use this")
			      (list (ask object 'name)))))))
;; end of new method INVOKE
	    (else (get-method n-o message))))))


;;;;;;;;;;;;;
