;;;		     MASSACHUSETTS INSTITUTE OF TECHNOLOGY
;;;	   Department of Electrical Engineering and Computer Science
;;;	   6.001---Structure and Interpretation of Computer Programs
;;;			      Fall Semester, 1989
;;;				 Problem Set 6
;;;
;;;			    Code file PS6-ADV.SCM


;;; ----------------------------------------------------------------------------
;;; Simple object system with inheritance

(define (ask object message . args)
  (let ((method (get-method object message)))
    (if (method? method)
	(apply method (cons object args))
	(error "No method" message (cadr method)))))

(define (get-method object message)
  (object message))

(define (no-method name)
  (list 'no-method name))

(define (no-method? x)
  (if (pair? x)
      (eq? (car x) 'no-method)
      false))

(define (method? x)
  (not (no-method? x)))


;;; ----------------------------------------------------------------------------
;;; Persons, places, and possessions will all be kinds of named objects

(define (make-named-object name)
  (lambda (message) 
    (cond ((eq? message 'name)
	   (lambda (self) name))
	  (else (no-method name)))))

;;; A thing is something that can be owned

(define (make-thing name)
  (let ((n-o (make-named-object name))
	(owner 'no-one))
    (lambda (message)
      (cond ((eq? message 'ownable?)
	     (lambda (self) true))
	    ((eq? message 'owned?)
	     (lambda (self)
	       (not (eq? owner 'no-one))))
	    ((eq? message 'change-owner)
	     (lambda (self new-owner)
	       (set! owner new-owner)
	       'done))
	    ((eq? message 'owner)
	     (lambda (self) owner))
	    (else
	     (get-method n-o message))))))

;;; Implementation of places

(define (make-place name)
  (let ((n-o (make-named-object name))
	(neighbor-map '())
	(things '()))
    (lambda (message)
      (cond ((eq? message 'things)
	     (lambda (self) things))
	    ((eq? message 'neighbors)
	     (lambda (self) (mapcar cdr neighbor-map)))
	    ((eq? message 'exits)
	     (lambda (self) (mapcar car neighbor-map)))
	    ((eq? message 'neighbor-towards)
	     (lambda (self direction)
	       (let ((p (assq direction neighbor-map)))
		 (if (null? p) false (cdr p)))))
	    ((eq? message 'appear)
	     (lambda (self new-thing)
	       (if (memq new-thing things)
		   (append (list (ask new-thing 'name))
			   '(is already at)
			   (list name))
		   (sequence (set! things (cons new-thing things))
			     'appeared))))
	    ((eq? message 'gone)
	     (lambda (self thing)
	       (if (not (memq thing things))
		   (append (list (ask thing 'name))
			   '(is not at)
			   (list name))
		   (sequence (set! things (delq thing things))
			     'disappeared))))
	    ((eq? message 'new-neighbor)
	     (lambda (self direction new-neighbor)
	       (if (assq direction neighbor-map)
		   (append '(direction already assigned) (list direction name))
		   (sequence
		     (set! neighbor-map
			   (cons (cons direction new-neighbor) neighbor-map))
		     'connected))))
	    (else (get-method n-o message))))))

;;; Implementation of people

(define (make-person name place threshold)
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
	    (else (get-method n-o message))))))

;;; A troll is a kind of person (but not a kind person!)

(define (make-troll name place threshold)
  (let ((p (make-person name place threshold)))
    (lambda (message)
      (cond ((eq? message 'act)
	     (lambda (self)
	       (let ((others (other-people-at-place
			      self
			      (ask self 'place))))
		 (if (not (null? others))
		     (ask self 'eat-person (pick-random others))
		     ((get-method p 'act) self)))))
	    ((eq? message 'eat-person)
	     (lambda (self person)
	       (ask self 'say
		    (append '("Growl.... I'm going to eat you,")
			    (list (ask person 'name))))
	       (go-to-heaven person)
	       (ask self 'say
		    (append '("Chomp chomp.") (list (ask person 'name))
			    '("tastes yummy!")))
	       'eaten))
	    (else (get-method p message))))))

(define (go-to-heaven person)
  (for-each (lambda (item) (ask person 'lose item))
	    (ask person 'possessions))
  (ask person 'say '("It is a far, far, better place I go to!"))
  (ask person 'move-to heaven)
  (remove-from-clock-list person)
  'dead)

;;; --------------------------------------------------------------------------
;;; Clock routines

(define *clock-list* '())

(define (initialize-clock-list)
  (set! *clock-list* '())
  'initialized)

(define (add-to-clock-list person)
  (set! *clock-list* (cons person *clock-list*))
  'added)

(define (remove-from-clock-list person)
  (set! *clock-list* (delq person *clock-list*))
  'removed)

(define (clock)
   (for-each (lambda (person) (ask person 'move))
	     *clock-list*))
	     

(define (run-clock n)
  (if (= n 0)
      'done
      (sequence (clock) (run-clock (- n 1)))))
;;; --------------------------------------------------------------------------
;;; Miscellaneous procedures

(define (is-a object type)
  (let ((m (get-method object type)))
    (if (method? m)
	(ask object type)
	false)))

(define (install-person person)
    (ask (ask person 'place) 'appear person)
    (add-to-clock-list person)
    'installed)

(define (other-people-at-place person place)
  (filter (lambda (object)
	    (if (not (eq? object person))
		(is-a object 'person?)
		false))
	  (ask place 'things)))

(define (greet-people person people)
  (if (not (null? people))
      (ask person 'say
	   (append '("Hi")
		   (mapcar (lambda (p) (ask p 'name))
			   people)))))

(define (display-message list-of-stuff)
  (newline)
  (for-each (lambda (s) (princ s) (princ " "))
	    list-of-stuff))

(define (announce-move name old-place new-place)
  (display-message
   (list name "moves from" (ask old-place 'name)
	 "to" (ask new-place 'name))))

(define (random-place old-place)
  (pick-random (ask old-place 'neighbors)))

(define (filter predicate list)
  (cond ((null? list) '())
	((predicate (car list))
	 (cons (car list) (filter predicate (cdr list))))
	(else (filter predicate (cdr list)))))


(define (pick-random list)
  (if (null? list)
      '()
      (list-ref list (random (length list)))))

(define (delq item list)
  (cond ((null? list) '())
	((eq? item (car list)) (delq item (cdr list)))
	(else (cons (car list) (delq item (cdr list))))))

;; FOR-EACH is another (preferred) name for the primitive
;; procedure MAPC.

(define for-each mapc)
