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

;;; This is the file ps6-mods.scm

;;; The MOBOT is a procedure

(define (make-mobot name my-status threshold limit-of-sight)
  (let ((visual-memory (make-map (make-map-rep)
				 (xcoord (position my-status))
				 (ycoord (position my-status))
				 (xcoord (position my-status))
				 (ycoord (position my-status))))
	(restlessness 0))
    (define (mobot m)
      (cond ((eq? m 'identity) name)
	    ((eq? m 'type) 'mobot)
	    ((eq? m 'position) (position my-status))
	    ((eq? m 'orientation) (orientation my-status))
	    ((eq? m 'move)
	     (set! restlessness (1+ restlessness))
	     (if (> restlessness threshold)
		 (sequence
		  (move-to (choose-direction))
		  (set! restlessness 0))))
	    ((eq? m 'go)
	     (set! restlessness (1+ restlessness))
	     (lambda (direction)
	       (move-to direction)))
	    (else (print "I don't know how to do this - make-mobot" m))))

;;; definitions internal to a Mobot

;;; Procedure that describes the movement of a Mobot

    (define (move-to direction)
      (turn! my-status direction name)     
      (let ((p-scene (look-around-world my-status limit-of-sight))
	    (m-scene (remember-scene visual-memory my-status)))
	(announce-scene name p-scene)
;;; The physical scene is compared with the remembered one.
	(let ((c (compare-scenes p-scene m-scene)))
;;; If surprised, raise an alarm.
	  (if (not (eq? c 'ok))
	      (alarm))
;;; In any case, update memory and continue patrol.
	  (update-map! visual-memory my-status p-scene)
	  (let ((old-position
		 (remember-position (position my-status))))
            (if (clear? p-scene) (walk! my-status old-position name))
	    (update-world! old-position (position my-status) mobot)))))

;;; I yell if I see something is different from how I remember it.

    (define (alarm)
	(print "Yaaaah-- I identified an UWO (unidentified wandering object)"))

;;; The line-of-sight description returned by REMEMBER-SCENE 
;;;  can be directly compared with a similar description returned
;;;  by LOOK-AROUND-WORLD.  Certain kinds of differences will cause the
;;; procedure to return alarm messages.

    (define (compare-scenes p-scene m-scene)
      (cond ((and (eq? (object p-scene) 'unknown)
		  (eq? (object m-scene) 'unknown))
	     'ok)                                   ;No perceptable difference.
	    ((or (and (eq? (object p-scene) 'unknown)
		      (> (distance p-scene) (distance m-scene)))
		 (and (eq? (object m-scene) 'unknown)
		      (> (distance m-scene) (distance p-scene)))
		 (and (not (eq? (object p-scene) 'unknown))
		      (not (eq? (object m-scene) 'unknown))
		      (or (not (= (distance p-scene) (distance m-scene)))
			  (not (eq? (object p-scene) (object m-scene))))))
	     'not-ok)
	    (else 'ok)))

;;; The MOBOT Visual Memory System
;;;  The MOBOT only considers the line-of-sight in the  direction
;;;   he is pointing.  He remembers the distance to either the 
;;;   first object in his line of sight or to the first place that
;;;   he has no information about its occupation.
;;;  Thus the description consists of two parts:
;;;    a number, representing the remepmbered distance (in steps)
;;;    the object at that distance
;;;     or UNKNOWN if there is no known object there.

    (define (remember-scene visual-memory status)
      (let ((my-place (position status))
	    (my-orientation (orientation status)))
	(define (loop place steps)
	  (let ((thing (visual-memory 'look! place)))
	    (if (eq? thing 'empty)
		(loop (add-vectors place my-orientation)
		      (+ steps 1))
		(construct-scene steps thing))))
	(loop (add-vectors my-place my-orientation) 1)))

;;; The Visual Memory System must be updated to reflect any new 
;;;  observations.  An observation is along a line of sight specified
;;;  by the orientation of the MOBOT as stored in his status.  

    (define (update-map! visual-memory status observed-scene)
      (let ((my-place (position status))
	    (my-orientation (orientation status)))
	(define (loop place steps)
	  (if (= steps (distance observed-scene))
	      (if (not (eq? (object observed-scene) 'unknown))
		  ((visual-memory 'store! place) (object observed-scene)))
	      (sequence ((visual-memory 'store! place) 'empty)
			(loop (add-vectors place my-orientation)
			      (+ steps 1)))))
	(loop my-place 0)))

;;; We put the new mobot on the queue in the world
    (enqueue mobot)
;;; We return the message passing procedure after we created the mobot
    mobot))

;;; ************************************************************************

;;; A moving foreign-object is also a procedure

(define (make-foreign-object name status threshold)
  (let ((restlessness 0)
	(possessions '())
	(limit-of-sight limit-of-world))
    (define (me m)
      (cond ((eq? m 'identity) name)
	    ((eq? m 'type) 'foreign-obj)
	    ((eq? m 'position) (position status))
	    ((eq? m 'orientation) (orientation status))
	    ((eq? m 'look-around) (look-around-world status limit-of-sight))
	    ((eq? m 'got-quiz?) (member? 'quiz2 possessions))
	    ((eq? m 'move)
	     (set! restlessness (1+ restlessness))
	     (if (> restlessness threshold)
		 (sequence
		  (move-to (choose-direction))
		  (set! restlessness 0))))
	    ((eq? m 'go)
	     (set! restlessness (1+ restlessness))
	     (lambda (direction)
	       (move-to direction)))
	    ((eq? m 'take)
	     (lambda (object)
	       (if (eq? (object 'type) 'thing)
		   (if (= (distance (look-around-world status limit-of-sight))
			  1)
		       (sequence
			(print name)
			(princ " took ")
			(princ (object 'identity))
			(set! possessions (cons object possessions))
			((object 'change-possessor) me)
			'taken)
		       (error "Thing taken not close enough"))
		   (error "Not a thing" object))))
	    ((eq? m 'lose)
	     (lambda (thing)
	       (set! possessions
		     (delete thing possessions))
	       ((thing 'change-possessor) 'no-one)
	       (print name)
	       (princ " lost ")
	       (princ (thing 'identity))))
	    ((eq? m 'list-possessions)
	     (for-each (lambda (thing)
			 (print (thing 'identity)))
		       possessions))
	    ((eq? m 'go-to-jail)
	     (set-position! status jail-position)
	     (update-world! (position status) jail-position me)
	     (print name)
	     (princ " went to jail"))
	    (else (error "I don't know how to do this- make-foreign-obj" m))))

;;; Definitions internal to a foreign object
    
;;; Procedure that describes the movement of a foreign object
    
    (define (move-to direction)
      (turn! status direction name)
      (let ((p-scene (look-around-world status limit-of-sight))
	    (old-position (remember-position (position status))))
	(announce-scene name p-scene)
	(if (not (or (eq? (object p-scene) 'empty)
		     (eq? (object p-scene) 'unknown)))
	    (if (and (eq? ((object p-scene) 'identity) 'quiz2)
		     (= (distance p-scene) 1))
		(sequence
		 ((me 'take) (object p-scene))
		 (walk! status old-position name))
		(if (clear? p-scene) (walk! status old-position name)))
	    (if (clear? p-scene) (walk! status old-position name)))
	(update-world! old-position (position status) me)))
    
;;; We put the new foreign object on the queue in the world
    (enqueue me)
;;; We return the message passing procedure after we created the foreign-object
    me))

;;; ***************************************************************************

;;; The "real world" simulation
;;; Setting up global variables for initialization

(define init-status-abbot nil)
(define init-status-moe nil)
(define jail-position nil)
(define dummy-status nil)
(define mobot-list nil)
(define limit-of-world nil)
(define q nil)
(define w nil)
(define f nil)
(define e nil)
(define world nil)
(define queue nil)
(define abbot nil)
(define moe nil)

;;; The initialization procedure 

(define (initialize)
  (print "INITIALIZING..............")
  (set! init-status-abbot (make-status-descriptor (make-vector 13 16)
						  (make-vector 0 -1)))
  (set! init-status-moe (make-status-descriptor (make-vector 14 15)
						(make-vector 0 1)))
  (set! jail-position (make-vector 0 17))
  (set! dummy-status (make-status-descriptor jail-position
					     (make-vector 0 0)))
  (set! mobot-list
	'(elliot stevie arnold mickey ernie donald evan charlie albert))
  (set! limit-of-world 18)
  (set! q (make-thing 'quiz2))
  (set! w (make-thing 'wall))
  (set! f (make-thing 'fence))
  (set! e 'empty)
;;; Please look at this 2 dimensional array sideways, i.e., the x axis
;;; should be the counter of the columns
  (set! world
	(make-map (list '*columns*
			(list '*rows* f f f f f f f f f f f f f f f f f f f f)
			(list '*rows* f e e e e e e e w e e e e e e e e f e f)
			(list '*rows* f e e e e e e e e e e e e e e e e f f f)
			(list '*rows* f e e e e e e e e e w e w e e w e e e f)
			(list '*rows* f e e w w w e e e e e e e e e w e e e f)
			(list '*rows* f e e e e e e e e e e e e e e w e e e f)
			(list '*rows* f e e e e e e e e e w w e e e w e e e f)
			(list '*rows* f e e e w e e e w e e w e e e e e e e f)
			(list '*rows* f e e e w e e e e e e e e e e e e e e f)
			(list '*rows* f e e e e e e e e e e e e w e e e e e f)
			(list '*rows* f e e e e e e e w e e w e w e e e e e f)
			(list '*rows* f e e w w e e e e e e e e e e e e e e f)
			(list '*rows* f e e e e e e e e e e e e e e e w e e f)
			(list '*rows* f e e e e e w e e e e e e e e e w e e f)
			(list '*rows* f e e e e e w e e e e e e e e e q e e f)
			(list '*rows* f e e e w e w e e e e e w w e e e w e f)
			(list '*rows* f e e e e e e e e e e e w w e e e e e f)
			(list '*rows* f e e e e e e e e e e e e e e e e e e f)
			(list '*rows* f e e e e e e e e e e e e e e e e e e f)
			(list '*rows* f f f f f f f f f f f f f f f f f f f f))
		  -1 -1 18 18))
;;; initializing the two mobots in the world
  (set! queue nil)
  (set! abbot (make-mobot 'abbot init-status-abbot 1 3))
  (update-world! (position dummy-status)
		 (position init-status-abbot)
		 abbot)
  (set! moe (make-mobot 'moe init-status-moe 2 3))
  (update-world! (position dummy-status)
		 (position init-status-moe)
		 moe)
  (print "THE WORLD IS INITIALIZED"))
;;; Finally, we use a clock and a queue to simulate the world

(define (clock)
  (for-each move queue))
(define (enqueue object)
  (set! queue (cons object queue))
  'enqueued)
(define (move motional-object)
  (motional-object 'move))
