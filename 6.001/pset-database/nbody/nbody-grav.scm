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

;;; This is the file nbody-gravity.scm
;;; Particles and fields. 


;;; The following procedure computes (X1-X2)/(|X1-X2|)^3 for two
;;;position vectors X1, and X2.

(define (ror3 x1 x2)
  (let ((dx (sub x1 x2)))
    (let ((r (magnitude dx)))
      (scale (/ 1 r r r) dx))))


;;; Given a list of bodies, the following procedure computes the
;;;fields between the bodies pairwise and accumulates a resultant
;;;for each body.  

(define (accumulate-fields bodies)
  ;; Clear out fields accumulated on last cycle of computation.
  (for-each bodies
            (lambda (body)
              (set-field! body zero)))
  ;; For each pair of bodies, each body is accelerated by the other
  ;;by an amount proportional to the mass of the other.
  (for-pairs bodies
             (lambda (pi pj)
               (let ((u (ror3 (position pj) (position pi))))
                 (set-field! pi
                             (add (field pi)
                                  (scale (mass pj) u)))
                 (set-field! pj
                             (sub (field pj)
                                  (scale (mass pi) u)))))))


;;; We update the state of each body, using the computed field to
;;;extrapolate the velocity, and using the extrapolated velocity to
;;;extrapolate the position.  We assume here that the state is
;;;defined with the velocity lagging the position by 1/2 dt.

(define (update-state bodies dt)
  (for-each bodies
            (lambda (body)
              (set-velocity! body
                             (add (velocity body)
                                  (scale (* G dt)
                                         (field body))))
              (set-position! body
                             (add (position body)
                                  (scale dt
                                         (velocity body)))))))


;;; The following code is used to set up the offset in the phase of
;;;the velocity computation from the position computation.  For
;;;example, to make the phase of the velocities lag the phase of the
;;;positions by 1/2 step, we say (OFFSET-VELOCITIES U (* -0.5 DT)).
;;;The universe may already be in an offset state, thus the offset
;;;change must be computed relative to the existing offset.

(define (offset-velocities universe target-offset)
  (let ((change-in-offset (- target-offset
                             (velocity-offset universe))))
    (for-each (bodies universe)
              (lambda (body)
                (set-velocity! body
                               (add (velocity body)
                                    (scale (* G change-in-offset)
                                           (field body)))))))
  (set-velocity-offset! universe target-offset))

(define (run universe dt endtime)
  (let ((bodies (bodies universe))
        (e (- endtime (/ dt 2))))
    (define (loop)
      (if (> e (time universe))
          (sequence (update-state bodies dt)
                    (accumulate-fields bodies)
		    (set-time! universe
                               (+ (time universe)
                                  dt))
                    (loop))
          'done))
    (accumulate-fields bodies)
    (offset-velocities universe (* -0.5 dt))
    (loop)))


;;; If the system has already been initialized then the velocities lag
;;;the positions by dt/2.  Thus, to reverse the system so that the
;;;reverse is also set up with the velocities lagging, we first
;;;reverse the current offset, before reversing the velocities.
;;;Then the reversed system will have the velocities lag.  Time is
;;;also inverted.

(define (reverse-system universe)
  (let ((old-offset (velocity-offset universe)))
    (offset-velocities universe (* -1 old-offset))
    (set-velocity-offset! universe old-offset)
    (for-each (bodies universe)
              (lambda (body)
                (set-velocity! body
                               (sub zero (velocity body)))))
    (set-time! universe (- 0 (time universe))))
  (time universe))


;;; Representations

;;; of Universe -- contains VELOCITY-OFFSET, TIME, and BODIES list.
                                                
(define (make-universe time bodies)
  (cons 0 (cons time bodies)))
(define (velocity-offset universe) (car universe))
(define (time universe) (cadr universe))
(define (bodies universe) (cddr universe))

(define (set-velocity-offset! universe o)
  (set-car! universe o))
(define (set-time! universe time)
  (set-car! (cdr universe) time))


;;; of Bodies

(define (make-body mass position velocity)
  (list position velocity zero mass))

(define (position body) (car body))
(define (velocity body) (cadr body))
(define (field body) (caddr body))
(define (mass body) (cadddr body))

(define (set-position! body position)
  (set-car! body position))
(define (set-velocity! body velocity)
  (set-car! (cdr body) velocity))
(define (set-field! body field)
  (set-car! (cddr body) field))


