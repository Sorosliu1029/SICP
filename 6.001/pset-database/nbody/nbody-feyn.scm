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

;;; This is the file nbody-feyn.scm

(define G 6.6372E-8)                                    ;cm^3/(gm sec^2)
(define dt .1)

(define feyn
  (make-universe 0
                 (list (make-body (/ 1 G)               ;mass
                                  (make-3vector 0 0 0)  ;position
                                  (make-3vector 0 0 0)) ;velocity
                       (make-body 0
                                  (make-3vector .5 0 0)
                                  (make-3vector 0 1.63 0)))))


;;; The following are universes resulting from running the FEYN
;;;universe with step size dt = .001.  These yield very precise
;;;computations of the answer, which we will use as a standard 
;;;for comparison.


;;; For end time = .5

(define feyn-05
  '(-5.0E-4
    0.5
    ((0 0 0) (0 0 0) (0 0 0) 15066594.3)
    ((0.115725969 0.61542946 0)
     (-1.20562229 0.63101266 0)
     (-7100261.8 -37759116 0)
     0)))


;;; For end time = 1

(define feyn-1
  '(-5.0E-4
    1
    ((0 0 0) (0 0 0) (0 0 0) 15066594.3)
    ((-0.46427083 0.67192157 0)
     (-1.00988588 -0.29387135 0)
     (12840216.1 -18583158 0)
     0)))


;;; For end time = 2

(define feyn-2
  '(-5.0E-4
    2
    ((0 0 0) (0 0 0) (0 0 0) 15066594.3)
    ((-0.98892439 0.0150912958 0)
     (-0.0192320147 -0.823834196 0)
     (15400585.3 -235017.754 0)
     0)))
