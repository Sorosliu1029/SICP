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


;;; PS3.SCM							Page 1

;;; This file provides an abstraction abstraction.  It is not necessary to
;;; understand the code to do the problem set.  However you will have to use
;;; it.  There are three components:
;;;
;;; make-abstraction	automatically builds a data abstraction
;;;
;;; make-instance	takes an abstraction and some values for components
;;;			and returns an instance of that abstraction
;;;
;;; select		is a general purpose selector to access components
;;;			of an abstraction instance
;;;
;;; An abstraction has a number of named fields or components.  For instance
;;;
;;; (define foo (make-abstraction 'a 'b 'c))
;;;
;;; defines an abstraction which has three components, named a, b, and c.
;;; [The quote marks ' are necessary whenever we refer to a field name.
;;;  We will learn about these later in the course.]
;;;
;;; (define foo1 (make-instance foo 'b 3 'a 2 'c 7))
;;;
;;; builds an instance of foo, whose components for [a,b,c] are [2,3,7].
;;; Notice that we can specify the component names in any order to make-instance
;;; so long as we name each one.  We must pass make-instance the abstraction
;;; we are using. It is the first argument.
;;;
;;; ==> (select foo foo1 'a)
;;; 2
;;; ==> (select foo foo1 'b)
;;; 3
;;; ==> (select foo foo1 'c)
;;; 7
;;;
;;; We can see that select lets us get at any component of an instance of foo.
;;;
;;; We can specify default values for components too.
;;;
;;; (define bar (make-abstraction 'u '(v 10) '(w 3) 'x))
;;;
;;; says that unless we explicitly give values for v and w, they will default to
;;; 10 and 3 respectively.  The default default, for u and x, is 0.
;;;
;;; ==> (define bar1 (make-instance bar 'x 5 'v 7))
;;; BAR1
;;; ==> (select bar bar1 'u)
;;; 0
;;; ==> (select bar bar1 'v)		;we overrode the default
;;; 7
;;; ==> (select bar bar1 'w)
;;; 3
;;; ==> (select bar bar1 'x)
;;; 5


;;; PS3.SCM							Page 2

;;; the code

;;; a fully formed field specification looks like: (<fieldname> <defaultvalue>)
;;; where <fieldname> is a symbol and <defaultvalue> is a number

(define (fully-formed-fieldname? fieldname)
  (if (pair? fieldname)
      (if (pair? (cdr fieldname))
	  (and (symbol? (car fieldname))
	       (null? (cddr fieldname))
	       (number? (cadr fieldname)))
	  nil)
      nil))

;;; an abstraction is a list (abstraction <fffieldname1> <fffieldname2> ...).

(define (abstraction? abstraction)
  (if (pair? abstraction)
      (eq? (car abstraction) 'abstraction)
      nil))

;;; given a spec = a list of fffieldnames, an instance = a list of values,
;;; and a fieldname, this CDRs down the two lists until it finds a field
;;; name which matches the requested field.  the first two cond clauses handle
;;; error conditions.

(define (find-field-value spec instance fieldname)
  (cond ((null? spec)
	 (if (null? instance)
	     (error "Abstraction doesn't include field:" fieldname)
	     (error "Not a correct instance type")))
	((null? instance)
	 (error "Not a correct instance type"))
	((eq? fieldname (caar spec))
	 (car instance))
	(else (find-field-value (cdr spec)
				(cdr instance)
				fieldname))))

;;; user level procedures

;;; to make an abstraction we sanitize all the fieldnames so they are fully
;;; formed with 0 as the default default

(define (make-abstraction . fields)
  (cons 'abstraction
	(mapcar (lambda (fieldname)
		  (cond ((symbol? fieldname) (list fieldname 0))
			((fully-formed-fieldname? fieldname)
			 fieldname)
			(else
			 (error "Badly formed fieldname:" fieldname))))
		fields)))

;;; to make an instance we check whether each field name is supplied and if
;;; not then use the default value.  we rely on memq returning the rest of a list
;;; when it finds what we are looking for.

(define (make-instance abstraction . specs)
  (if (abstraction? abstraction)
      (mapcar (lambda (field)
		(let ((name (car field))
		      (default (cadr field)))
		  (let ((specified (memq name specs)))
		    (if (null? specified)
			default
			(cadr specified)))))
	      (cdr abstraction))
      (error "Invalid abstraction:" abstraction)))

;;; to select a field value we iterate down the abstraction description and the
;;; instance looking for a matching fieldname.

(define (select abstraction instance fieldname)
  (if (abstraction? abstraction)
      (find-field-value (cdr abstraction) instance fieldname)
      (error "Invalid abstraction:" abstraction)))

