From bh%anarres.Berkeley.EDU@berkeley.edu Mon Dec 11 15:29:20 1989
Return-Path: <bh%anarres.Berkeley.EDU@Berkeley.EDU>
Date: Mon, 11 Dec 89 11:47:20 PST
From: bh%anarres.Berkeley.EDU@berkeley.edu (Brian Harvey)
To: hal
Subject: logo/logo-meta.soln.scm

;;; logo-meta.scm      Part of programming project #3

;;; Differences between the book and this version:  Eval and apply have
;;; been changed to logo-eval and logo-apply so as not to overwrite the Scheme
;;; versions of these routines. An extra procedure initialize-logo has been
;;; added. This routine resets the global environment and then executes the
;;; driver loop. This procedure should be invoked to start the metacircular
;;; evaluator executing.  Note: It will reset your global environment and all
;;; definitions to the Logo interpreter will be lost. To restart the Logo
;;; interpreter without resetting the global environment, just invoke
;;; driver-loop.  Don't forget that typing control-C twice will get you out of
;;; the metacircular evaluator back into Scheme.

;;; Problems 1, 2, and 6 are entirely in logo.scm
;;; Problems 4, 5, and 7 require you to find and change existing procedures.

;;;  Procedures that you must write from scratch:

;;; Problem 3    eval-line

(define (eval-line line-obj env)
  (if (ask line-obj 'empty?)
      '=no-value=
      (let ((value (logo-eval line-obj env)))
	(if (eq? value '=no-value=)
       	    (eval-line line-obj env)
	    value))))


;;; Problem 5    variables  (other procedures must be modified, too)
;;; data abstraction procedures

(define (variable? exp)
  (and (symbol? exp) (eq? (first exp) ':)))

(define (variable-name exp)
  (bf exp))


;;; Problem 8   handle-infix

(define (de-infix token)
  (cdr (assq token '((+ . sum)
		     (- . difference)
		     (* . product)
		     (/ . quotient)
		     (= . equalp)
		     (< . lessp)
		     (> . greaterp)))))

(define (handle-infix value line-obj env)
  (if (ask line-obj 'empty?)
      value
      (let ((token (ask line-obj 'next)))
	(if (memq token '(+ - * / = < >))
	    (handle-infix ((text (lookup-procedure (de-infix token)))
			   value
			   (eval-prefix line-obj env) )
			  line-obj
			  env)
	    (sequence
	     (ask line-obj 'put-back token)
	     value)))))


;;; Problem 9    eval-definition


(define (eval-definition line-obj)
  (define (parse-formal token)
    (if (eq? (first token) ':)
	(bf token)
	(error "Bad input name format in TO" token)))
  (define (get-formals)
    (if (ask line-obj 'empty?)
	'()
	(let ((token (ask line-obj 'next)))
	  (cons (parse-formal token) (get-formals)))))
  (define (get-body)
    (prompt "-> ")
    (let ((line (logo-read)))
      (if (equal? line '(end))
	  '()
	  (cons line (get-body)))))
  (let ((name (ask line-obj 'next)))
    (let ((formals (get-formals)))
      (set! the-procedures
	    (cons (list name
			'compound
			(length formals)
			(cons formals (get-body)))
		  the-procedures))))
  '=no-value=)


;;; Problem 10    eval-sequence

(define (eval-sequence exps env)
  (if (null? exps)
      '=no-value=
      (let ((value (eval-line (make-line-obj (car exps)) env)))
	(cond ((eq? value '=stop=) '=no-value=)
	      ((and (pair? value) (eq? (car value) '=output=))
	       (cdr value))
	      ((not (eq? value '=no-value=))
	       (error "You don't say what to do with" value))
	      (else (eval-sequence (cdr exps) env))))))



;;; SETTING UP THE ENVIRONMENT

(define the-primitive-procedures nil)

(define (add-prim name count proc)
  (set! the-primitive-procedures
	(cons (list name 'primitive count proc)
	      the-primitive-procedures)))

(add-prim 'first 1 first)
(add-prim 'butfirst 1 bf)
(add-prim 'bf 1 bf)
(add-prim 'last 1 last)
(add-prim 'butlast 1 bl)
(add-prim 'bl 1 bl)
(add-prim 'word -2 word)
(add-prim 'sentence -2 se)
(add-prim 'se -2 se)
(add-prim 'list -2 list)
(add-prim 'fput 2 cons)

(add-prim 'sum -2 (make-logo-arith +))
(add-prim 'difference 2 (make-logo-arith -))
(add-prim '=unary-minus= 1 (make-logo-arith -))
(add-prim '- 1 (make-logo-arith -))
(add-prim 'product -2 (make-logo-arith *))
(add-prim 'quotient 2 (make-logo-arith /))
(add-prim 'remainder 2 (make-logo-arith remainder))

(add-prim 'print 1 logo-print)
(add-prim 'pr 1 logo-print)
(add-prim 'show 1 logo-show)
(add-prim 'type 1 logo-type)
(add-prim 'make '(2) make)

(add-prim 'run '(1) run)
(add-prim 'if '(2) logo-if)
(add-prim 'ifelse '(3) ifelse)
(add-prim 'equalp 2 (logo-pred (make-logo-arith equalp)))
(add-prim 'lessp 2 (logo-pred (make-logo-arith <)))
(add-prim 'greaterp 2 (logo-pred (make-logo-arith >)))
(add-prim 'emptyp 1 (logo-pred empty?))
(add-prim 'numberp 1 (logo-pred (make-logo-arith number?)))
(add-prim 'listp 1 (logo-pred list?))
(add-prim 'wordp 1 (logo-pred (lambda (x) (not (list? x)))))

(add-prim 'stop 0 (lambda () '=stop=))
(add-prim 'output 1 (lambda (x) (cons '=output= x)))
(add-prim 'op 1 (lambda (x) (cons '=output= x)))

(add-prim 'load 1 meta-load)

(define the-global-environment nil)
(define the-procedures the-primitive-procedures)

;;; INITIALIZATION AND DRIVER LOOP

;;; The following code initializes the machine and starts the Logo
;;; system.  You should not call it very often, because it will clobber
;;; the global environment, and you will lose any definitions you have
;;; accumulated.

(define (initialize-logo)
  (set! the-global-environment (extend-environment '() '() '()))
  (set! the-procedures the-primitive-procedures)
  (driver-loop))

(define (driver-loop)
  (define (helper)
    (prompt "? ")
    (let ((line (logo-read)))
      (if (not (null? line))
  	  (let ((result (eval-line (make-line-obj line)
				   the-global-environment)))
	    (if (not (eq? result '=no-value=))
		(logo-print (list "You don't say what to do with" result))))))
    (helper))
  (logo-read)
  (helper))

;;; APPLYING PRIMITIVE PROCEDURES

;;; To apply a primitive procedure, we ask the underlying Scheme system
;;; to perform the application.  (Of course, an implementation on a
;;; low-level machine would perform the application in some other way.)

(define (apply-primitive-procedure p args)
  (apply (text p) args))


;;; Now for the code that's based on the book!!!


;;; Section 4.1.1

;; Given an expression like (proc :a :b :c)+5
;; logo-eval calls eval-prefix for the part in parentheses, and then
;; handle-infix to check for and process the infix arithmetic.
;; Eval-prefix is comparable to Scheme's eval.

(define (logo-eval line-obj env)
  (handle-infix (eval-prefix line-obj env) line-obj env))

(define (eval-prefix line-obj env)
  (define (eval-helper paren-flag)
    (let ((token (ask line-obj 'next)))
      (cond ((self-evaluating? token) token)
            ((quoted? token) (text-of-quotation token))
            ((variable? token)
	     (lookup-variable-value (variable-name token) env))
            ((definition? token) (eval-definition line-obj))
	    ((left-paren? token)
	     (let ((result (handle-infix (eval-helper #t)
				       	 line-obj
				       	 env)))
	       (let ((token (ask line-obj 'next)))
	       	 (if (right-paren? token)
		     result
		     (error "Too much inside parens")))))
	    ((right-paren? token)
	     (error "Unexpected ')'"))
            (else
	     (let ((proc (lookup-procedure token)))
	       (if (null? proc) (error "I don't know how to" token))
	       (if (list? (arg-count proc))
	       	   (logo-apply proc
			       (cons env
				     (collect-n-args (car (arg-count proc))
						     line-obj
						     env))
			       env)
	       	   (logo-apply proc
		       	       (collect-n-args (if paren-flag
						   (arg-count proc)
						   (abs (arg-count proc)))
					       line-obj
					       env)
		       	       env)))) )))
  (eval-helper #f))

(define (logo-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment
                         (parameters procedure)
                         arguments
                         env)))
        (else
         (error "Unknown procedure type -- LOGO-APPLY" procedure))))

(define (collect-n-args n line-obj env)
  (cond ((= n 0) '())
	((and (< n 0) (not (ask line-obj 'empty?)))
	 (let ((token (ask line-obj 'next)))
	   (ask line-obj 'put-back token)
	   (if (right-paren? token)
	       '()
      	       (let ((next (logo-eval line-obj env)))
        	 (cons next
	      	       (collect-n-args (-1+ n) line-obj env)) ))))
	(else      
      	 (let ((next (logo-eval line-obj env)))
           (cons next
	      	 (collect-n-args (-1+ n) line-obj env)) ))))

;;; Section 4.1.2 -- Representing expressions

;;; numbers

(define (self-evaluating? exp) (number? exp))

;;; quote

(define (quoted? exp)
  (or (list? exp)
      (eq? (first exp) '|"|)))

(define (text-of-quotation exp)
  (if (list? exp)
      exp
      (bf exp)))

;;; parens

(define (left-paren? exp) (eq? exp '|(|))

(define (right-paren? exp) (eq? exp '|)|))

;;; definitions

(define (definition? exp)
  (eq? exp 'to))

;;; procedures

(define (lookup-procedure name)
  (assq name the-procedures))

(define (primitive-procedure? p)
  (eq? (cadr p) 'primitive))

(define (compound-procedure? p)
  (eq? (cadr p) 'compound))

(define (arg-count proc)
  (caddr proc))

(define (text proc)
  (cadddr proc))

(define (parameters proc) (car (text proc)))

(define (procedure-body proc) (cdr (text proc)))

;;; Section 4.1.3

;;; Operations on environments

(define (lookup-variable-value var env)
  (let ((b (binding-in-env var env)))
    (if (found-binding? b)
        (binding-value b)
        (error "Unbound variable" var))))

(define (binding-in-env var env)
  (if (no-more-frames? env)
      no-binding
      (let ((b (binding-in-frame var (first-frame env))))
        (if (found-binding? b)
            b
            (binding-in-env var (rest-frames env))))))

(define (extend-environment variables values base-env)
  (adjoin-frame (make-frame variables values) base-env))

(define (define-variable! var val env)
  (let ((b (binding-in-env var env)))
    (if (found-binding? b)
        (set-binding-value! b val)
        (set-first-frame!
          the-global-environment
          (adjoin-binding (make-binding var val)
                          (first-frame the-global-environment))))))

;;; Representing environments

(define (first-frame env) (car env))

(define (rest-frames env) (cdr env))

(define (no-more-frames? env) (null? env))

(define (adjoin-frame frame env) (cons frame env))

(define (set-first-frame! env new-frame)
  (set-car! env new-frame))

;;; Representing frames

(define (make-frame variables values)
  (cond ((and (null? variables) (null? values)) '())
        ((null? variables)
         (error "Too many values supplied" values))
        ((null? values)
         (error "Too few values supplied" variables))
        (else
         (cons (make-binding (car variables) (car values))
               (make-frame (cdr variables) (cdr values))))))

(define (adjoin-binding binding frame)
  (cons binding frame))

(define (assq key bindings)
  (cond ((null? bindings) no-binding)
        ((eq? key (binding-variable (car bindings))) 
         (car bindings))
        (else (assq key (cdr bindings)))))

(define (binding-in-frame var frame)
  (assq var frame))

(define (found-binding? b)
  (not (eq? b no-binding)))

(define no-binding nil)

;;; Representing bindings

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (set-binding-value! binding value)
  (set-cdr! binding value))


