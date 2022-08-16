From bh%anarres.Berkeley.EDU@Berkeley.EDU Mon Dec 11 15:31:34 1989
Return-Path: <bh%anarres.Berkeley.EDU@Berkeley.EDU>
Date: Mon, 11 Dec 89 11:47:24 PST
From: bh%anarres.Berkeley.EDU@Berkeley.EDU (Brian Harvey)
To: hal
Subject: logo/logo.scm

;;; logo.scm         part of programming project #3


;;; Problem 1   make-line-obj

(define (make-line-obj text)
  (error "make-line-obj not written yet!"))


;;; Problem 2   logo-type

(define (logo-type val)
  (error "logo-type not written yet!"))

(define (logo-print val)
  (logo-type val)
  (newline)
  '=no-value=)

(define (logo-show val)
  (logo-print (list val)))


;;; (Problems 3 and 4 require changes to logo-meta.scm)


;;; Problem 5   variables   (logo-meta.scm is also affected)

(define (make env var val)
  (error "make not written yet!")
  '=no-value=)


;;; Here are the primitives RUN, IF, and IFELSE.  Problem 6 provides
;;; support for these, but you don't have to modify them.

(define (run env exp)
  (eval-line (make-line-obj exp) env))

(define (logo-if env t/f exp)
  (cond ((eq? t/f 'true) (eval-line (make-line-obj exp) env))
	((eq? t/f 'false) '=no-value=)
	(else (error "Input to IF not true or false" t/f))))

(define (ifelse env t/f exp1 exp2)
  (cond ((eq? t/f 'true) (eval-line (make-line-obj exp1) env))
	((eq? t/f 'false) (eval-line (make-line-obj exp2) env))
	(else (error "Input to IFELSE not true or false" t/f))))


;;; Problem 6   logo-pred

(define (logo-pred pred)
  pred)      ;; This isn't written yet but we fake it for now.


;;; Here is an example of a Scheme predicate that will be turned into
;;; a Logo predicate by logo-pred:

(define (equalp a b)
  (if (and (number? a) (number? b))
      (= a b)
      (equal? a b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Stuff below here is needed for the interpreter to work but you  ;;;
;;;  don't have to modify anything or understand how they work.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The Logo reader

(define (logo-read)
  (define lookahead '())
  (define (logo-read-help depth)
    (define (get-char)
      (if lookahead
	  (let ((char lookahead))
	    (set! lookahead '())
	    char)
	  (let ((char (read-char)))
	    (if (eq? char #\\)
	      	(list (read-char))
	      	char))))
    (define (quoted char)
      (if (pair? char)
	  char
	  (list char)))
    (define (get-symbol char)
      (define (iter sofar char)
      	(cond ((pair? char) (iter (cons (car char) sofar) (get-char)))
	      ((memq char
		     '(#\space #\newline #\+ #\- #\* #\/
			       #\= #\< #\> #\( #\) #\[ #\] ))
	       (set! lookahead char)
	       sofar)
	      (else (iter (cons char sofar) (get-char))) ))
      (original-implode (reverse (iter '() char))) )
    (define (get-token space-flag)
      (let ((char (get-char)))
      	(cond ((eq? char #\space) (get-token #t))
	      ((memq char '(#\+ #\* #\/ #\= #\< #\> #\( #\) ))
	       (string->symbol (make-string 1 char)))
	      ((eq? char #\-)
	       (if space-flag
		   (let ((char (get-char)))
		     (let ((result (if (eq? char #\space)
				       '-
				       '=unary-minus=)))
		       (set! lookahead char)
		       result))
		   '-))
	      ((eq? char #\[) (logo-read-help (1+ depth)))
	      ((pair? char) (get-symbol char))
	      ((eq? char #\")
	       (let ((char (get-char)))
	       	 (if (memq char '(#\[ #\] #\newline))
		     (sequence (set! lookahead char) '|"|)
		     (word '|"| (get-symbol (quoted char))))))
	      (else (get-symbol char)) )))
    (let ((char (get-char)))
      (cond ((eq? char #\newline)
	     (if (> depth 0) (set! lookahead char))
	     '())
	    ((eq? char #\])
	     (if (> depth 0) '() (error "Unexpected ]")))
	    ((eof-object? char) char)
	    (else (set! lookahead char)
		  (let ((token (get-token #f)))
		    (cons token (logo-read-help depth)) )))))
  (logo-read-help 0))


;;; Assorted stuff

(define (make-logo-arith op)
  (lambda args (apply op (mapcar maybe-num args))))

(define (maybe-num val)
  (original-implode (explode val)))

(define tty-port (current-input-port))

(define (prompt string)
  (if (eq? (current-input-port) tty-port) (princ string)))

(define (meta-load fn)
  (define (loader)
    (let ((exp (logo-read)))
      (if (eof-object? exp)
	  '()
	  (sequence (eval-line (make-line-obj exp)
			       the-global-environment)
		    (loader)))))
  (with-input-from-file (symbol->string fn) loader)
  '=no-value=)

(define *keyboard-interrupt-handler* (lambda() (reset)))


;;; This has to do with improving BUTFIRST etc. as described
;;; in the handout.

(define original-implode
   ; makes a symbol from a list of objects
   (lambda (x)
     (define (string->whichever x)
       (define (sw str minus pointok pointseen eok eseen endok)
	 (cond ((null? str) (if endok
				(string->number (apply string-append x))
				(string->symbol (apply string-append x)) ))
	       ((and minus (equal? (car str) "-"))
		(sw (cdr str) #f #f pointseen #f eseen #f))
	       ((and pointok (not pointseen) (equal? (car str) "."))
		(sw (cdr str) #f #f #t #t eseen #f))
	       ((and eok (not eseen) (equal? (car str) "e"))
	       	(sw (cdr str) #t #f #t #f #t #f))
	       ((member (car str) '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
	       	(sw (cdr str) #f #t pointseen #t eseen #t))
	       (else (string->symbol (apply string-append x))) ))
       (if (null? x)
	   '||
	   (sw x #t #t #f #f #f #f)))
     (string->whichever
      (mapcar (lambda (x) (format "~a" x)) x) ) ))

(define (implode x)
  (string->symbol (apply string-append
			 (mapcar (lambda (x) (format "~a" x)) x))))


