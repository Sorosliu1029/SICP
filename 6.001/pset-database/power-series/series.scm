;;;; PS.SCM: Power-series arithmetic using infinite streams. 

;;; Power series may be represented by (infinite) streams of
;;;  coefficients.

;;; Thus we may represent the series
;;;         a0 + a1*x + a2*x^2 + a3*x^3 + ...
;;;  by the stream {a0 a1 a2 a3 ...}

;;; In our series the coefficients are manipulated with the 
;;;  procedures ADD, MUL, SUB, DIV, rather than +, *, -, /
;;;  so that we may use rational arithmetic, complex arithmetic,
;;;  or polynomial arithmetic... as desired.
;;;  These are defined in generic arithmetic.


;;; The following procedures provide a set of capabilities for
;;;  manipulating series.

;;; c*(a0 + a1*x + a2*x^2 + a3*x^3 + ...)
;;;  = c*a0 + c*a1*x + c*a2*x^2 + c*a3*x^3 + ...)

(define scale-series
  (lambda (c s)
    (map-stream (lambda (x) (mul c x))
		s)))


;;; (a0 + a1*x + a2*x^2 + ...) + (b0 + b1*x + b2*x^2 + ...)
;;;   = (a0+b0) + (a1+b1)*x + (a2+b2)*x^2 + ...

(define add-series
  (lambda (s1 s2)
    (map-2-streams add s1 s2)))


;;; (a0 + a1*x + a2*x^2 + ...) * (b0 + b1*x + b2*x^2 + ...)
;;;   = a0*b0 + (a0*b1+a1*b0)*x + (a0*b2+a1*b1+a2*b0)*x^2 + ...
;;; Each coefficient of the result is formed by reversing an initial
;;;  segment of one series, multiplying it by the coefficients of an
;;;  initial segment of the other series, and accumulating the
;;;  products. 

(define mul-series
  (lambda (s1 s2)
    (map-2-streams inner-product-segments
		   (map-stream reverse (initial-segments-series s1))
		   (initial-segments-series s2))))

(define initial-segments-series
  (let ()
    (define i-s-helper
      (lambda (seg series)
	(let ((nseg (cons (head series) seg)))
	  (cons-stream nseg
		       (i-s-helper nseg
				   (tail series))))))
    (lambda (s)
      (i-s-helper '() s))))

(define inner-product-segments
  (lambda (seg1 seg2)
    (reduce add (map2 mul seg1 seg2))))	;REDUCE, MAP2 are defined in PSUTIL

;;; The indefinite integral of a series 
;;;           a0 + a1*x + a2*x^2 + a3*x^3 + ...
;;;  is       a0*x + a1*x^2/2 + a2*x^3/3 + ...
;;;  We return the stream {a0 a1/2 a2/3 a3/4 ...} to 
;;;  represent this.  Note: this is not a legitimate 
;;;  representation of the correct series because all
;;;  series must begin with a constant term to make 
;;;  evaluation (partial sums) start up right.  This 
;;;  choice means that definite integrals (such as the
;;;  examples in COS-SERIES and SIN-SERIES, below)
;;;  can be started up with CONS-STREAM to add in the
;;;  constant of integration.

(define indefinite-integral
  (let ()
    (define integrate-helper
      (lambda (s n)
	(cons-stream (div (head s) n)
		     (integrate-helper (tail s)
				       (+ n 1)))))
    (lambda (series)
      (integrate-helper series 1))))


;;; A power series may be evaluated at a particular 
;;;  point.  Given a stream that represents a series, the
;;;  following procedure will produce a stream representing
;;;  the sequence of partial sums of a series.  Note that 
;;;  this is a sequence, not a series.

(define partial-sums
  (let ((partial-sums-helper
	 (lambda (x)
	   (define loop
	     (lambda (series sum xn)
	       (cons-stream sum
			    (loop (tail series)
				  (add sum (mul xn (head series)))
				  (mul xn x)))))
	   loop)))
    (lambda (series x)
      ((partial-sums-helper x) series 0 1))))

































