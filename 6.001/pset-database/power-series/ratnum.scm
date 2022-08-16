;;;; RATNUM.SCM -- Rational number arithmetic package

;;; The following procedures try to remove common factors as early as
;;;  possible to reduce the cost of manipulation of big integers.

(define (+_rational x y)
  (let ((dx (denominator x)) (dy (denominator y)))
    (let ((d1 (gcd dx dy)))
      (let ((dx/d1 (quotient dx d1)) (dy/d1 (quotient dy d1)))
	(if (= 1 d1)
	    (make-rational (+ (* (numerator x) dy)
			      (* dx (numerator y)))
			   (* dx dy))
	    (let ((tem (+ (* dy/d1 (numerator x))
			  (* dx/d1 (numerator y)))))
	      (if (= tem 0)
		  0
		  (let ((d2 (gcd tem d1)))
		    (make-rational (quotient tem d2)
				   (* dx/d1
				      (quotient dy d2)))))))))))

(define (-_rational x y)
  (+_rational x
	      (make-rational (- (numerator y))
			     (denominator y))))

(define (*_rational x y)
  (let ((nx (numerator x)) (ny (numerator y))
        (dx (denominator x)) (dy (denominator y)))
    (let ((gnxdy (gcd nx dy)) (gnydx (gcd ny dx)))
      (let ((nx/gcd (quotient nx gnxdy)) (dy/gcd (quotient dy gnxdy))
	    (ny/gcd (quotient ny gnydx)) (dx/gcd (quotient dx gnydx)))
	(make-rational (* nx/gcd ny/gcd)
		       (* dx/gcd dy/gcd))))))

(define (/_rational x y)
  (*_rational x
	      (make-rational (denominator y)
			     (numerator y))))

(define (=_rational x y)
  (and (= (numerator x) (numerator y))
       (= (denominator x) (denominator y))))

(define (negate-rational x)
  (make-rational (- (numerator x))
		 (denominator x)))

(define (recip-rational x)
  (make-rational (denominator x) (numerator x)))

;;; Representation

(define (make-rational n d)		;gcd already done
  (cond ((= d 0) (error "zero denominator -- rational"))
	((= n 0) 0)
	((= d 1) n)
	((< d 0) (make-rational (- n) (- d)))
	(else (construct-rational n d))))

(define (rational? q)
  (cond ((integer? q) #!true)
	((pair? q) (eq? (car q) '*rat*))
	(else #!false)))

(define (construct-rational n d)
  (list '*rat* n d))

;;; number? is used instead of integer? in the following so that
;;; reals can be coerced to reals.

(define (numerator q)
  (cond ((number? q) q)
	((eq? '*rat* (car q)) (cadr q))
	(else (error "bad rational"))))

(define (denominator q)
  (cond ((number? q) 1)
	((eq? '*rat* (car q)) (caddr q))
	(else (error "bad rational"))))

(define (rat->real q)
  (/ (numerator q)
     (denominator q)))

;;;; Generic arithmetic with scheme numbers and rationals.

(define (with-coercions real-op rational-op)
  (lambda (x y)
    (if (and (rational? x) (rational? y))
	(rational-op x y)
	(real-op (rat->real x) (rat->real y)))))



(define add (with-coercions + +_rational))
(define sub (with-coercions - -_rational))
(define mul (with-coercions * *_rational))
(define div (with-coercions / /_rational))

