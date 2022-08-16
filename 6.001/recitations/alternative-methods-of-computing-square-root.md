In lectures and the text, there are (at least) three fundamentally different presentations of how one might build a computer algorithm for computing the square root of a non-negative real number: (1) due to Heron of Alexandria, successively approximating y as the square root of x by averaging y and x/y, (2) a method that finds the fixed point of the transformation y ==> x/y, and (3) Newton’s method, that finds a zero of the function y2 - x. Here are some implementations of each of these algorithms, and examples of their use. Note that each implementation traces successive values of the approximation, to let us see what is being computed.

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.00001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Heron of Alexandria’s sqrt Method
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sqrt-heron x)

  (define (sqrt-iter guess x)
    (display guess) (newline)
    (if (good-enuf? guess x)
 (improve guess x)  ;original returned guess; less accurate
 (sqrt-iter (improve guess x)
     x)))
  (define (improve guess x)
    (average guess (/ x guess)))

  (define (good-enuf? guess x)
    (< (abs (- (square guess) x))
       tolerance))

  (sqrt-iter 1.0 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fixed point method for sqrt
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (fixed-point f iguess)

  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (define (try guess)
    (display guess) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
   next
   (try next))))

  (try iguess))

(define (fsqrt x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y)))
   1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Newton’s Method
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dx tolerance)

(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx))
   (f x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
     ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (nsqrt x)
  (newtons-method
   (lambda (y) (- (square y) x))
   1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generalization (just for fun)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (fsqrt1 x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp
   1.0))

(define (nsqrt1 x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))

Using these definitions, we can compute (sqrt 2) in the three different ways:

(sqrt-heron 2)
1.
1.5
1.4166666666666665
1.4142156862745097
;Value: 1.4142135623746899

(fsqrt 2)
1.
1.5
1.4166666666666665
1.4142156862745097
;Value: 1.4142135623746899

(nsqrt 2)
1.
1.4999975000090175
1.416666805550865
1.4142156951657834
;Value: 1.4142135623822438

Note that the first two methods are really doing precisely the same calculation. The third differs only in that in Newton’s method we are really just approximating the derivative, rather than computing it directly. Even so, the third method also goes through the same number of steps of calculation and intermediate values to arrive at an answer that differs from the first two only in the 12th significant digit! Thus, in essence, each of these three methods is actually doing exactly the same thing.

Note also that each method converges quickly even for much larger arguments:

(fsqrt 2000)
1.
1000.5
501.24950024987504
252.6197645828103
130.26840074561815
72.81065908632895
50.13958236477863
45.014113668459046
44.72231152890744
44.721359560127915
;Value: 44.721359549995796

(nsqrt 2000)
1.
1000.49500282873
501.2470075904563
252.61852978302747
130.26780512293146
72.81039859738789
50.13950281939877
45.01410604971975
44.722311512074825
44.721359560234205
;Value: 44.721359549995796