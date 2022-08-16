In the recitation following the introduction of higher-order procedures, I argued that there is a necessary sense of design elegance for building complex systems that remain comprehensible. One aspect of that elegance is related to regularity and the ability to compose operations. For example, we looked at two alternative implementations of how one might take a derivative of an arbitrary function of one variable:

The typical approach in ordinary programming languages is to define a procedure, say ddx, that takes as inputs the procedure that computes the function of one argument and the value, x, at which the derivative is to be evaluated.

;; This is a small number that happens to have an exact binary fraction representation
(define dx 0.00390625)

(define (ddx f x)
  (/ (- (f (+ x dx))
        (f x))
     dx))

We then invoke our procedure as follows:

(ddx square 3)
;Value: 6.00390625

However, this procedure is, technically speaking, ugly! Its signature is

((number ==> number), number) ==> number

and it appears to confound two separate issues: (1) how to find a numerical approximation to the derivative of a function, and (2) how to evaluate that derivate at a particular value of x.

Instead, we should prefer the following:

(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx))
          (f x))
       dx)))

We use it as follows:

((deriv square) 3)
;Value: 6.00390625

This deriv procedure’s signature is

(number ==> number) ==> (number ==> number)

which is much more regular (it transforms an input of a certain type into an output of the same type), and then allows us to define ddx in terms of deriv, if we choose, as

(define (ddx f x)
  ((deriv f) x))

Elegance has its rewards, because if we now become interested in taking the second derivative of a function, we can simply compose our existing ability to take the first derivative, twice. Thus, for example:

((deriv (deriv square)) 3)
;Value: 2.

Further, we might try

((deriv (deriv (deriv square))) 3)
;Value: 0.

which is great. In fact, if we define

(define (compose f g)
  (lambda (x)
    (f (g x))))

then note that we may define

(define 2nd-deriv (compose deriv deriv))

and indeed

((2nd-deriv square) 8)
;Value: 2.

Note that we first saw compose as a procedure that composes two procedures each of which has signature

number ==> number

leading to examples of its use such as

((compose square sqrt) 7)
;Value: 7.000000000000001

In the present case, we are using it on two procedures each of which has signature

(number ==> number) ==> (number ==> number)

The fact that we are able to do this without changing the definition of compose indicates that we have successfully captured some essence of the mathematical notion of composition.
