;;; section 1: tutorial exercises

;;; exercise 1.1, skipped, see [ROOTDIR/exercises/chapter1]

;;; exercise 1.2, skipped, see [ROOTDIR/exercises/chapter1]

;;; exercise 1.3
(define (my-sine x)
  (if (small-enuf? x)
      x
      (* (my-sine (/ x 3))
	 (- 3
	    (* 4 
	       (my-sine (/ x 3))
	       (my-sine (/ x 3)))))))

;;; grow of space: O(log(n))
;;; grow of number of operations: O(n)

;;; exercise 1.4
(define (repeated p n)
  (cond ((= n 0) (lambda (x) x))
        ((= n 1) p)
        (else (lambda (x) (p ((repeated p (-1+ n)) x))))))

((repeated square 2) 5) ;;; => (5^2)^2 = 625
;Value: 625

((repeated square 5) 2) ;;; => (((((2^2)^2)^2)^2)^2) = 4,294,967,296
;Value: 4294967296

;;; section 2: continued fractions

;;; exercise 2.1
(define (cont-frac-r n d k)
  (define (recur i)
    (if (> i k)
	0.0
	(/ (n i)
	   (+ (d i) (recur (1+ i))))))
  (recur 1))

(define (cont-frac-i n d k)
  (define (iter i r)
    (if (= i 0)
	r
	(iter (-1+ i) (/ (n i)
			 (+ (d i) r)))))
  (iter k 0.0))

;;; 1/phi ~= 0.61803398875

(cont-frac-i (lambda (i) 1) (lambda (i) 1) 1)
;Value: 1.
(cont-frac-i (lambda (i) 1) (lambda (i) 1) 2)
;Value: .5
(cont-frac-i (lambda (i) 1) (lambda (i) 1) 3)
;Value: .6666666666666666
(cont-frac-i (lambda (i) 1) (lambda (i) 1) 4)
;Value: .6000000000000001
(cont-frac-i (lambda (i) 1) (lambda (i) 1) 5)
;Value: .625
(cont-frac-i (lambda (i) 1) (lambda (i) 1) 6)
;Value: .6153846153846154
(cont-frac-i (lambda (i) 1) (lambda (i) 1) 7)
;Value: .6190476190476191
(cont-frac-i (lambda (i) 1) (lambda (i) 1) 8)
;Value: .6176470588235294
(cont-frac-i (lambda (i) 1) (lambda (i) 1) 9)
;Value: .6181818181818182
(cont-frac-i (lambda (i) 1) (lambda (i) 1) 10)
;Value: .6179775280898876
(cont-frac-i (lambda (i) 1) (lambda (i) 1) 11)
;Value: .6180555555555556

;;; k = 11

;;; exercise 2.2
(define (estimate-pi k)
  (/ 4 (+ (brouncker k) 1)))

(define (brouncker k)
  (cont-frac-i (lambda (i) (square (- (* 2 i) 1)))
               (lambda (i) 2)
               k))

(estimate-pi 116)
;Value: 3.1501395060584128
(estimate-pi 117)
;Value: 3.1331182294626685
(estimate-pi 118)
;Value: 3.1499958665934695

;;; k = 118

;;; exercise 2.3
(define (atan-cf k x)
  (cont-frac-i (lambda (i) (if (= i 1) x (square (* x (-1+ i)))))
               (lambda (i) (-1+ (* 2 i)))
               k))

;;; exercise 2.4
(atan-cf 100 1.57)
;Value: 1.0036550779803273
(atan 1.57)
;Value: 1.0036550779803273

(atan-cf 100 0)
;Value: 0
(atan 0)
;Value: 0

(atan-cf 100 1)
;Value: .7853981633974483
(atan 1)
;Value: .7853981633974483

(atan-cf 100 3)
;Value: 1.2490457723982544
(atan 3)
;Value: 1.2490457723982544

(atan-cf 100 10)
;Value: 1.4711276682474386
(atan 10)
;Value: 1.4711276743037347

(atan-cf 100 30)
;Value: 1.5336085945350313
(atan 30)
;Value: 1.5374753309166493

(atan-cf 100 100)
;Value: 1.1895927431625923
(atan 100)
;Value: 1.5607966601082315
(atan-cf 1000 100)
;Value: 1.5607966536952345

;;; exercise 2.5
(define (nested-acc op r term k)
  (define (iter i acc)
    (if (= i 0)
	acc
	(iter (-1+ i) ((op i) (term i) acc))))
  (iter k r))

(define (f x k)
  (nested-acc (lambda (i) (lambda (n d) (sqrt (+ n d))))
              0
              (lambda (i) x)
              k))

;;; exercise 2.6
;;; f(1) = golden-ratio ~= 1.61803398875

(f 1 1)
;Value: 1
(f 1 9)
;Value: 1.6179775309347393
(f 1 10)
;Value: 1.6180165422314876
(f 1 11)
;Value: 1.6180285974702324

;;; k = 10

;;; exercise 2.7
;;; sin(x) = x - x^3/3! + x^5/5! - x^7/7! + ...

(define (cont-sin x k)
  (nested-acc (lambda (i) +)
              0
              (lambda (i) 
                (let ((k (-1+ (* 2 i))))
                  (* (if (odd? i) 1 -1)
                     (/ (expt x k) (factorial k)))))
              k))

(define (factorial n)
  (if (= n 0) 1 (* n (factorial (-1+ n)))))

(cont-sin 0 10)
;Value: 0
(sin 0)
;Value: 0

(cont-sin (/ 3.14 4) 10)
;Value: .706825181105366
(sin (/ 3.14 4))
;Value: .706825181105366

(cont-sin (/ 3.14 2) 10)
;Value: .9999996829318344
(sin (/ 3.14 2))
;Value: .9999996829318346

(cont-sin 3.14 10)
;Value: 1.5926523931604208e-3
(sin 3.14)
;Value: 1.5926529164868282e-3

;;; exercise 2.8
(define (build n d b)
  (/ n (+ d b)))

(define (repeated-build k n d b)
  ((repeated (lambda (b) (build n d b))
	     k)
   b))

;;; exercise 2.9
;;; 1/phi ~= 0.61803398875
(repeated-build 10 1 1 0.0)
;Value: .6179775280898876
(repeated-build 20 1 1 0.0)
;Value: .6180339850173578

;;; exercise 2.10
(define (fibonacci k)
  (if (< k 2) k (+ (fibonacci (- k 1)) (fibonacci (- k 2)))))

(define (r k)
  (if (= k 1)
      (lambda (x) (/ 1 (1+ x)))
      (lambda (x) (/ 1 (1+ ((r (-1+ k)) x))))))

(define (r-fib k)
  (let ((fib-k (fibonacci k))
        (fib-k-1 (fibonacci (-1+ k)))
        (fib-k+1 (fibonacci (1+ k))))
    (lambda (x) (/ (+ fib-k (* fib-k-1 x))
		   (+ fib-k+1 (* x fib-k))))))

((r 2) 0)
;Value: 1/2
((r-fib 2) 0)
;Value: 1/2

((r 10) 2)
;Value: 123/199
((r-fib 10) 2)
;Value: 123/199

;;; section: optional problem
(define (cont-frac-new n d)
  (define (f k) (/ (a k) (b k)))
  (define (a j)
    (cond ((= j -1) 1)
          ((= j 0) 0)
          (else (+ (* (d j) (a (- j 1))) (* (n j) (a (- j 2)))))))
  (define (b j)
    (cond ((= j -1) 0)
          ((= j 0) 1)
          (else (+ (* (d j) (b (- j 1))) (* (n j) (b (- j 2)))))))
  (define tolerance 0.0001)
  (define (close-enuf? curr next)
    (< (abs (- curr next)) tolerance))
  (define (iter k curr)
    (let ((next (f (1+ k))))
      (cond ((close-enuf? curr next)
             (display "; Q: how many terms are evaluated ? A: ")
             (display k)
             curr)
            (else (iter (1+ k) next)))))
  (iter 0 0))

(cont-frac-new (lambda (i) 1.0)
               (lambda (i) 1.0))
; Q: how many terms are evaluated ? A: 10
;Value: .6179775280898876

(define (tan-cf x)
  (cont-frac-new (lambda (i)
                   (if (= i 1) x (- (square x))))
                 (lambda (i) (-1+ (* 2 i)))))

(tan-cf 1.0)
; Q: how many terms are evaluated ? A: 4
;Value: 1.5573770491803278
(tan 1.0)
;Value: 1.5574077246549023

(tan-cf (/ 3.14 4))
; Q: how many terms are evaluated ? A: 4
;Value: .9992018697579907
(tan (/ 3.14 4))
;Value: .9992039901050427

(tan-cf (/ 3.14 2))
; Q: how many terms are evaluated ? A: 8
;Value: 1255.765549835621
(tan (/ 3.14 2))
;Value: 1255.7655915007897

(tan-cf 3.14)
; Q: how many terms are evaluated ? A: 7
;Value: -1.6447971997619805e-3
(tan 3.14)
;Value: -1.592654936407223e-3
