;;; section 1
(include "utils.scm")

;;; log: Sch-Num -> Sch-Num
;;; square: Sch-Num -> Sch-Num

(log 2)
;Value: .6931471805599453

((compose square log) 2)
;Value: .4804530139182014

((compose log square) 2)
;Value: 1.3862943611198906

;;; F = Sch-Num -> Sch-Num
;;; compose: (F, F) -> F

((thrice square) 3)
;Value: 6561

(square (square (square 3)))
;Value: 6561

;;; thrice: (T -> T) -> (T -> T)

((repeated sin 5) 3.1)
;Value: 4.1532801333692235e-2

(sin(sin(sin(sin(sin 3.1)))))
;Value: 4.1532801333692235e-2

;;; repeated: ((T -> T), Sch-Nonneg-Int) -> (T -> T)

;;; exercise 1.a & 1.b

;;; n = 27
(((thrice thrice) f) 0) ;;; =>
(((compose (compose thrice thrice) thrice) f) 0) ;;; =>
(((compose (lambda (x) (thrice (thrice x))) thrice) f) 0) ;;; =>
(((lambda (x) ((lambda (x) (thrice (thrice x))) (thrice x))) f) 0) ;;; =>
(((lambda (x) (thrice (thrice x))) (thrice f)) 0) ;;; =>
((thrice (thrice (thrice f))) 0) ;;; 3 * 3 * 3

;;; (((thrice thrice) 1+) 6) => 33
(((thrice thrice) 1+) 6)
;Value: 33
((repeated 1+ 27) 6)
;Value: 33

;;; (((thrice thrice) identity) compose) => compose
(((thrice thrice) identity) compose)
;Value: #[compound-procedure 14 compose]

;;; (((thrice thrice) square) 1) => 1
(((thrice thrice) square) 1)
;Value: 1

;;; (((thrice thrice) square) 2) => ((((2^2)^2)^2)^...), 27 power2 operations, very big number
(((thrice thrice) square) 2)
;;; take too long time, no result
;;; as an illustration, here is the value of ((((2^2)^2)^2)^...), 9 power2 operations, is a big number
((thrice (thrice square)) 2)
;Value: 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084096

;;; section 2
(include "curves.scm")

;;; exercise 2
(define (unit-line-at y)
  (lambda (t) (make-point t y)))
(define unit-line (unit-line-at 0))

;;; 1
;;; type of unit-line-at: Sch-Num -> Curve

;;; 2
(define (vertical-line point length)
  (lambda (t) (make-point (x-of point) (+ (y-of point) (* t length)))))

;;; 3
;;; type of vertical-line: (Point, Sch-Num) -> Curve

;;; exercise 3
;;; reflect-through-y-axis: (x, y) -> (-x, y)
(define (reflect-through-y-axis curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point (- (x-of ct))
                  (y-of ct)))))

;;; translate x0 distance along x axes, y0 distance along y axes
;;; (x, y) -> (x0+x, y0+y)

;;; scale-x-y stretches a curve along x coordinate by a scale factor,
;;;                             along y coordinate by b scale factor
;;; (x, y) -> (a*x, b*y)

;;; rotate-around-origin rotates a curve by theta
;;; using formulas of trigonometric functions:
;;; assuming (x, y) with radian of alpha,
;;; x' = cos(alpha+theta) = cos(alpha)*cos(theta)-sin(alpha)*sin(theta)
;;;    = x*cos(theta)-y*sin(theta),
;;; y' = sin(alpha+theta) = sin(alpha)*cos(theta)+cos(alpha)*sin(theta)
;;;    = y*cos(theta)+x*sin(theta)
;;; (x, y) -> (x*cos(theta)-y*sin(theta), y*cos(theta)+x*sin(theta))

;;; deriv-t draws derivative curve of original curve

;;; exercise 4
(define (connect-ends curve1 curve2)
  (let* ((end-point (curve1 1))
	 (start-point (curve2 0))
	 (curve2-coincided ((translate (- (x-of start-point) (x-of end-point))
				       (- (y-of start-point) (y-of end-point)))
			    curve2)))
    (connect-rigidly curve1 curve2-coincided)))

;;; section 3
(include "picture.scm")
(include "drawing.scm")
