;;; Soros Liu
;;; Exercise 2.13
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.13

(percent (mul-interval x y))
;;; =>
(percent (make-interval (* (lower-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
;;; =>
(/
 (width (make-interval (* (lower-bound x) (lower-bound y))
		       (* (upper-bound x) (upper-bound y))))
 (center (make-interval (* (lower-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y)))))
;;; =>
(/
 (/ (- 
     (upper-bound (make-interval (* (lower-bound x) (lower-bound y))
				 (* (upper-bound x) (upper-bound y))))
     (lower-bound (make-interval (* (lower-bound x) (lower-bound y))
				 (* (upper-bound x) (upper-bound y)))))
    2)
 (/ (+ 
     (lower-bound (make-interval (* (lower-bound x) (lower-bound y))
				 (* (upper-bound x) (upper-bound y)))) 
     (upper-bound (make-interval (* (lower-bound x) (lower-bound y))
				 (* (upper-bound x) (upper-bound y)))))
    2))
;;; =>
(/
 (- 
  (upper-bound (make-interval (* (lower-bound x) (lower-bound y))
			      (* (upper-bound x) (upper-bound y))))
  (lower-bound (make-interval (* (lower-bound x) (lower-bound y))
			      (* (upper-bound x) (upper-bound y)))))
 (+ 
  (lower-bound (make-interval (* (lower-bound x) (lower-bound y))
			      (* (upper-bound x) (upper-bound y)))) 
  (upper-bound (make-interval (* (lower-bound x) (lower-bound y))
			      (* (upper-bound x) (upper-bound y))))))
;;; =>
(/
 (- 
  (* (upper-bound x) (upper-bound y))
  (* (lower-bound x) (lower-bound y)))
 (+ 
  (* (lower-bound x) (lower-bound y)) 
  (* (upper-bound x) (upper-bound y))))

;;; let (upper-bound x) -> Ux, (lower-bound x) -> Lx, the same as y
;;; that is:
;;; (percent (mul-interval x y)) => (UxUy-LxLy) / (UxUy+LxLy)
;;; meanwhile (percent x) => (Ux-Lx) / (Ux+Lx)
;;;           (percent y) => (Uy-Ly) / (Uy+Ly)
;;;           (+ (percent x) (percent y)) =>
;;;             (UxUy - LxLy) / ((UxUy+UxLy+LxUy+LxLy)/2)
;;; notice when with SMALL percent tolerance, Ux ~= Lx, Uy ~= Ly,
;;; so (UxUy+UxLy+LxUy+LxLy)/2 ~= (UxUy+LxLy)
;;; so we can get a simple formula that:
;;;   (percent (mul-interval x y)) ~= (+ (percent x) (percent y))
