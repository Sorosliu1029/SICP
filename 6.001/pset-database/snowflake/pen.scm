;;;This is the graphics support for PS1.  You should use this
;;;code as a "black box" and not worry about the details.

(define (make-plotter ox oy)
  (let ((xcor ox)
        (ycor oy))
    (lambda (message)
      (cond ((eq? message 'clearscreen)
             (lambda ()
               (set! xcor ox)
               (set! ycor oy)
               (clear-graphics)
               (position-pen ox oy)
	       'cleared))
            ((eq? message 'move-pen-xy)
             (lambda (x y)
               (let ((nx (+ xcor x))
                     (ny (+ ycor y)))
                 (draw-line-to (round nx) (round ny))
                 (set! xcor nx)
                 (set! ycor ny)
		 'pen-moved)))
            ((eq? message 'set-pen-at)
             (lambda (x y)
               (set! xcor x)
               (set! ycor y)
               (position-pen x y)
	       'pen-set))
            (else (error "undefined operation -- MAKE-PLOTTER"))))))

(define *plotter* (make-plotter 0 0))

(define clearscreen (*plotter* 'clearscreen))

(define move-pen-xy (*plotter* 'move-pen-xy))

(define set-pen-at (*plotter* 'set-pen-at))

(define (plot-line angle length)
  (move-pen-xy (* length (cos angle))
               (* length (sin angle))))

