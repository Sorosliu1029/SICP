(load-option 'x11)

(define (make-window/X11 width height x y)
  (let ((window
	 (make-graphics-device 'X
			       false
			       (x-geometry-string x y width height)
			       true)))
    ;; Prevent this window from receiving the keyboard focus.
    (x-graphics/disable-keyboard-focus window)
    ;; Inform the window manager that this window does not do any
    ;; keyboard input.
    (x-graphics/set-input-hint window false)
    ;; OK, now map the window onto the screen.
    (x-graphics/map-window window)
    (x-graphics/flush window)
    window))