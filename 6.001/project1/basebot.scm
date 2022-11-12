;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s^2
(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
    (+ (* 0.5 a (square t)) (* v t) u)))

;; you need to complete this procedure, then show some test cases

(position 0 0 0 0) ; -> 0
;Value: 0

(position 0 0 20 0) ; -> 20
;Value: 20

(position 0 5 10 10) ; -> 60
;Value: 60

(position 2 2 2 2)
;Value: 10.

(position 5 5 5 5)
;Value: 92.5

(position 2 2 2 0)
;Value: 2

(position 0 2 2 0)
;Value: 2

(position gravity 0 0 10)
;Value: 490.00000000000006

;; Problem 2

(define root1
  (lambda (a b c)
    (let ((r (- (square b) (* 4 a c)))) 
      (if (or (= a 0) (< r 0))
	  (error "invalid parameters" a b c)
	  (/ (- (- b) (sqrt r)) (* 2 a))))))

(define root2
  (lambda (a b c)
    (let ((r (- (square b) (* 4 a c)))) 
      (if (or (= a 0) (< r 0))
	  (error "invalid parameters" a b c)
	  (/ (+ (- b) (sqrt r)) (* 2 a))))))

;; complete these procedures and show some test cases

(root1 5 3 6)
;invalid parameters 5 3 6

(root1 1 -2 1)
;Value: 1

(root1 1 0 -4)
;Value: -2

(root2 5 3 6)
;invalid parameters 5 3 6

(root2 1 -2 1)
;Value: 1

(root2 1 0 -4)
;Value: 2

;; Problem 3

;;; only root2 (bigger root) makes sense
;;; since initial elevation is greater than 0, one of the roots must be negative,
;;; but time cannot be negative, so only bigger root makes sense
(define time-to-impact
  (lambda (vertical-velocity elevation)
    (root1 (- (* 0.5 gravity)) vertical-velocity elevation)))

(time-to-impact 0 4.9)
;Value: 1.

(time-to-impact 0 0)
;Value: 0

(time-to-impact 10 0)
;Value: 2.0408163265306123

(time-to-impact 10 10)
;Value: 2.7759847483760463

;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity (- elevation target-elevation))))

(time-to-height 0 10 10)
;Value: 0

(time-to-height 0 10 5.1)
;Value: 1.

(time-to-height 10 10 10)
;Value: 2.0408163265306123

(time-to-height 10 10 0)
;Value: 2.7759847483760463

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (let ((rad (degree2radian angle)))
      (* (* velocity (cos rad))
	 (time-to-impact (* velocity (sin rad))
			 elevation)))))

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees

;; what is the distance traveled in each case?
;; record both in meters and in feet

(travel-distance-simple 1 45 0)
;Value: 20.32892781536815

(meters-to-feet (travel-distance-simple 1 45 0))
;Value: 67.0854617907149

(travel-distance-simple 1 45 90)
;Value: 5.496418989612468e-4

(meters-to-feet (travel-distance-simple 1 45 90))
;Value: 1.8138182665721145e-3

(travel-distance-simple 1 45 45)
;Value: 207.6278611514906

(meters-to-feet (travel-distance-simple 1 45 45))
;Value: 685.171941799919


;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 0.01)

(define find-best-angle
  (lambda (velocity elevation)
    (define (find-iter at best-angle best-distance)
      (let ((dis (travel-distance-simple elevation velocity at)))
        (cond ((> at 90) best-angle)
              ((> dis best-distance) (find-iter (+ at alpha-increment) at dis))
              (else (find-iter (+ at alpha-increment) best-angle best-distance)))))
    (find-iter 0 0 0)))

;; find best angle

(find-best-angle 45 1)
;Value: 44.859999999999644

;; try for other velocities

(find-best-angle 20 1)
;Value: 44.309999999999754

(find-best-angle 100 1)
;Value: 44.96999999999962

(find-best-angle 0.1 1)
;Value: 1.290000000000001

;; try for other heights

(find-best-angle 45 2)
;Value: 44.72999999999967

(find-best-angle 45 0)
;Value: 44.999999999999616

(find-best-angle 45 10)
;Value: 43.67999999999988

(find-best-angle 45 100)
;Value: 35.48000000000151

;;; best angle is probably 45 degree

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin 
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two 
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level 
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4 
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor 
;; that can be computed

;; We would like to again compute distance , but taking into account 
;; drag.
;; Basically we can rework the equations to get four coupled linear 
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the 
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one 
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.01) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(define (speed u v) (sqrt (+ (square u) (square v))))

(define integrate
  (lambda (x0 y0 u0 v0 dt g m beta ret-func)
    (define (dx u) (* u dt))
    (define (dy v) (* v dt))
    (define (du u v) (* (/ -1 m) (speed u v) beta u dt))
    (define (dv u v) (* (- (+ (* (/ 1 m) (speed u v) v beta) g)) dt))
    (define (iter x y u v t)
      (if (< y 0)
	  (ret-func x y u v t)
	  (iter (+ x (dx u))
		(+ y (dy v))
		(+ u (du u v))
		(+ v (dv u v))
		(+ t dt))))
    (iter x0 y0 u0 v0 0)))

(define travel-distance
  (lambda (elevation velocity angle)
    (let ((alpha (degree2radian angle)))
      (integrate 0
		 elevation
		 (* velocity (cos alpha))
		 (* velocity (sin alpha))
		 0.01
		 gravity
		 mass
		 beta
		 (lambda (x y u v t) x)))))

;; RUN SOME TEST CASES

(meters-to-feet (travel-distance 1 45 45))
;Value: 304.3610105268868

(meters-to-feet (travel-distance 1 40 45))
;Value: 269.5039326610774

(meters-to-feet (travel-distance 1 35 45))
;Value: 231.99119882455986

(meters-to-feet (travel-distance 1 45 48))
;Value: 298.10178311613436
(meters-to-feet (travel-distance 1 45 47))
;Value: 300.50616033897336

(meters-to-feet (travel-distance 1 45 30))
;Value: 299.4568019926534
(meters-to-feet (travel-distance 1 45 31))
;Value: 302.0005584012364

;;; within range of [31, 47] degress, the ball will land over the fence

;; what about Denver?
(define denver-density 1.06)  ; kg/m^3
(define denver-beta (* .5 drag-coeff denver-density (* 3.14159 .25 (square diameter))))
(define denver-travel-distance
  (lambda (elevation velocity angle)
    (let ((alpha (degree2radian angle)))
      (integrate 0
		 elevation
		 (* velocity (cos alpha))
		 (* velocity (sin alpha))
		 0.01
		 gravity
		 mass
		 denver-beta
		 (lambda (x y u v t) x)))))

(meters-to-feet (denver-travel-distance 1 45 45))
;Value: 329.42480960223105

(meters-to-feet (denver-travel-distance 1 40 45))
;Value: 289.4718574393191

(meters-to-feet (denver-travel-distance 1 35 45))
;Value: 247.33457092270757

(meters-to-feet (denver-travel-distance 1 45 55))
;Value: 298.61759267940505
(meters-to-feet (denver-travel-distance 1 45 54))
;Value: 303.09309253868827

(meters-to-feet (denver-travel-distance 1 45 24))
;Value: 297.96243502418537
(meters-to-feet (denver-travel-distance 1 45 25))
;Value: 302.9649906480375

;;; in Denver, it's within range [25, 54] degrees, wider than in Boston

;; Problem 7

;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance

(define (fastest-throw velocity distance elevation)
  (define (integrate-time u0 v0 dt g m beta)
    (define tolerance 0.25)
    (integrate 0 elevation u0 v0 dt g m beta
               (lambda (x y u v t)
                 (if (< (abs (- x distance)) tolerance) t 0))))
  (define (iter at fastest)
    (define alpha-increment 0.1)
    (define dt 0.001)
    (define (debug t)
      (cond ((> t 0)
	     (newline)
	     (display "; angle: ")
	     (display at)
	     (display ", time: ")
	     (display t))))
    (let ((alpha (degree2radian at)))
      (if (> at 90)
	  fastest
	  (let ((t (integrate-time (* velocity (cos alpha))
				   (* velocity (sin alpha))
				   dt
				   gravity
				   mass
				   beta)))
	    (debug t)
	    (if (and (> t 0) (or (< t fastest) (= 0 fastest)))
		(iter (+ at alpha-increment) t)
		(iter (+ at alpha-increment) fastest))))))
  (iter -90 0))

;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

(fastest-throw 45 36 1)
; angle: 4.699999999999164, time: .9480000000000007
; angle: 4.799999999999164, time: .9590000000000007
; angle: 78.099999999999, time: 6.775000000000597
;Value: .9480000000000007

(fastest-throw 55 36 1)
; angle: 2.599999999999165, time: .7730000000000006
; angle: 80.09999999999889, time: 7.667000000000895
; angle: 80.19999999999888, time: 7.669000000000896
;Value: .7730000000000006

(fastest-throw 35 36 1)
; angle: 8.99999999999915, time: 1.233999999999975
; angle: 9.099999999999149, time: 1.2439999999999738
; angle: 73.99999999999923, time: 5.652000000000222
; angle: 74.09999999999923, time: 5.654000000000223
;Value: 1.233999999999975

;;; assume pitcher stands at the center of the line between second base and home,
;;; so the distance from pitcher to home plate is roughly 18m
;;; 90mph ~= 40.5 m/sec
(define pitcher-fastest (fastest-throw 40.5 18 1))
; angle: .1999999999991641, time: .47900000000000037
; angle: .2999999999991641, time: .4870000000000004
; angle: 83.59999999999869, time: 6.435000000000484
pitcher-fastest
;Value: .47900000000000037

;;; cather throw to second base
(define catcher-fastest (fastest-throw 40.5 36 1))
; angle: 6.199999999999159, time: 1.0549999999999946
; angle: 6.299999999999159, time: 1.0659999999999934
; angle: 76.59999999999908, time: 6.30400000000044
; angle: 76.69999999999908, time: 6.306000000000441
catcher-fastest
;Value: 1.0549999999999946

;;; catch and release time
(- 3 pitcher-fastest catcher-fastest)
;Value: 1.466000000000005

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s

(fastest-throw 45 30 1)
; angle: 3.0999999999991656, time: .7650000000000006
; angle: 3.1999999999991657, time: .7760000000000006
; angle: 80.19999999999888, time: 6.821000000000613
; angle: 80.29999999999887, time: 6.823000000000613
;Value: .7650000000000006
(fastest-throw 55 30 1)p
; angle: 1.4999999999991644, time: .6330000000000005
; angle: 81.89999999999878, time: 7.703000000000907
;Value: .6330000000000005
(fastest-throw 35 30 1)
; angle: 6.499999999999158, time: .9940000000000008
; angle: 6.599999999999158, time: 1.0030000000000003
; angle: 76.89999999999907, time: 5.723000000000246
; angle: 76.99999999999906, time: 5.725000000000247
; angle: 77.09999999999906, time: 5.727000000000247
;Value: .9940000000000008

(fastest-throw 45 60 1)
; angle: 11.899999999999139, time: 1.8199999999999104
; angle: 11.999999999999138, time: 1.831999999999909
; angle: 68.39999999999955, time: 6.457000000000491
; angle: 68.49999999999955, time: 6.461000000000492
;Value: 1.8199999999999104
(fastest-throw 55 60 1)
; angle: 7.4999999999991545, time: 1.4709999999999488
; angle: 72.59999999999931, time: 7.439000000000819
; angle: 72.6999999999993, time: 7.44300000000082
;Value: 1.4709999999999488
(fastest-throw 35 60 1)
; angle: 22.19999999999921, time: 2.4909999999998367
; angle: 22.299999999999212, time: 2.4999999999998357
; angle: 22.399999999999213, time: 2.5089999999998347
; angle: 22.499999999999215, time: 2.5179999999998337
; angle: 58.599999999999724, time: 5.0600000000000245
; angle: 58.699999999999726, time: 5.065000000000026
; angle: 58.79999999999973, time: 5.070000000000028
; angle: 58.89999999999973, time: 5.0750000000000295
;Value: 2.4909999999998367

(fastest-throw 45 80 1)
; angle: 20.899999999999192, time: 2.8489999999997973
; angle: 20.999999999999194, time: 2.859999999999796
; angle: 21.099999999999195, time: 2.870999999999795
; angle: 57.299999999999706, time: 5.897000000000304
; angle: 57.39999999999971, time: 5.902000000000306
; angle: 57.49999999999971, time: 5.908000000000308
; angle: 57.59999999999971, time: 5.91400000000031
;Value: 2.8489999999997973
(fastest-throw 55 80 1)
; angle: 12.799999999999136, time: 2.2189999999998666
; angle: 64.99999999999974, time: 7.094000000000704
; angle: 65.09999999999974, time: 7.099000000000705
;Value: 2.2189999999998666
(fastest-throw 35 80 1)
;Value: 0

(fastest-throw 45 90 1)
; angle: 28.899999999999306, time: 3.669999999999707
; angle: 28.999999999999307, time: 3.678999999999706
; angle: 29.09999999999931, time: 3.6889999999997047
; angle: 29.19999999999931, time: 3.6989999999997036
; angle: 29.29999999999931, time: 3.7079999999997026
; angle: 29.399999999999313, time: 3.7179999999997015
; angle: 48.099999999999575, time: 5.293000000000102
; angle: 48.19999999999958, time: 5.300000000000105
; angle: 48.29999999999958, time: 5.307000000000107
; angle: 48.39999999999958, time: 5.31500000000011
; angle: 48.49999999999958, time: 5.322000000000112
; angle: 48.59999999999958, time: 5.329000000000114
; angle: 48.699999999999584, time: 5.336000000000117
;Value: 3.669999999999707
(fastest-throw 55 90 1)
; angle: 16.299999999999127, time: 2.6849999999998153
; angle: 16.399999999999128, time: 2.697999999999814
; angle: 60.39999999999975, time: 6.834000000000617
; angle: 60.49999999999975, time: 6.840000000000619
; angle: 60.59999999999975, time: 6.846000000000621
;Value: 2.6849999999998153
(fastest-throw 35 90 1)
;Value: 0

;; Problem 8

(define (travel-distance-with-bounce n elevation velocity angle)
  (define integrate-with-bounce
    (lambda (u0 v0 dt g m beta)
      (integrate 0 elevation u0 v0 dt g m beta
                 (lambda (x y u v t)
                   (if (= n 0)
		       x
		       (+ x
			  (travel-distance-with-bounce (-1+ n) 0 (/ velocity 2) angle)))))))
  (let ((alpha (degree2radian angle)))
    (integrate-with-bounce (* velocity (cos alpha))
			   (* velocity (sin alpha))
			   0.01
			   gravity
			   mass
			   beta)))

(travel-distance-with-bounce 0 1 35 45)
;Value: 70.30036328016965
(travel-distance-with-bounce 1 1 35 45)
;Value: 96.07822264302163
(travel-distance-with-bounce 2 1 35 45)
;Value: 103.56833418372945
(travel-distance-with-bounce 3 1 35 45)
;Value: 105.52789288882138

(travel-distance-with-bounce 0 1 45 45)
;Value: 92.23060925057175
(travel-distance-with-bounce 1 1 45 45)
;Value: 130.61079130660193
(travel-distance-with-bounce 2 1 45 45)
;Value: 142.54695974193643
(travel-distance-with-bounce 3 1 45 45)
;Value: 145.75359976216043

(travel-distance-with-bounce 0 1 45 35)
;Value: 93.4412381854694
(travel-distance-with-bounce 1 1 45 35)
;Value: 130.8023892860631
(travel-distance-with-bounce 2 1 45 35)
;Value: 142.1209328518085
(travel-distance-with-bounce 3 1 45 35)
;Value: 145.16206094125295

(travel-distance-with-bounce 0 1 45 55)
;Value: 83.33416328784766
(travel-distance-with-bounce 1 1 45 55)
;Value: 118.75015170656962
(travel-distance-with-bounce 2 1 45 55)
;Value: 129.91288893913278
(travel-distance-with-bounce 3 1 45 55)
;Value: 132.92392035350923

;;; arbitrary number of bounces until the ball stops moving
(define (n-bounce elevation velocity angle)
  (define (not-moving? curr next)
    (< (abs (- curr next)) diameter))
  (define (iter cnt curr)
    (let ((next (travel-distance-with-bounce cnt elevation velocity angle)))
      (if (not-moving? curr next)
	  cnt
	  (iter (1+ cnt) next))))
  (iter 0 0))

(define (n-bounce-distance elevation velocity angle)
  (travel-distance-with-bounce (n-bounce elevation velocity angle) elevation velocity angle))

(n-bounce 1 35 45)
;Value: 6
(n-bounce-distance 1 35 45)
;Value: 106.20317355501743

(n-bounce 1 45 45)
;Value: 6
(n-bounce-distance 1 45 45)
;Value: 146.86340632375453

(n-bounce 1 55 45)
;Value: 7
(n-bounce-distance 1 55 45)
;Value: 185.7925414388342

;; Problem 9

(define (travel-distance-with-bounce-with-drag n elevation velocity angle)
  (define integrate-with-bounce-with-drag
    (lambda (u0 v0 dt g m beta)
      (integrate 0 elevation u0 v0 dt g m beta
                 (lambda (x y u v t)
                   (if (= n 0)
		       x
		       (+ x
			  (travel-distance-with-bounce-with-drag (-1+ n) 0 (speed u v) angle)))))))
  (let ((alpha (degree2radian angle)))
    (integrate-with-bounce-with-drag (* velocity (cos alpha))
				     (* velocity (sin alpha))
				     0.01
				     gravity
				     mass
				     beta)))

(travel-distance-with-bounce-with-drag 0 1 35 45)
;Value: 70.30036328016965
(travel-distance-with-bounce-with-drag 1 1 35 45)
;Value: 106.67505120788115
(travel-distance-with-bounce-with-drag 2 1 35 45)
;Value: 130.88717661477688
(travel-distance-with-bounce-with-drag 3 1 35 45)
;Value: 149.05507927786525

(travel-distance-with-bounce-with-drag 0 1 45 45)
;Value: 92.23060925057175
(travel-distance-with-bounce-with-drag 1 1 45 45)
;Value: 133.85412770069894
(travel-distance-with-bounce-with-drag 2 1 45 45)
;Value: 160.3681483172379
(travel-distance-with-bounce-with-drag 3 1 45 45)
;Value: 179.8845837567382

(travel-distance-with-bounce-with-drag 0 1 45 35)
;Value: 93.4412381854694
(travel-distance-with-bounce-with-drag 1 1 45 35)
;Value: 131.7415841395019
(travel-distance-with-bounce-with-drag 2 1 45 35)
;Value: 155.73638899715934
(travel-distance-with-bounce-with-drag 3 1 45 35)
;Value: 173.34357162889785

(travel-distance-with-bounce-with-drag 0 1 45 55)
;Value: 83.33416328784766
(travel-distance-with-bounce-with-drag 1 1 45 55)
;Value: 124.08987078199979
(travel-distance-with-bounce-with-drag 2 1 45 55)
;Value: 150.8477145157898
(travel-distance-with-bounce-with-drag 3 1 45 55)
;Value: 170.81392477502607