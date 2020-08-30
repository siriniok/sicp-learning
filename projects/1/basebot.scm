;;; Vladyslav Siriniok
;;; Project 1, 6.001, Autumn 2018

;; idea is to simulate a baseball robot

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


;; Helper Functions

(define square
  (lambda (x)
    ;; Computes x squared
    ;; Type: number -> number
    (* x x)))

(define assert
  (lambda (predicate message)
    ;; Asserts some predicate
    ;; Returns a list of the predicate value and the message
    ;; Type: (boolean, string) -> list
    (list predicate message)))

(define assert-equal
  (lambda (expected actual)
    ;; Compares the expected value to the actual
    ;; Returns a list of the predicate, expected and actual values
    (assert (equal? expected actual) (list expected actual))))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi 3.14159)


;; Problem 1

(define position
  (lambda (a v u t)
    ;; Calculates a position u1 using equation at²/2 + vt + u
    ;; Type: (number, number, number, number) -> number
    ;; Constraint: t >= 0
    (if (< t 0)
	#f
	(+ (* 1/2 a (square t)) (* v t) u))))

;; tests for position
(assert-equal 0 (position 0 0 0 0))
(assert-equal 20 (position 0 0 20 0))
(assert-equal 60 (position 0 5 10 10))
(assert-equal 10 (position 2 2 2 2))
(assert-equal 185/2 (position 5 5 5 5))
(assert-equal #f (position 0 0 0 -1))


;; Problem 2

(define quadratic-formula
  (lambda (a b c op)
    ;; Finds the roots of quadratic equation
    ;; Uses the quadratic formula (-b ± sqrt(b^2 - 4ac)) / 2a
    ;; Type: (number, number, number, procedure) -> number | boolean
    ;; Constraint: a != 0, b^2 - 4a >= 0
    (define discriminant (- (square b) (* 4 a c)))
    (define first-term (op (- b) (sqrt discriminant)))
    (define second-term (* 2 a))
    (cond ((= a 0) #f)
	  ((< discriminant 0) #f)
	  (else (/ first-term second-term)))))

(define root1
  (lambda (a b c)
    ;; Finds the first root of quadratic equation
    ;; Type: (number, number, number) -> number | boolean
    ;; Constraint: a != 0, b^2 - 4ac >= 0
    (quadratic-formula a b c +)))

(define root2
  (lambda (a b c)
    ;; Finds the first root of quadratic equation
    ;; Type: (number, number, number) -> number | boolean
    ;; Constraint: a != 0, b^2 - 4ac >= 0
    (quadratic-formula a b c -)))

;; tests for root1 and root2
(assert-equal -1/4 (root1 4 9 2))
(assert-equal -2 (root2 4 9 2))

(assert-equal 1/4 (root1 -4 9 -2))
(assert-equal 2 (root2 -4 9 -2))

(assert-equal 0 (root1 5 6 0))
(assert-equal -6/5 (root2 5 6 0))

(assert-equal 0 (root1 5 0 0))
(assert-equal (root1 5 0 0) (root2 5 0 0)) ; D = 0

(assert-equal -1 (root1 2 4 2))
(assert-equal (root1 2 4 2) (root2 2 4 2)) ; D = 0

(assert-equal #f (root1 0 8 6)) ; a can't be 0
(assert-equal #f (root2 0 8 6))

(assert-equal #f (root1 5 3 6)) ; D < 0


;; Problem 3

(define time-to-impact
  (lambda (vertical-velocity elevation)
    ;; Finds the the duration of ball's flight,
    ;; i.e x2 for -0.5gt^2 + vt + h = 0
    ;; Type: (number, number) -> number | boolean
    ;; Constraint: v^2 + 2gh >= 0
    (root2 (* -1/2 gravity) vertical-velocity elevation)))

(assert-equal 0 (time-to-impact 0 0))
(assert-equal 4.0 (time-to-impact 19.6 0))
(assert-equal #f (time-to-impact 10 -20))


;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    ;; Finds when the ball drops to a particular height r
    ;; i.e x2 for -0.5gt^2 + vt + (h - r) = 0
    ;; Type: (number, number, number) -> number | boolean
    ;; Constraint: v^2 + 2g(h -r) >= 0
    (time-to-impact vertical-velocity (- elevation
					 target-elevation))))

(assert-equal 4.0 (time-to-height 19.6 5 5))


;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    ;; Converts degrees to radians
    ;; Type: number -> number
    (/ (* deg pi) 180.)))

(define horizontal-velocity
  (lambda (velocity angle)
    ;; Computes horizontal velocity
    ;; Type: (number, number) -> number
    (* velocity (cos (degree2radian angle)))))

(define vertical-velocity
  (lambda (velocity angle)
    ;; Computes vertical velocity
    ;; Type: (number, number) -> number
    (* velocity (sin (degree2radian angle)))))

(define travel-distance-simple
  (lambda (elevation velocity angle)
    ;; Finds kinematic travel distance
    ;; Type (number, number, number) -> (number)
    (let ((vx (horizontal-velocity velocity angle))
	  (vy (vertical-velocity velocity angle)))
      (let ((ty (time-to-impact vy elevation)))
	(position 0 vx 0 ty)))))

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    ;; Converts meters to feet
    ;; number -> number
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    ;; Converts feet to meters
    ;; number -> nubmer
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    ;; Converts hours to seconds
    ;; number -> number
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    ;; Converts seconds to hours
    ;; number -> number
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees

;; what is the distance traveled in each case?
;; record both in meters and in feet
(assert-equal 20.32892781536815 (travel-distance-simple 1 45 0))
(assert-equal 67.0854617907149 (meters-to-feet (travel-distance-simple 1 45 0)))
(assert-equal 207.6278611514906 (travel-distance-simple 1 45 45))
(assert-equal 685.171941799919 (meters-to-feet (travel-distance-simple 1 45 45)))
(assert-equal 5.496418989612468e-4 (travel-distance-simple 1 45 90))
(assert-equal 1.8138182665721145e-3 (meters-to-feet (travel-distance-simple 1 45 90)))


;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define (find-best-angle velocity elevation)
  ;; Finds the angle that gives the best distance
  ;; Type: (number, number) -> number
  (define alpha-increment 0.01)
  (define max-angle 90.0)

  (define (find-best-angle-iter best-distance best-angle angle)
    (let ((distance (travel-distance-simple elevation velocity angle)))
      (cond ((> angle max-angle) best-angle)
	    ((> best-distance distance) best-angle)
	    (else (find-best-angle-iter distance angle (+ angle alpha-increment))))))
  (find-best-angle-iter 0 0 0))

;; find best angle
;; try for other velocities
;; try for other heights
(assert-equal 89.99000000000913 (find-best-angle 0 0))
(assert-equal 44.999999999999616 (find-best-angle 5 0))
(assert-equal 44.999999999999616 (find-best-angle 25 0))
(assert-equal 44.999999999999616 (find-best-angle 45 0))
(assert-equal 44.999999999999616 (find-best-angle 90 0))

(assert-equal 89.99000000000913 (find-best-angle 0 1))
(assert-equal 36.820000000001244 (find-best-angle 5 1))
(assert-equal 44.559999999999704 (find-best-angle 25 1))
(assert-equal 44.859999999999644 (find-best-angle 45 1))
(assert-equal 44.96999999999962 (find-best-angle 90 1))

(assert-equal 89.99000000000913 (find-best-angle 0 10))
(assert-equal 18.590000000000106 (find-best-angle 5 10))
(assert-equal 41.10000000000039 (find-best-angle 25 10))
(assert-equal 43.67999999999988 (find-best-angle 45 10))
(assert-equal 44.659999999999684 (find-best-angle 90 10))


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
;; let speed = (u^2+v^2)^(1/2), then
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
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(define (integrate x y u v t m g beta dt cb)
  ;; Integrates 
  ;; dx = u dt
  ;; dy = v dt
  ;; du = - 1/m speed beta u dt
  ;; dv = - (1/m speed beta v + g)
  ;; until y reaches the desired point
  ;; Type: (number, number, number, number, number, number, number,
  ;;        number, number, procedure) -> number
  (define (speed u v) (sqrt (+ (square u) (square v))))
    
  (if (< y 0)
      (cb x y u v t)
      (integrate (+ x (* u dt))
		 (+ y (* v dt))
		 (+ u (* (/ -1 m) (speed u v) u beta dt))
		 (+ v (* -1 (+ (* (/ 1 m) (speed u v) v beta) g) dt))
		 (+ t dt)
		 m g beta dt cb)))

(define integrate-distance
  (lambda (x y u v m g beta dt)
    (integrate x y u v 0 m g beta dt (lambda (x y u v t) x))))

(define integrate-time
  (lambda (x y u v m g beta dt)
    (integrate x y u v 0 m g beta dt (lambda (x y u v t) t))))

(define travel-distance
  (lambda (beta elevation velocity angle)
    (integrate-distance 0 elevation
			(horizontal-velocity velocity angle)
			(vertical-velocity velocity angle)
			mass gravity beta 0.01)))

(define travel-time
  (lambda (beta elevation velocity angle)
    (integrate-time 0 elevation
		    (horizontal-velocity velocity angle)
		    (vertical-velocity velocity angle)
		    mass gravity beta 0.01)))

;; RUN SOME TEST CASES

;; Let's try the same values as for the travel-distance-simple
(assert-equal 19.342554900331546 (travel-distance beta 1 45 0))
(assert-equal 92.23060925057175 (travel-distance beta 1 45 45))
(assert-equal 2.4126919125841404e-4 (travel-distance beta 1 45 90))

(assert-equal .47000000000000025 (travel-time beta 1 45 0))
(assert-equal 5.069999999999936 (travel-time beta 1 45 45))
(assert-equal 6.919999999999897 (travel-time beta 1 45 90))

;; what about Denver?
(define denver-density 1.06)
(define denver-beta (* .5 drag-coeff denver-density
		       (* 3.14159 .25 (square diameter))))

(assert-equal 19.591523207205473 (travel-distance denver-beta 1 45 0))
(assert-equal 99.82569987946395 (travel-distance denver-beta 1 45 45))
(assert-equal 2.621293723634342e-4 (travel-distance denver-beta 1 45 90))

(assert-equal .47000000000000025 (travel-time denver-beta 1 45 0))
(assert-equal 5.209999999999933 (travel-time denver-beta 1 45 45))
(assert-equal 7.129999999999892 (travel-time denver-beta 1 45 90))

;; Let's try different velocities with an angle of 45 degrees
(assert-equal 91.68807578657687 (travel-distance beta 0 45 45))
(assert-equal 81.11581595016993 (travel-distance beta 0 40 45))
(assert-equal 69.63190099516319 (travel-distance beta 0 35 45))

(assert-equal 5.019999999999937 (travel-time beta 0 45 45))
(assert-equal 4.649999999999945 (travel-time beta 0 40 45))
(assert-equal 4.229999999999954 (travel-time beta 0 35 45))

;; How quickly does the distance drop when the angle changes?
(assert-equal 93.14981124173583 (travel-distance beta 0 45 40))
(assert-equal 91.68807578657687 (travel-distance beta 0 45 45))
(assert-equal 88.31120906026788 (travel-distance beta 0 45 50))

(assert-equal 4.619999999999946 (travel-time beta 0 45 40))
(assert-equal 5.019999999999937 (travel-time beta 0 45 45))
(assert-equal 5.3899999999999295 (travel-time beta 0 45 50))


;; Problem 7
 
;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance

;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s

(define (find-required-angle beta elevation velocity target)
  (define tolerance 0.1)
  (define alpha-increment 0.01)
  (define min-angle -90)
  (define max-angle 90)
  
  (define (fit-in-distance distance)
    (let ((diff (abs (- distance target))))
      (if (> diff tolerance) 0 diff)))
 
  (define (find-required-angle-iter best-distance best-angle angle)
    (let ((distance (travel-distance beta elevation velocity angle)))
      (cond ((> angle max-angle) best-angle)
	    ((> (fit-in-distance best-distance) (fit-in-distance distance)) best-angle)
	    (else (find-required-angle-iter distance angle (+ angle alpha-increment))))))
  (find-required-angle-iter 0 0 min-angle))

;; Let's try some distances with velocity 45 m/s
(assert-equal 0 (find-required-angle beta 1 45 90))
(assert-equal 0 (find-required-angle beta 1 45 36))



;; You can use this to get a sense of times involved in baseball.
;; For example, the distance from home plate to second base is roughly
;; 36m.  If your catcher has a gun for an arm, and can throw at 100
;; mph, (or 45 m/sec), how long does it take for the throw to reach
;; second base?  How long if he throws at 35 m/sec? of at 55 m/sec?
;; Note that a really good base runner should be able to get from
;; first to second base in roughly 3 seconds.
;; If the pitcher is throwing at 90 mph how long does it take to reach
;; home?  If the catcher throws at 90 mph, how much time does he have
;; to catch and release the ball if he is going to put out a runner
;; trying to steal second? Now use your procedures to get some data on
;; outfielders.  Suppose an outfielder has a strong arm and can throw
;; at 45m/sec.
;; How quickly can he throw the ball to a target at a distance of 30m?
;; 60m?  80m?
;; What if he can throw 55 m/sec? What about the same distances but
;; with a weaker outfielder, who can only throw at 35m/sec?


;; find best angle
;; try for other velocities
;; try for other heights
(assert-equal 89.99000000000913 (find-best-angle 0 0))
(assert-equal 44.999999999999616 (find-best-angle 5 0))
(assert-equal 44.999999999999616 (find-best-angle 25 0))
(assert-equal 44.999999999999616 (find-best-angle 45 0))
(assert-equal 44.999999999999616 (find-best-angle 90 0))

(assert-equal 89.99000000000913 (find-best-angle 0 1))
(assert-equal 36.820000000001244 (find-best-angle 5 1))
(assert-equal 44.559999999999704 (find-best-angle 25 1))
(assert-equal 44.859999999999644 (find-best-angle 45 1))
(assert-equal 44.96999999999962 (find-best-angle 90 1))

(assert-equal 89.99000000000913 (find-best-angle 0 10))
(assert-equal 18.590000000000106 (find-best-angle 5 10))
(assert-equal 41.10000000000039 (find-best-angle 25 10))
(assert-equal 43.67999999999988 (find-best-angle 45 10))
(assert-equal 44.659999999999684 (find-best-angle 90 10))


;; handle 0 case!
(define (best-time-to-target beta elevation velocity target)
  (travel-time beta
	       elevation
	       velocity
	       (find-required-angle beta elevation velocity target)))



;; Problem 8

;; Problem 9
