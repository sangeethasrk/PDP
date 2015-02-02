;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bounce) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(define TIME-ON-TASK 45)

(provide INITIAL-WORLD)
(provide next-world)
;(provide key-handler)
;(provide mouse-handler)
(provide world-ball)
;(provide world-paused?)
;(provide ticks-since-click)
;(provide score)
(provide ball-x)
(provide ball-y)

; canvas constants
(define CENTER-X 150) ; pixels
(define CENTER-Y 200) ; pixels
(define WIDTH (* 2 CENTER-X)) ; pixels
(define HEIGHT (* 2 CENTER-Y)) ; pixels
(define EMPTY-SCENE (empty-scene WIDTH HEIGHT)) ; Image
 
; Ball constants
(define BALL-RADIUS 20) ; pixels
(define BALL-IMG (circle BALL-RADIUS "solid" "black")) ; Image

(define VERTICAL-VELOCITY 0) ; pixels/tick
(define HORIZONTAL-VELOCITY 3) ; pixels/tick
(define TIME 1) ; ticks
(define ACCLN 1) ; acceleration in pixel/tick^2
(define BOUNCE-COEFFICIENT 0.9) ; Number

; A Direction is one of:
; - "right"
; - "left"
; - "top"
; - "down"
; INTERP: represents a canvas direction where "right" is x-increasing.
(define RIGHT "right")
(define LEFT "left")
(define TOP "top")
(define DOWN "down")
 
; <direction predicates> : Direction -> Boolean
; Returns true if d is the Direction indicated by the function name.
; EXAMPLES:
(begin-for-test
  (check-equal? (right? "right")
                #true)
  (check-equal? (left? "right")
                #false)
  (check-equal? (top? "top")
                #true)
  (check-equal? (down? DOWN)
                #true))
; STRATEGY: function composition
(define (right? d) (string=? d RIGHT))
(define (left? d) (string=? d LEFT))
(define (top? d) (string=? d TOP))
(define (down? d) (string=? d DOWN))

; A Ball is a (make-ball Coordinate Coordinate x-Direction y-Direction Number)
; INTERP: The Coordinates are the ball's x position and y position respectively
; x-Direction refers to ball's direction along x axis which can be either or 
; left or right and y-Direction refers to ball's direction along y axis which 
; can be either of top or down, and Number represents velocity in pixels/tick
(define-struct ball (xposn yposn xdir ydir vel))
 
; TEMPLATE:
; ball-fn : Ball -> ???
; (define (ball-fn b)
;  (... (ball-xposn b) ... 
;       (ball-xposn b) ...
;       (ball-xdir b)) ...
;       (ball-ydir b)) ...
;       (ball-vel b) ...))

(define INIT-BALL (make-ball CENTER-X BALL-RADIUS RIGHT DOWN VERTICAL-VELOCITY))
(define BALL-X-RIGHT-EDGE (- WIDTH BALL-RADIUS)) ; x Coordinate
(define BALL-X-LEFT-EDGE BALL-RADIUS) ; x Coordinate
(define BALL-Y-DOWN-EDGE (- HEIGHT BALL-RADIUS)) ; y Coordinate
(define BALL-Y-TOP-EDGE BALL-RADIUS) ; y Coordinate
 
; A World is a Ball
(define INITIAL-WORLD INIT-BALL)

; run : World -> World
; Starts the simulation.
; STRATEGY: function composition
(define (run init-world)
  (big-bang init-world
   (on-tick next-world)
   (to-draw draw)))

; Wish List:

; next-world : World -> World	      ; Computes the next World state. 

; draw : World -> Image	              ; Renders the current World state 
; into an Image.

; key-handler: World Key-Event -> World  ; Computes the next world state based 
; on the key event occured.

; mouse-handler: World Coordinate Coordinate Mouse-Event -> World  ; Computes 
; the next world state based on the mouse event occured and the position where 
; the mouse event occured

; draw : World -> Image
; Renders the current World state.
; EXAMPLES:
(begin-for-test
  (check-equal? (draw INIT-BALL)
                (place-image BALL-IMG CENTER-X BALL-RADIUS EMPTY-SCENE))
  (check-equal? (draw (make-ball BALL-X-LEFT-EDGE CENTER-Y RIGHT DOWN 0))
                (place-image BALL-IMG BALL-X-LEFT-EDGE CENTER-Y EMPTY-SCENE)))
; STRATEGY: function composition
(define (draw w)
  (draw-ball-on w EMPTY-SCENE))
 
; draw-ball-on : Ball Image -> Image
; Draws Ball b onto Image img.
; WHERE: BALL-RADIUS <= (ball-x b) <= ((image-width img) - BALL-RADIUS)
;        (Ball b is completely within img.)
; EXAMPLES:
(begin-for-test
  (check-equal? (draw-ball-on INIT-BALL EMPTY-SCENE)
                (place-image BALL-IMG CENTER-X BALL-RADIUS EMPTY-SCENE))
  (check-equal? (draw-ball-on 
                 (make-ball BALL-X-LEFT-EDGE CENTER-Y RIGHT DOWN 0) EMPTY-SCENE)
                (place-image BALL-IMG BALL-X-LEFT-EDGE CENTER-Y EMPTY-SCENE)))
; STRATEGY: data decomposition on b : Ball
(define (draw-ball-on b img)
  (place-image BALL-IMG (ball-xposn b) (ball-yposn b) img))

; next-world : Ball -> Ball
; Computes the next world state of the ball.
; WHERE: Ball b is completely on the canvas.
; EXAMPLES:
(begin-for-test
  (check-equal? (next-world INIT-BALL)
                (make-ball 153 21.5 RIGHT DOWN 1))
  (check-equal? (next-world (make-ball 20 CENTER-Y LEFT TOP 5))
                (make-ball 20 195.5 RIGHT TOP 4)))
; STRATEGY: Function Composition
(define (next-world b)
  (cond
    [(ball-inside-canvas? b) (next-ball-insidecanvas b)]
    [else (next-ball-moverestricted b)]))

; ball-inside-canvas? : Ball -> Boolean
; Checks whether ball is inside the canvas.
(begin-for-test
  (check-equal? (ball-inside-canvas? INIT-BALL)
                #true)
  (check-equal? (ball-inside-canvas? (make-ball 20 CENTER-Y LEFT TOP 5))
                #false))
; STRATEGY: Function Composition
(define (ball-inside-canvas? b)
  (if 
   (and 
    (within-xlimits? (ball-xposn b) (ball-xdir b))
    (within-ylimits? (ball-yposn b) (ball-ydir b) (ball-vel b))) #true #false))

; within-xlimits? : Coordinate Direction -> Boolean
; Checks whether the ball is within the x axis boundaries of the canvas
; EXAMPLES:
(begin-for-test
  (check-equal? (within-xlimits? 30 LEFT)
                #true)
  (check-equal? (within-xlimits? 280 RIGHT)
                #false))
; STRATEGY: Function Composition
(define (within-xlimits? x dir)
  (and 
     (<= (next-x x dir) BALL-X-RIGHT-EDGE)
     (>= (next-x x dir) BALL-X-LEFT-EDGE)))

; within-ylimits? : Coordinate Direction Number -> Boolean
; Checks whether the ball is within the y axis boundaries of the canvas
; EXAMPLES:
(begin-for-test
  (check-equal? (within-ylimits? 30 TOP 3)
                #true)
  (check-equal? (within-ylimits? 380 DOWN 2)
                #false))
; STRATEGY: Function Composition
(define (within-ylimits? y dir vel)
  (and
     (>= (next-y y dir vel) BALL-Y-TOP-EDGE)
     (<= (next-y y dir vel) BALL-Y-DOWN-EDGE)))

; next-x : Coordinate Direction -> Coordinate
; Computes the next x position in Direction dir.
; EXAMPLES:
(begin-for-test
  (check-equal? (next-x 50 LEFT)
                47)
  (check-equal? (next-x 30 RIGHT)
                33))
; STRATEGY: Data Decomposition on x : ball-xposn 
(define (next-x  x dir)
  (cond     
    [(right? dir) (+ x HORIZONTAL-VELOCITY)]
    [(left? dir) (- x HORIZONTAL-VELOCITY)]))

; next-y : Coordinate Direction Number-> Coordinate
; Computes the next y position in Direction dir with Velocity vel.
; EXAMPLES:
(begin-for-test
  (check-equal? (next-y 10 TOP 5)
                5.5)
  (check-equal? (next-y 5 DOWN 2)
                8.5))
; STRATEGY: Data Decomposition on y : ball-yposn 
(define (next-y y dir vel) 
  (cond
    [(down? dir) (+ y (calc-velocity vel dir) 0.5)]
    [(top? dir) (- y (+ (calc-velocity vel dir) 0.5))]))

; calc-velocity : Number -> Number
; Computes the new vertical velocity based on the direction of the ball
; EXAMPLES:
(begin-for-test
  (check-equal? (calc-velocity 10 TOP)
                9)
  (check-equal? (calc-velocity 5 DOWN)
                6))
; STRATEGY: Function Composition
(define (calc-velocity vel dir)
  (cond
    [(top? dir) (- vel ACCLN)] 
    [(down? dir) (+ vel ACCLN)]))  

; next-ball-insidecanvas : Ball -> Ball
; Computes the next ball state.
; WHERE: Ball b is completely on the canvas.
; EXAMPLES:
(begin-for-test
  (check-equal? (next-ball-insidecanvas (make-ball 25 50 RIGHT DOWN 2))
                (make-ball 28 53.5 RIGHT DOWN 3)))
; STRATEGY: Data Decomposition on b : Ball
(define (next-ball-insidecanvas b)
  (make-ball 
   (next-x (ball-xposn b) (ball-xdir b)) 
   (next-y (ball-yposn b) (ball-ydir b) (ball-vel b))
   (ball-xdir b)
   (ball-ydir b)
   (calc-velocity (ball-vel b) (ball-ydir b))))

; next-ball-moverestricted : Ball -> Ball
; Computes the next ball state when its position 
; caculates to a location outside the canvas.
; EXAMPLES:
(begin-for-test
  (check-equal? (next-ball-moverestricted (make-ball 20 50 LEFT DOWN 2))
                (make-ball BALL-X-LEFT-EDGE 53.5 RIGHT DOWN 3)))
; STRATEGY: Data Decomposition on b : Ball
(define (next-ball-moverestricted b)
  (make-ball 
   (get-restricted-x (ball-xposn b) (ball-xdir b))
   (get-restricted-y (ball-yposn b) (ball-ydir b) (ball-vel b))
   (get-x-chngd-dir (ball-xposn b) (ball-xdir b))
   (get-y-chngd-dir (ball-yposn b) (ball-ydir b) (ball-vel b))
   (get-ball-velocity (ball-yposn b) (ball-ydir b) (ball-vel b))))
 
; get-restricted-x : Coordinate Direction -> Coordinate
; Computes the restricted x position in Direction dir.
; EXAMPLES:
(begin-for-test
  (check-equal? (get-restricted-x BALL-X-LEFT-EDGE LEFT)
                BALL-X-LEFT-EDGE)
  (check-equal? (get-restricted-x BALL-X-RIGHT-EDGE RIGHT)
                BALL-X-RIGHT-EDGE)
  (check-equal? (get-restricted-x 50 RIGHT)
                53))
; STRATEGY: Function Composition
(define (get-restricted-x x dir)
  (cond
    [(< (next-x x dir) BALL-X-LEFT-EDGE) BALL-X-LEFT-EDGE]
    [(> (next-x x dir) BALL-X-RIGHT-EDGE) BALL-X-RIGHT-EDGE]
    [else (next-x x dir)]))

; get-restricted-y : Coordinate Direction Number -> Coordinate
; Computes the restricted y position in Direction dir.
; EXAMPLES:
(begin-for-test
  (check-equal? (get-restricted-y BALL-Y-TOP-EDGE TOP 2)
                BALL-Y-TOP-EDGE)
  (check-equal? (get-restricted-y BALL-Y-DOWN-EDGE DOWN 1)
                BALL-Y-DOWN-EDGE)
  (check-equal? (get-restricted-y 50 DOWN 6)
                57.5))
; STRATEGY: Function Composition
(define (get-restricted-y y dir vel)
  (cond
    [(< (next-y y dir vel) BALL-Y-TOP-EDGE) BALL-Y-TOP-EDGE]
    [(> (next-y y dir vel) BALL-Y-DOWN-EDGE) BALL-Y-DOWN-EDGE]
    [else (next-y y dir vel)]))

; get-x-chngd-dir : Coordinate Direction Number -> Direction
; Changes the direction of the Ball along x axis if the ball 
; touches the boundaries of the canvas
; EXAMPLES:
(begin-for-test
  (check-equal? (get-x-chngd-dir 20 LEFT)
                RIGHT)
  (check-equal? (get-x-chngd-dir 280 RIGHT)
                LEFT)
  (check-equal? (get-x-chngd-dir 150 LEFT)
                LEFT))
; STRATEGY: Function Composition
(define (get-x-chngd-dir x dir)
  (cond
    [(<= (next-x x dir) BALL-X-LEFT-EDGE) RIGHT]
    [(>= (next-x x dir) BALL-X-RIGHT-EDGE) LEFT]
    [else dir]))

; get-y-chngd-dir : Coordinate Direction Number -> Direction
; Changes the direction of the Ball along y axis if the ball 
; touches the boundaries of the canvas
; EXAMPLES:
(begin-for-test
  (check-equal? (get-y-chngd-dir 20 TOP 0.5)
                DOWN)
  (check-equal? (get-y-chngd-dir 380 DOWN 7)
                TOP)
  (check-equal? (get-y-chngd-dir 200 DOWN 7)
                DOWN)) 
; STRATEGY: Function Composition
(define (get-y-chngd-dir y dir vel)
  (cond
    [(<= (next-y y dir vel) BALL-Y-TOP-EDGE) DOWN]
    [(>= (next-y y dir vel) BALL-Y-DOWN-EDGE) TOP]
    [else dir]))

; get-ball-velocity : Coordinate Direction Number -> Number
; Computes the ball's velocity based on its direction and y coordinate value
; A negative velocity represents change in direection hence it corrects negative
; velocity values calculated, to positive values
; EXAMPLES:
(begin-for-test
  (check-equal? (get-ball-velocity 30 TOP 0.5)
                0.5)
  (check-equal? (get-ball-velocity 50 DOWN 7)
                8))
; STRATEGY: Function Composition
(define (get-ball-velocity y dir vel)
  (if
   (< (get-restrictedball-velocity y dir vel) 0)
   (* (get-restrictedball-velocity y dir vel) 
      (sub1 0)) 
   (get-restrictedball-velocity y dir vel)))

; get-restrictedball-velocity : Coordinate Direction Number -> Number
; Computes the ball's velocity based on its direction and y coordinate value
; EXAMPLES:
(begin-for-test
  (check-equal? (get-restrictedball-velocity BALL-Y-DOWN-EDGE DOWN 5)
                4.5)
  (check-equal? (get-restrictedball-velocity 50 DOWN 5)
                6))
; STRATEGY: Function Composition
(define (get-restrictedball-velocity y dir vel)
  (if
   (= (get-restricted-y y dir vel) 
      BALL-Y-DOWN-EDGE) 
   (get-new-velocity vel)
   (calc-velocity vel dir)))
 
; get-new-velocity: Number -> Number
; Calculates the new velocity after the ball has hit the ground using 
; bounce coefficient
; EXAMPLES:
(begin-for-test
  (check-equal? (get-new-velocity 5)
                4.5))
; STRATEGY: Data Decomposition on vel : ball-vel ; ball velocity
(define (get-new-velocity vel)
  (* vel BOUNCE-COEFFICIENT))

; world-ball : World -> Ball
; It is a representation of the ball
; EXAMPLES:
(begin-for-test
  (check-equal? (world-ball INIT-BALL)
                (make-ball CENTER-X BALL-Y-TOP-EDGE RIGHT DOWN 0)))
; STRATEGY: Data Decomposition on w : World
(define (world-ball w)
  (make-ball 
   (ball-xposn w)
   (ball-yposn w)
   (ball-xdir w)
   (ball-ydir w)
   (ball-vel w)))

; ball-x : Ball -> Coordinate
; Gets the x Coordinate from the ball's current position
; EXAMPLES: 
(begin-for-test
  (check-equal? (ball-x INIT-BALL)
                CENTER-X)
  (check-equal? (ball-x 
                 (make-ball BALL-X-RIGHT-EDGE BALL-Y-TOP-EDGE LEFT DOWN 0)) 
                BALL-X-RIGHT-EDGE))
; STRATEGY: data decomposition on b : Ball 
(define (ball-x b)
  (ball-xposn b))

; ball-y : Ball -> Coordinate
; Gets the y Coordinate from the ball's current position
; EXAMPLES: 
(begin-for-test
  (check-equal? (ball-y INIT-BALL)
                BALL-RADIUS)
  (check-equal? (ball-y 
                 (make-ball BALL-X-RIGHT-EDGE BALL-Y-TOP-EDGE LEFT DOWN 4.5)) 
                BALL-Y-TOP-EDGE))
; STRATEGY: data decomposition on b : Ball
(define (ball-y b)
  (ball-yposn b))

(run INITIAL-WORLD)