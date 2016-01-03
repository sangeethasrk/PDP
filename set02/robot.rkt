;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 13)

(provide initial-robot)
(provide robot-left)
(provide robot-right)
(provide robot-x)
(provide robot-y)
(provide robot-forward)

; A ROBOT-RADIUS is a Pixel
; INTERP: ROBOT-RADIUS represents the size of the robot in pixels
(define ROBOT-RADIUS 15)

; A ROOM-WIDTH is a Pixel
; INTERP: ROOM-WIDTH represents the width of the room in pixels
(define ROOM-WIDTH 200)

; A ROOM-HEIGHT is a Pixel
; INTERP: ROOM-HEIGHT represents the height of the room in pixels
(define ROOM-HEIGHT 400)

; A FLUSH-WIDTH is a Pixel
; INTERP: FLUSH-WIDTH represents the boundary condition in terms of width of the room for a robot with a radius equal to ROBOT-RADIUS
(define FLUSH-WIDTH (- ROOM-WIDTH ROBOT-RADIUS))  

; A FLUSH-HEIGHT is a Pixel
; INTERP: FLUSH-HEIGHT represents the boundary condition in terms of height of the room for a robot with a radius equal to ROBOT-RADIUS
(define FLUSH-HEIGHT (- ROOM-HEIGHT ROBOT-RADIUS))   

; A direction is a String and is one of:
; - "Up"
; - "Down"
; - "Left"
; - "Right" 
(define UP "Up")
(define DOWN "Down")
(define LEFT "Left")
(define RIGHT "Right")

; A robot is a (make-robot xcomponent ycomponent direction)
; INTERP: xcomponent is a coordinate, ycomponent is a coordinante and 
; A direction represents the current direction of the robot, it is a String and is one of:
; - "Up"
; - "Down"
; - "Left"
; - "Right"
(define-struct robot (xcomponent ycomponent direction))

; initial-robot : Coordinate Coordinate -> Robot
; Returns a Robot located at (x,y), facing up.
; STRATEGY: Data Decomposition

(begin-for-test
  (check-equal? (initial-robot 0 0)
                (make-robot 0 0 "Up")
                "The 'initial-robot' fuction at location 0,0 failed")
  (check-equal? (initial-robot 200 400)
                (make-robot 200 400 "Up")
                "The 'initial-robot' fuction at location 200,400 failed")
  (check-equal? (initial-robot -10 210)
                (make-robot -10 210 "Up")
                "The 'initial-robot' fuction at location -10,210 failed")
  (check-equal? (initial-robot 220 450)
                (make-robot 220 450 "Up")
                "The 'initial-robot' fuction at location 220,450 failed")
  (check-equal? (initial-robot -30 -50)
                (make-robot -30 -50 "Up")
                "The 'initial-robot' fuction at location -30,-50 failed"))


(define (initial-robot x y) 
 (make-robot x y UP))

; robot-left : Robot -> Robot
; Returns a Robot like r, but turned 90 degrees left to the current facing direction of the Robot
; STRATEGY: Data Decomposition

(begin-for-test
  (check-equal? (robot-left (make-robot 0 0 "Up"))
                (make-robot 0 0 "Left")
                "The 'robot-left' fuction with current direction as 'Up' failed to turn the robot left")
  (check-equal? (robot-left (make-robot 200 400 "Down"))
                (make-robot 200 400 "Right")
                "The 'robot-left' fuction with current direction as 'Down' failed to turn the robot left")
  (check-equal? (robot-left (make-robot -10 210 "Right"))
                (make-robot -10 210 "Up")
                "The 'robot-left' fuction with current direction as 'Right' failed to turn the robot left")
  (check-equal? (robot-left (make-robot 220 450 "Left"))
                (make-robot 220 450 "Down")
                "The 'robot-left' fuction with current direction as 'Left' failed to turn the robot left")
  (check-equal? (robot-left (make-robot -30 -50 "Right"))
                (make-robot -30 -50 "Up")
                "The 'robot-left' fuction with current direction as 'Right' failed to turn the robot left"))

(define (robot-left r)
 (cond
    [(string=? (robot-direction r) UP) (make-robot (robot-xcomponent r) (robot-ycomponent r) LEFT)]
    [(string=? (robot-direction r) LEFT) (make-robot (robot-xcomponent r) (robot-ycomponent r) DOWN)]
    [(string=? (robot-direction r) DOWN) (make-robot (robot-xcomponent r) (robot-ycomponent r) RIGHT)]
    [(string=? (robot-direction r) RIGHT) (make-robot (robot-xcomponent r) (robot-ycomponent r) UP)]))


; robot-right : Robot -> Robot
; Returns a Robot like r, but turned 90 degrees right to the current facing direction of the Robot
; STRATEGY: Data Decomposition

(begin-for-test
  (check-equal? (robot-right (make-robot 0 0 "Up"))
                (make-robot 0 0 "Right")
                "The 'robot-right' fuction with current direction as 'Up' failed to turn the robot right")
  (check-equal? (robot-right (make-robot 200 400 "Down"))
                (make-robot 200 400 "Left")
                "The 'robot-right' fuction with current direction as 'Down' failed to turn the robot right")
  (check-equal? (robot-right (make-robot -10 210 "Right"))
                (make-robot -10 210 "Down")
                "The 'robot-right' fuction with current direction as 'Right' failed to turn the robot right")
  (check-equal? (robot-right (make-robot 220 450 "Left"))
                (make-robot 220 450 "Up")
                "The 'robot-right' fuction with current direction as 'Left' failed to turn the robot right")
  (check-equal? (robot-right (make-robot -30 -50 "Right"))
                (make-robot -30 -50 "Down")
                "The 'robot-right' fuction with current direction as 'Right' failed to turn the robot right"))

(define (robot-right r)
 (cond
    [(string=? (robot-direction r) UP) (make-robot (robot-xcomponent r) (robot-ycomponent r) RIGHT)]
    [(string=? (robot-direction r) LEFT) (make-robot (robot-xcomponent r) (robot-ycomponent r) UP)]
    [(string=? (robot-direction r) DOWN) (make-robot (robot-xcomponent r) (robot-ycomponent r) LEFT)]
    [(string=? (robot-direction r) RIGHT) (make-robot (robot-xcomponent r) (robot-ycomponent r) DOWN)]))

; robot-x : Robot -> Coordinate
; Returns the x component of the Robot's location.
; STRATEGY: Data Decomposition

(begin-for-test
  (check-equal? (robot-x (make-robot 0 0 "Up"))
                0
                "The 'robot-x' fuction failed to return the x component of the robot's current location")
  (check-equal? (robot-x (make-robot 200 400 "Down"))
                200
                "The 'robot-x' fuction failed to return the x component of the robot's current location")
  (check-equal? (robot-x (make-robot -10 210 "Right"))
                -10
                "The 'robot-x' fuction failed to return the x component of the robot's current location")
  (check-equal? (robot-x (make-robot 220 450 "Left"))
                220
                "The 'robot-x' fuction failed to return the x component of the robot's current location")
  (check-equal? (robot-x (make-robot -30 -50 "Right"))
                -30
                "The 'robot-x' fuction failed to return the x component of the robot's current location"))

(define (robot-x r)
  (robot-xcomponent r))

; robot-y : Robot -> Coordinate
; Returns the y component of the Robot's location.
; STRATEGY: Data Decomposition

(begin-for-test
  (check-equal? (robot-y (make-robot 0 0 "Up"))
                0
                "The 'robot-y' fuction failed to return the y component of the robot's current location")
  (check-equal? (robot-y (make-robot 200 400 "Down"))
                400
                "The 'robot-y' fuction failed to return the y component of the robot's current location")
  (check-equal? (robot-y (make-robot -10 210 "Right"))
                210
                "The 'robot-y' fuction failed to return the y component of the robot's current location")
  (check-equal? (robot-y (make-robot 220 450 "Left"))
                450
                "The 'robot-y' fuction failed to return the y component of the robot's current location")
  (check-equal? (robot-y (make-robot -30 -50 "Right"))
                -50
                "The 'robot-y' fuction failed to return the y component of the robot's current location"))

(define (robot-y r)
  (robot-ycomponent r))

; robot-forward : Robot NonNegReal -> Robot
; Returns a Robot like r, but moved forward by d pixels.
; STRATEGY: Function Composition

(begin-for-test
  (check-equal? (robot-forward (make-robot 0 0 "Up") 10)
                (make-robot 0 -10 "Up")
                "The 'robot-forward' fuction failed to move the robot 10 pixels upwards")
  (check-equal? (robot-forward (make-robot 200 400 "Down") 20)
                (make-robot 200 420 "Down")
                "The 'robot-forward' fuction failed to move the robot 20 pixels down")
  (check-equal? (robot-forward (make-robot -10 210 "Right") 200)  ;To test the boundary condition along x-axis
                (make-robot 185 210 "Right")
                "The 'robot-forward' fuction failed to move the robot 200 pixels right")
  (check-equal? (robot-forward (make-robot 220 450 "Left") 5)
                (make-robot 215 450 "Left")
                "The 'robot-forward' fuction failed to move the robot 5 pixels left")
  (check-equal? (robot-forward (make-robot -30 -50 "Right") 25)
                (make-robot -5 -50 "Right")
                "The 'robot-forward' fuction failed to move the robot 25 pixels right")
  (check-equal? (robot-forward (make-robot 50 200 "Down") 35)
                (make-robot 50 235 "Down")
                "The 'robot-forward' fuction failed to move the robot 35 pixels down")
  (check-equal? (robot-forward (make-robot 20 210 "Up") 220)  ;To test the boundary condition along y-axis
                (make-robot 20 15 "Up")
                "The 'robot-forward' fuction failed to move the robot 200 pixels up"))

(define (robot-forward r d)
  (cond
    [(string=? (robot-direction r) UP) (robot-moveup r d)]
    [(string=? (robot-direction r) DOWN) (robot-movedown r d)]
    [(string=? (robot-direction r) LEFT) (robot-moveleft r d)]
    [(string=? (robot-direction r) RIGHT) (robot-moveright r d)]))

; robot-moveup : Robot NonNegReal -> Robot
; Returns a Robot like r whose direction is "Up" and has moved 'upwards' by d pixels
; Checks for boundary condition too, i.e, if the robot is inside the room and 
; moving it 'upwards' by d pixels would put it outside the room, it stops at the top boundary of the canvas/room
; STRATEGY: Data Decomposition

(begin-for-test
  (check-equal? (robot-moveup (make-robot 0 0 "Up") 10)
                (make-robot 0 -10 "Up")
                "The 'robot-moveup' fuction failed to move the robot 10 pixels upwards")
  (check-equal? (robot-moveup (make-robot 200 400 "Up") 20)
                (make-robot 200 380 "Up")
                "The 'robot-moveup' fuction failed to move the robot 20 pixels upwards")
  (check-equal? (robot-moveup (make-robot 30 410 "Up") 200)  
                (make-robot 30 210 "Up")
                "The 'robot-moveup' fuction failed to move the robot 200 pixels right")
  (check-equal? (robot-moveup (make-robot 20 20 "Up") 15)  ;To test the boundary condition along y-axis/Top of the canvas
                (make-robot 20 15 "Up")
                "The 'robot-moveup' fuction failed to move the robot 5 pixels upwards")
  (check-equal? (robot-moveup (make-robot -30 -50 "Up") 25)
                (make-robot -30 -75 "Up")
                "The 'robot-moveup' fuction failed to move the robot 25 pixels upwards"))

(define (robot-moveup r d)
  (cond
    [(or (< (robot-xcomponent r) ROBOT-RADIUS) (> (robot-xcomponent r) FLUSH-WIDTH) (< (robot-ycomponent r) ROBOT-RADIUS)) 
     (make-robot (robot-xcomponent r) (- (robot-ycomponent r) d) UP)]
    [else 
     (cond 
       [(>= (- (robot-ycomponent r) d) ROBOT-RADIUS) (make-robot (robot-xcomponent r) (- (robot-ycomponent r) d) UP)]
       [else (make-robot (robot-xcomponent r) ROBOT-RADIUS UP)])]))

; robot-movedown : Robot NonNegReal -> Robot
; Returns a Robot like r whose direction is "Down" and has moved 'downwards' by d pixels
; Checks for boundary condition too, i.e, if the robot is inside the room and 
; moving it 'down' by d pixels would put it outside the room, it stops at the base of the canvas/room
; STRATEGY: Data Decomposition

(begin-for-test
  (check-equal? (robot-movedown (make-robot 0 0 "Down") 10)
                (make-robot 0 10 "Down")
                "The 'robot-movedown' fuction failed to move the robot 10 pixels down")
  (check-equal? (robot-movedown (make-robot 200 400 "Down") 20)
                (make-robot 200 420 "Down")
                "The 'robot-movedown' fuction failed to move the robot 20 pixels down")
  (check-equal? (robot-movedown (make-robot 30 -30 "Down") 200)  
                (make-robot 30 170 "Down")
                "The 'robot-movedown' fuction failed to move the robot 200 pixels down")
  (check-equal? (robot-movedown (make-robot 20 20 "Down") 390)  ;To test the boundary condition along y-axis/Bottom of the canvas
                (make-robot 20 385 "Down")
                "The 'robot-movedown' fuction failed to move the robot 390 pixels down")
  (check-equal? (robot-movedown (make-robot 210 410 "Down") 25)
                (make-robot 210 435 "Down")
                "The 'robot-movedown' fuction failed to move the robot 25 pixels down"))

(define (robot-movedown r d)
  (cond
    [(or (< (robot-xcomponent r) ROBOT-RADIUS) (> (robot-xcomponent r) FLUSH-WIDTH) (> (robot-ycomponent r) FLUSH-HEIGHT)) 
     (make-robot (robot-xcomponent r) (+ (robot-ycomponent r) d) DOWN)]
    [else 
     (cond 
       [(<= (+ (robot-ycomponent r) d) FLUSH-HEIGHT) (make-robot (robot-xcomponent r) (+ (robot-ycomponent r) d) DOWN)]
       [else (make-robot (robot-xcomponent r) FLUSH-HEIGHT DOWN)])]))


; robot-moveleft : Robot NonNegReal -> Robot
; Returns a Robot like r whose direction is "Left" and has moved 'left' by d pixels
; Checks for boundary condition too, i.e, if the robot is inside the room and 
; moving it 'left' by d pixels would put it outside the room, it stops at the 'left' wall
; STRATEGY: Data Decomposition

(begin-for-test
  (check-equal? (robot-moveleft (make-robot 0 0 "Left") 10)
                (make-robot -10 0 "Left")
                "The 'robot-moveleft' fuction failed to move the robot 10 pixels left")
  (check-equal? (robot-moveleft (make-robot 200 400 "Left") 20)
                (make-robot 180 400 "Left")
                "The 'robot-moveleft' fuction failed to move the robot 20 pixels left")
  (check-equal? (robot-moveleft (make-robot 240 40 "Left") 200)  
                (make-robot 40 40 "Left")
                "The 'robot-moveleft' fuction failed to move the robot 200 pixels left")
  (check-equal? (robot-moveleft (make-robot 20 20 "Left") 20)  ;To test the boundary condition along x-axis/Left wall
                (make-robot 15 20 "Left")
                "The 'robot-moveleft' fuction failed to move the robot 390 pixels left")
  (check-equal? (robot-moveleft (make-robot -30 -50 "Left") 25)
                (make-robot -55 -50 "Left")
                "The 'robot-moveleft' fuction failed to move the robot 25 pixels left"))

(define (robot-moveleft r d)
  (cond
    [(or (< (robot-ycomponent r) ROBOT-RADIUS) (> (robot-ycomponent r) FLUSH-HEIGHT) (< (robot-xcomponent r) ROBOT-RADIUS)) 
     (make-robot (- (robot-xcomponent r) d) (robot-ycomponent r) LEFT)]
    [else 
     (cond 
       [(>= (- (robot-xcomponent r) d) ROBOT-RADIUS) (make-robot (- (robot-xcomponent r) d) (robot-ycomponent r) LEFT)]
       [else (make-robot ROBOT-RADIUS (robot-ycomponent r) LEFT)])]))

; robot-moveright : Robot NonNegReal -> Robot
; Returns a Robot like r whose direction is "Right" has moved 'right' by d pixels
; Checks for boundary condition too, i.e, if the robot is inside the room and 
; moving it 'right' by d pixels would put it outside the room, it stops at the 'right' wall
; STRATEGY: Data Decomposition

(begin-for-test
  (check-equal? (robot-moveright (make-robot 0 0 "Right") 10)
                (make-robot 10 0 "Right")
                "The 'robot-moveright' fuction failed to move the robot 10 pixels right")
  (check-equal? (robot-moveright (make-robot 200 400 "Right") 20)
                (make-robot 220 400 "Right")
                "The 'robot-moveright' fuction failed to move the robot 20 pixels right")
  (check-equal? (robot-moveright (make-robot -30 30 "Right") 200)  
                (make-robot 170 30 "Right")
                "The 'robot-moveright' fuction failed to move the robot 200 pixels right")
  (check-equal? (robot-moveright (make-robot 20 20 "Right") 190)  ;To test the boundary condition along x-axis/Right Wall
                (make-robot 185 20 "Right")
                "The 'robot-moveright' fuction failed to move the robot 190 pixels right")
  (check-equal? (robot-moveright (make-robot 210 410 "Right") 25)
                (make-robot 235 410 "Right")
                "The 'robot-moveright' fuction failed to move the robot 25 pixels right"))

(define (robot-moveright r d)
  (cond
    [(or (< (robot-ycomponent r) ROBOT-RADIUS) (> (robot-ycomponent r) FLUSH-HEIGHT) (> (robot-xcomponent r) FLUSH-WIDTH)) 
     (make-robot (+ (robot-xcomponent r) d) (robot-ycomponent r) RIGHT)]
    [else 
     (cond 
       [(<= (+ (robot-xcomponent r) d) FLUSH-WIDTH) (make-robot (+ (robot-xcomponent r) d) (robot-ycomponent r) RIGHT)]
       [else (make-robot FLUSH-WIDTH (robot-ycomponent r) RIGHT)])]))