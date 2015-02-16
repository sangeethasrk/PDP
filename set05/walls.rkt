;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname walls) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(define TIME-ON-TASK 45)

;(provide INITIAL-WORLD)
;(provide next-world)
;(provide key-handler)
;(provide mouse-handler)
;(provide end?)
;(provide get-balls)
;(provide mk-ball)
;(provide replace-balls)
;(provide get-ball-x)
;(provide get-ball-y)
;(provide score)
;(provide level)

; canvas constants
(define CENTER-X 400) ; pixels
(define CENTER-Y 400) ; pixels
(define WIDTH (* 2 CENTER-X)) ; pixels
(define HEIGHT (* 2 CENTER-Y)) ; pixels
(define EMPTY-SCENE (empty-scene WIDTH HEIGHT)) ; Image
 
; Ball constants
(define BALL-RADIUS 20) ; pixels
(define BALL-IMG (circle BALL-RADIUS "solid" "blue")) ; Image

(define VEL-X (random 10)) ; pixels/tick
(define VEL-Y (random 10)) ; pixels/tick

; A Direction is one of:
; - "right"
; - "left"
; - "top"
; - "down"
; INTERP: represents a canvas direction where "right" is x-increasing and "left"
; is x-decreasing, "down" is y-increasing and "top" is y-decreasing.
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

; A Velocity is a (make-velocity Number Number) 
(define-struct velocity (xvel yvel))

; TEMPLATE:
; velocity-fn : Velocity -> ???
; (define (velocity-fn v)
;  (... (velocity-xvel v) ... 
;       (velocity-yvel v)) ...))

; A Dir is a (make-dir Direction Direction)  
(define-struct dir (xdir ydir))

; TEMPLATE:
; dir-fn : Dir -> ???
; (define (dir-fn d)
;  (... (dir-xdir d) ... 
;       (dir-ydir d)) ...))

; A Ball is a (make-ball Coordinate Coordinate Velocity Dir)
(define-struct ball (xposn yposn vel direc))
 
; TEMPLATE:
; ball-fn : Ball -> ???
; (define (ball-fn b)
;  (... (ball-xposn b) ... 
;       (ball-xposn b) ...
;       (ball-vel b)) ...
;       (ball-direc b)) ...))

; A [List-of Ball] is one of: 
; – '() 
; – (cons Ball [List-of Ball])

; MPTY is an empty list
(define MPTY '())

; Wall-Orientation is one of:
; - horizontal
; - verticle
; INTERP: The Wall can have one of the following orientations
(define HORIZONTAL "horizontal")
(define VERTICLE "verticle")

; <wall-orientation predicates> : Wall-Orientation -> Boolean
; Returns true if o is the Wall-Orientation indicated by the function name.
; EXAMPLES:
(begin-for-test
  (check-equal? (horizontal? "horizontal")
                #true)
  (check-equal? (verticle? HORIZONTAL)
                #false))
; STRATEGY: function composition
(define (horizontal? o) (string=? o HORIZONTAL))
(define (verticle? o) (string=? o VERTICLE))

; Wall-State is one of:
; - active
; - inactive
; INTERP: The Wall assumes one of the following states based on 
; completion of building of the wall
(define ACTIVE "active")
(define INACTIVE "inactive")

; <wall-state predicates> : Wall-State -> Boolean
; Returns true if s is the Wall-State indicated by the function name.
; EXAMPLES:
(begin-for-test
  (check-equal? (active? "active")
                #true)
  (check-equal? (inactive? ACTIVE)
                #false))
; STRATEGY: function composition
(define (active? s) (string=? s ACTIVE))
(define (inactive? s) (string=? s INACTIVE))

; A Wall is a 
; (make-wall Coordinate Coordinate Coordinate Wall-Orientation Wall-State)
; INTERP: The first two co-ordinates represent start and end point along the 
; major axis and the third co-ordinate represents the value of the minor axies
(define-struct wall (startpt endpt minor type state))

; TEMPLATE:
; wall-fn : Wall -> ???
; (define (wall-fn w)
;  (... (wall-startpt w) ... 
;       (wall-endpt w) ...
;       (wall-minor w)) ...
;       (wall-type w)) ...
;       (wall-state w))... ))

; A [List-of Wall] is one of: 
; – '() 
; – (cons Wall [List-of Wall])

; A World is a (make-world 
;                 [List-of Ball] 
;                 [List-of Wall] 
;                 Wall-Orientation 
;                 Number 
;                 Number 
;                 Number)
(define-struct world (balls walls orientation level score goalscore))

; TEMPLATE:
; world-fn : World -> ???
; (define (world-fn w)
;  (... (world-balls w) ... 
;       (world-walls w) ...
;       (world-orientation w)) ...
;       (world-level w)) ... 
;       (world-score w)) ...
;       (world-goalscore w)) ... ))

(define INIT-VELOCITY (make-velocity VEL-X VEL-Y))
(define INIT-DIR (make-dir RIGHT DOWN))
(define INIT-BALL (make-ball CENTER-X BALL-RADIUS INIT-VELOCITY INIT-DIR))
(define BALL-X-RIGHT-EDGE (- WIDTH BALL-RADIUS)) ; x Coordinate
(define BALL-X-LEFT-EDGE BALL-RADIUS) ; x Coordinate
(define BALL-Y-DOWN-EDGE (- HEIGHT BALL-RADIUS)) ; y Coordinate
(define BALL-Y-TOP-EDGE BALL-RADIUS) ; y Coordinate
(define INITIAL-WORLD (make-world INIT-BALL '() VERTICLE 1 0 55))

; run : World -> World
; Starts the simulation.
; STRATEGY: function composition
(define (run w)
  (big-bang w
            (on-tick next-world)
            (on-key key-handler)
            (on-mouse mouse-handler)
            (on-draw render)
            (stop-when end? render-last)))

(define (next-world w)
  (make-world 
   (update-ball-list (world-balls w) (world-walls w) (world-orientation w))
   (update-wall-list (world-walls w))
   (world-orientation w)
   (world-level w)
   (world-score w)
   (get-goalscore (world-level w) (world-goalscore w))))

(define (update-ball-list balls walls type)
  (cond 
    [(empty? (rest balls)) (cons (update-ball (first balls) walls type) '())]
    [else (append (cons (update-ball (first balls) walls type) '()) 
                  (update-ball-list (rest balls) walls type))]))

(define (update-ball b w t)
  (cond
    [(ball-inside-canvas? b w) (next-ball-inside-canvas b t)]
    [else (next-ball-moverestricted b w)]))

(define (ball-inside-canvas? b w)
  (and (within-xlimits? (ball-xposn b) (ball-direc b) (ball-vel b)) 
       (within-ylimits? (ball-yposn b) (ball-direc b) (ball-vel b)) 
       (within-wall-limits? b w)))

; within-xlimits? : Coordinate Direction -> Boolean
; Checks whether the ball is within the x axis boundaries of the canvas
; EXAMPLES:
(begin-for-test
  (check-equal? (within-xlimits? 30 LEFT 3)
                #true)
  (check-equal? (within-xlimits? 385 RIGHT 9)
                #false))
; STRATEGY: Function Composition
(define (within-xlimits? x d v)
  (and 
     (<= (next-posn x (dir-xdir d) (velocity-xvel v)) BALL-X-RIGHT-EDGE)
     (>= (next-posn x (dir-xdir d) (velocity-xvel v)) BALL-X-LEFT-EDGE)))

; within-ylimits? : Coordinate Direction Number -> Boolean
; Checks whether the ball is within the y axis boundaries of the canvas
; EXAMPLES:
(begin-for-test
  (check-equal? (within-ylimits? 30 TOP 3)
                #true)
  (check-equal? (within-ylimits? 380 DOWN 9)
                #false))
; STRATEGY: Function Composition
(define (within-ylimits? y d v)
  (and
     (>= (next-posn y (dir-ydir d) (velocity-yvel v)) BALL-Y-TOP-EDGE)
     (<= (next-posn y (dir-ydir d) (velocity-yvel v)) BALL-Y-DOWN-EDGE)))

(define (next-posn cood d v)
  (cond 
    [(or (right? d) (down? d)) (+ cood v)]
    [(or (left? d) (top? d)) (- cood v)]))

(define (within-wall-limits? b w)
  (cond 
    [(empty? w) #true]
    [else (and (check-wall? b (first w)) (within-wall-limits? (rest w)))]))

(define (check-wall? b w)
  (cond 
    [(inactive? (wall-state w)) 
     (check-ball-notagainstwall? b (wall-type w) (wall-minor w))]
    [else #true]))

(define (check-ball-notagainstwall? b type minor-cood)
  (cond
    [(verticle? type) (or 
                       (and (< (ball-xposn b) minor-cood) 
                            (<= 
                             (calc-ball-next 
                              (ball-xposn b) (ball-direc b) (ball-vel b) type) 
                             minor-cood))
                       (and (> (ball-xposn b) minor-cood) 
                            (>= 
                             (calc-ball-next 
                              (ball-xposn b) (ball-direc b) (ball-vel b) type) 
                             minor-cood)))]
    [(horizantal? type) (or 
                         (and (< (ball-yposn b) minor-cood) 
                              (<= 
                               (calc-ball-next 
                                (ball-yposn b) (ball-direc b) (ball-vel b) type)
                               minor-cood))
                         (and (> (ball-yposn b) minor-cood) 
                              (>= 
                               (calc-ball-next 
                                (ball-yposn b) (ball-direc b) (ball-vel b) type)
                               minor-cood)))]))

(define (calc-ball-next a d v t)
  (cond
    [(verticle? t) (next-posn a (dir-xdir d) (velocity-xvel v))]
    [(horizontal? t) (next-posn a (dir-ydir d) (velocity-yvel v))]))

(define (next-ball-inside-canvas b)
  (make-ball
   (cal-ball-next (ball-xposn b) (ball-direc b) (ball-vel b) type)
   (cal-ball-next (ball-yposn b) (ball-direc b) (ball-vel b) type)
   (ball-vel b)
   (ball-direc b)))

(define (next-ball-moverestricted b w)
  (make-ball 
   (update-posn (get-xrestricted 
                  (ball-xposn b) (ball-direc b) (ball-vel b)) 
                 (ball-xposn b) w)
   (update-posn (get-yrestricted 
                  (ball-yposn b) (ball-direc b) (ball-vel b)) 
                 (ball-yposn b) w)
   (ball-vel b)
   (update-direction b w)))

(define (get-xrestricted a d v)
    (flush-against-canvas a d (velocity-xvel v)))

(define (get-yrestricted a d v)
  (flush-against-canvas a d (velocity-yvel v)))

(define (flush-against-canvas a d v)
  (cond 
    [(and (right? (dir-xdir d)) 
          (< (next-posn a (dir-xdir d) v) BALL-X-RIGHT-EDGE)) 
     BALL-X-RIGHT-EDGE]
    [(and (left? (dir-xdir d)) 
          (< (next-posn a (dir-xdir d) v) BALL-X-LEFT-EDGE)) 
     BALL-X-LEFT-EDGE]
    [(and (top? (dir-xdir d)) 
          (< (next-posn a (dir-ydir d) v) BALL-Y-TOP-EDGE)) 
     BALL-Y-TOP-EDGE]
    [(and (down? (dir-xdir d)) 
          (< (next-posn a (dir-ydir d) v) BALL-Y-DOWN-EDGE)) 
     BALL-Y-DOWN-EDGE]
    [else a]))

(define (update-posn a b w)
  (cond 
    [(empty? w) a]
    [else (flush-against-walls a b w)]))

(define (flush-against-walls a b w)
  (cond
    [(empty? (rest w)) (flush-against-wall a b (first w))]
    [else (if (check-wall-flushed? a b (first w)) 
              (flush-against-walls a b (rest w))
              (flush-against-wall a b (first w)))]))

(define (check-wall-flushed? a b w)
  (or (and (< b (wall-minor w)) 
           (>= a (wall-minor w)))
      (and (> b (wall-minor w)) 
           (<= a (wall-minor w)))))

(define (flush-against-wall  a b w)
  (if (check-wall-flushed? a b w) (wall-minor w) a))

(define (update-direction b w)
  (make-dir 
   (update-ball-xdirec b w)
   (update-ball-ydirec b w)))

(define (update-ball-xdirec b w)
  (cond
    [(empty? w) (get-xchanged b)]
    [else (if (not (within-wall-limits? b w)) 
              (update-xdir b (first w)) 
              (update-ball-xdirec b (rest w)))]))

(define (get-xchanged b)
  (getchanged-xdir (ball-xposn b) (direction-xdir (ball-direc d)) (ball-vel b)))

(define (update-xdir b w)
  (cond
    [(verticle? (wall-type w)) (change-xdir b (wall-minor w))]
    [(horizontal? (wall-type w)) 
     (cond
       [(and (>= (next-posn 
                  (ball-xposn b) 
                  (direction-xdir (ball-direc b)) 
                  (velocity-xvel (ball-vel b))) (wall-minor w))
             (< (ball-xposn b) (wall-minor w))
             (down? (direction-xdir (ball-direc b)))) TOP DOWN])]))

(define (change-xdir b minor)
  (cond
    [(right? (direction-xdir (ball-direc b))) 
     (if (and (< (ball-xposn b) minor) 
              (>= (next-posn 
                   (ball-xposn b) 
                   (direction-xdir (ball-direc b)) 
                   (velocity-xvel (ball-vel b))) minor)) LEFT RIGHT)]
    [(left? (direction-xdir (ball-direc b))) 
     (if (and (> (ball-xposn b) minor) 
              (<= (next-posn 
                   (ball-xposn b) 
                   (direction-xdir (ball-direc b)) 
                   (velocity-xvel (ball-vel b))) minor)) RIGHT LEFT)]
    [else (get-xchanged b)]))

(define (update-ball-ydirec b w)
  (cond
    [(empty? w) (get-ychanged b)]
    [else (if (not (within-wall-limits? b w)) 
              (update-ydir b (first w)) 
              (update-ball-ydirec b (rest w)))]))

(define (get-ychanged b)
  (getchanged-ydir (ball-yposn b) (direction-ydir (ball-direc d)) (ball-vel b)))

(define (update-ydir b w)
  (cond
    [(verticle? (wall-type w)) 
     (cond
       [(and (>= (next-posn 
                  (ball-yposn b) 
                  (direction-ydir (ball-direc b)) 
                  (velocity-yvel (ball-vel b))) (wall-minor w))
             (< (ball-yposn b) (wall-minor w))
             (right? (direction-ydir (ball-direc b)))) LEFT RIGHT]
       [(and (>= (next-posn 
                  (ball-yposn b) 
                  (direction-ydir (ball-direc b)) 
                  (velocity-yvel (ball-vel b))) (wall-minor w))
             (< (ball-yposn b) (wall-minor w))
             (left? (direction-ydir (ball-direc b)))) RIGHT LEFT])]
    [(horizontal? (wall-type w)) (change-ydir b (wall-minor w))]))

(define (change-ydir b minor)
  (cond
    [(top? (direction-ydir (ball-direc b))) 
     (if (and (< (ball-yposn b) minor) 
              (>= (next-posn 
                   (ball-yposn b) 
                   (direction-ydir (ball-direc b)) 
                   (velocity-yvel (ball-vel b))) minor)) LEFT RIGHT)]
    [(left? (direction-ydir (ball-direc b))) 
     (if (and (> (ball-yposn b) minor) 
              (<= (next-posn 
                   (ball-yposn b) 
                   (direction-ydir (ball-direc b)) 
                   (velocity-yvel (ball-vel b))) minor)) RIGHT LEFT)]
    [else (get-ychanged b)]))

(define (getchanged-xdir x d v)
  (cond
    [(<= (next-posn x d (velocity-xvel v) BALL-X-LEFT-EDGE)) RIGHT]
    [(<= (next-posn x d (velocity-xvel v) BALL-X-RIGHT-EDGE)) LEFT]
    [else d]))

(define (getchanged-ydir y d v)
  (cond
    [(<= (next-posn y d (velocity-yvel v) BALL-Y-TOP-EDGE)) DOWN]
    [(<= (next-posn y d (velocity-yvel v) BALL-Y-DOWN-EDGE)) TOP]
    [else d]))

(define (update-wall-list w)
  (cond
    [(empty? w) '()]
    [else (append (wall-afternexttick (first w)) (update-wall-list (rest w)))]))

(define (wall-afternexttick w)
  (cond
    [(active? (wall-state w)) (wall-grow w)]
    [(inactive? (wall-state w)) '()]))

(define (key-handler w ke)
  (if (key=? " ") 
      (make-world
       (world-balls w)
       (world-walls w)
       (change-orientation (world-orientation w))
       (world-level w)
       (world score w)
       (world-goalscore w)) w))

(define (change-orientation o)
  (cond
    [(verticle? o) HORIZONTAL]
    [(horizontal? o) VERTICLE]))

(define (render w)
  (place-image (draw-score-image w) 
               (- WIDTH (image-width (draw-score-image w))) 
               (image-height (draw-score-image w)) 
               (draw-balls-and-walls w)))

(define (draw-score-image w)
  (above 
   (text (get-score-text (world-score w)) 10 "black") 
   (text (get-score-text (world-goalscore w)) 10 "black")))

(define (get-score-text score)
  (string-append (number->text score) "%"))

(define (draw-balls-and-walls w)
  (ball-list-to-image (world-balls w) (wall-list-to-image (world-walls w))))

(define (ball-list-to-image b img)
  (cond 
    [(empty? (rest b)) (render-ball-img (first w) img)]
    [else (render-ball-img (first w) (ball-list-to-image (rest w) img))]))

(define (render-list-to-image b img)
  (place-image BALL-IMG (ball-xposn b) (ball-yposn b) img))

(define (wall-list-to-image w)
  (cond 
    [(empty? w) MT]
    [else (render-wall-image (first w) (wall-list-to-image (rest w)))]))

(define (render-wall-image w img)
  (cond 
    [(verticle? (wall-type w)) 
     (place-image 
      (rectangle 
       2 (get-wall-heightwidth (wall-startpt w) (wall-endpt w)) "solid" "black")
      (wall-minor w)
      (/ (get-wall-heightwidth (wall-startpt w) (wall-endpt w)) 2) img)]
    [(horizontal? (wall-type w))
     (place-image 
      (rectangle 
       (get-wall-heightwidth (wall-startpt w) (wall-endpt w)) 2 "solid" "black")
      (/ (get-wall-heightwidth (wall-startpt w) (wall-endpt w)) 2) 
      (wall-minor w) img)]))

(define (get-wall-heightwidth stpt edpt)
  (if (< (- stpt edpt) 0) 
      (* (- stpt edpt) (- 1 2))
      (- stpt edpt)))

(define (end? w)
  (collide-with-activewall? (world-balls w) (world-walls w)))

(define (collide-with-activewalls? b w)
  (cond 
    [(empty? w) #false]
    [else (if (active? (wall-state (first w))) 
              (ball-collides-with-activewall? b (first w)) #false)]))

(define (ball-collides-with-activewall b w)
  (cond
    [(empty? (rest b)) (check-ballcollision? (first b) w)]
    [else (and (check-ballcollision? (first b) w) 
               (collide-with-activewall? (rest b)))]))

(define (check-ballcollision? b w)
  (cond
    [(verticle? (wall-type w)) (or (and (> (ball-xposn b) (wall-minor w))
                                    (<= (get-xchanged b) (wall-minor w)))
                                   (and (< (ball-xposn b) (wall-minor w))
                                    (>= (get-xchanged b) (wall-minor w))))]
    [(horizantal? (wall-type w)) (or (and (> (ball-yposn b) (wall-minor w))
                                    (<= (get-ychanged b) (wall-minor w)))
                                   (and (< (ball-yposn b) (wall-minor w))
                                    (>= (get-ychanged b) (wall-minor w))))]
    [else #false]))

(define (render-last w)
  (place-image (text "GAME-OVER" 16 "black") 
               CENTER-X CENTER-Y (render w)))