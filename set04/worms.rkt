;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname worms) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 20)

(provide INITIAL-WORLD)
(provide next-world)
(provide key-handler)
(provide end?)
;(provide world-worm)
;(provide create-worm)
;(provide worm-length)
;(provide worm-head-x)
;(provide worm-head-y)
;(provide replace-food)
;(provide replace-worm)
;(provide posns-overlap?)

; constants 
(define HEIGHT 300) ; the height of canvas
(define WIDTH 300) ; the width of canvas
(define CENTER-HEIGHT (/ HEIGHT 2)) ;pixels
(define CENTER-WIDTH (/ WIDTH 2)) ;pixels
;.... temp ....
(define food-x 20)
(define food-y 10)

; graphical constants 

; MT is an empty-scene 
(define MT (empty-scene WIDTH HEIGHT))

(define WORM-RADIUS 5) ; pixels
(define WORM-DIAMETER (* 2 WORM-RADIUS)) ; pixels
(define WORM-COLOR "red") ; color
(define FOOD-COLOR "green") ; color
(define FOOD-RADIUS 5) ; pixels
(define WORM-X-RIGHT-EDGE (- WIDTH WORM-RADIUS)) ; x Coordinate
(define WORM-X-LEFT-EDGE WORM-RADIUS) ; x Coordinate
(define WORM-Y-DOWN-EDGE (- HEIGHT WORM-RADIUS)) ; y Coordinate
(define WORM-Y-TOP-EDGE WORM-RADIUS) ; y Coordinate

; The worm-segment image
(define WORM-IMG (circle WORM-RADIUS "solid" WORM-COLOR))
; The food image
(define FOOD-IMG (circle FOOD-RADIUS "solid" FOOD-COLOR))
; The game-over image
(define GAME-OVER (text "Game-Over" 16 "black"))

; A Posn, already pre-defined is a (make-posn Coordinate Coordinate)
; INTERP: represents a position on a coordinate plane where in the coordinates 
; in the structure represent the x and y coordinates 

; An ListOfPosn is one of: 
; – empty 
; – (cons posn ListOfPosn)
; ListOfPosn is a list of posn/ x and y coordinate combinations.

; MPTY is an empty list
(define MPTY '())

; A Direction is one of:
; - "right"
; - "left"
; - "top"
; - "down"
; INTERP: represents a canvas direction where "right" is x-increasing and "left"
; is x-decreasing, "down" is y-increasing and "up" is y-decreasing.
(define RIGHT "right")
(define LEFT "left")
(define UP "up")
(define DOWN "down")
 
; <direction predicates> : Direction -> Boolean
; Returns true if d is the Direction indicated by the function name.
; EXAMPLES:
(begin-for-test
  (check-equal? (right? "right")
                #true)
  (check-equal? (left? RIGHT)
                #false)
  (check-equal? (up? UP)
                #true)
  (check-equal? (down? DOWN)
                #true))
; STRATEGY: function composition
(define (right? d) (string=? d RIGHT))
(define (left? d) (string=? d LEFT))
(define (up? d) (string=? d UP))
(define (down? d) (string=? d DOWN))

; A Worm is a (make-Worm ListOfPosn Direction)
(define-struct Worm [psn dir])

; TEMPLATE:
; worm-fn: Worm -> ???
; (define (worm-fn wrm)
;  (... (Worm-psn wrm) ... 
;       (Worm-dir wrm) ...))

; A World is a (make-World Worm Posn)
; INTERP: Current World here refers to the value of current Worm and 
; position of food on the canvas
(define-struct World [worm food])

; TEMPLATE:
; world-fn: World -> ???
; (define (world-fn w)
;  (... (World-worm w) ... 
;       (World-food w) ...)) 

; The definition of initial-world
(define INITIAL-WORLD 
  (make-World 
   (make-Worm 
    (cons (make-posn WORM-X-LEFT-EDGE WORM-Y-TOP-EDGE) MPTY) DOWN)
   (make-posn food-x food-y)))

; run: World Number -> World
; INTERP: returns the current wrld. the function uses big bang and triggers 
; functions based on key events, clock ticks etc.
; EXAMPLES: 

; STRATEGY: Function Composition
(define (run w tick-rate)
  (big-bang w
            (on-tick next-world tick-rate)
            (on-key key-handler)
            (on-draw render)
            (stop-when end? render-last)))

(define (next-world w)
  (make-World 
   (extract-new-worminfo (World-worm w)) 
   (extract-new-foodinfo (World-food w))))

(define (extract-new-worminfo wrm)
  (make-Worm 
   (cond 
     [(empty? (Worm-psn wrm)) MPTY]
     [else (cons 
            (extract-newPosninfo (first (Worm-psn wrm)) 
                                 (Worm-dir wrm)) MPTY)])
   (Worm-dir wrm)))

(define (extract-newPosninfo p d)
  (cond
    [(right? d) (make-posn (+ (posn-x p) WORM-DIAMETER) (posn-y p))]
    [(left? d) (make-posn (- (posn-x p) WORM-DIAMETER) (posn-y p))]
    [(up? d) (make-posn (posn-x p) (- (posn-y p) WORM-DIAMETER))]
    [(down? d) (make-posn (posn-x p) (+ (posn-y p) WORM-DIAMETER))]))

(define (extract-new-foodinfo fd)
  (make-posn (posn-x fd) (posn-y fd)))

(define (key-handler w ke)
  (make-World (make-Worm (first (Worm-psn (World-worm w))) ke) 
              (make-posn food-x food-y)))

(define (render w)
  (draw-world-on w MT))

(define (draw-world-on w img)
  (place-images (list WORM-IMG FOOD-IMG)
                (list (make-posn 
                       (posn-x (first (Worm-psn (World-worm w)))) 
                       (posn-y (first (Worm-psn (World-worm w)))))
                      (make-posn food-x food-y)) img))

(define (end? w)
  (check-outside-boundary? 
   (first (Worm-psn (World-worm w))) (Worm-dir (World-worm w))))

(define (check-outside-boundary? p d)
  (cond
    [(right? d) (> (+ (posn-x p) WORM-DIAMETER) WIDTH)]
    [(left? d) (< (- (posn-x p) WORM-DIAMETER) 0)]
    [(up? d) (< (- (posn-y p) WORM-DIAMETER) 0)]
    [(down? d) (> (+ (posn-y p) WORM-DIAMETER) HEIGHT)]))

(define (render-last w)
  (place-images 
   (list WORM-IMG FOOD-IMG GAME-OVER)
   (list (make-posn (posn-x (first (Worm-psn (World-worm w)))) 
                    (posn-y (first (Worm-psn (World-worm w)))))
                      (make-posn food-x food-y)
                      (make-posn CENTER-WIDTH CENTER-HEIGHT)) MT))

(run INITIAL-WORLD 1)

   
