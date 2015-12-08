;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname worms) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 48)

(provide INITIAL-WORLD)
(provide next-world)
(provide key-handler)
(provide end?)
(provide world-worm)
(provide create-worm)
(provide worm-length)
(provide worm-head-x)
(provide worm-head-y)
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
(define food-y 20)

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

; food is a Posn

; A World is a (make-World Worm Posn)
; INTERP: Current World here refers to the value of current Worm and 
; position of food on the canvas
(define-struct World [worm food])

; TEMPLATE:
; world-fn: World -> ???
; (define (world-fn w)
;  (... (World-worm w) ... 
;       (World-food w) ...)) 

; Initial world-posn list
(define init-worm-posnlist 
  (cons (make-posn WORM-X-LEFT-EDGE WORM-Y-TOP-EDGE) MPTY))

; The definition of initial-world
(define INITIAL-WORLD 
  (make-World 
   (make-Worm init-worm-posnlist DOWN)
   (make-posn food-x food-y)))

; run: World Number -> World
; Returns the current wrld. the function uses big bang and triggers 
; functions based on key events, clock ticks etc.
; STRATEGY: Function Composition
(define (run w tick-rate)
  (big-bang w
            (on-tick next-world tick-rate)
            (on-key key-handler)
            (on-draw render)
            (stop-when end? render-last)))

; next-world: World -> World
; Returns the next world with new updated values for Worm and food
; EXAMPLES:
(begin-for-test 
  (check-equal? (next-world INITIAL-WORLD)
                (make-World 
                 (make-Worm (cons (make-posn 5 15) MPTY) DOWN)
                 (make-posn food-x food-y))
                "Function failed"))
; STRATEGY: Data decomposition on w  : World
(define (next-world w)
  (make-World 
   (extract-new-worminfo (World-worm w)) 
   (extract-new-foodinfo (World-food w))))

; extract-new-worminfo: Worm -> Worm
; Gets the new values of position and direction of the worm
; EXAMPLES:
(begin-for-test 
  (check-equal? (extract-new-worminfo (make-Worm init-worm-posnlist RIGHT))
                (make-Worm (cons (make-posn 15 5) MPTY) RIGHT)
                "Function failed"))
; STRATEGY: Data Decomposition on wrm : Worm
(define (extract-new-worminfo wrm)
  (make-Worm (append (change-head-posn (Worm-psn wrm) (Worm-dir wrm)) 
                     (delete-last-segment (rest (Worm-psn wrm))))
             (Worm-dir wrm)))

; change-head-posn: Worm -> ListOfPosn
; Gets the new position of the head segment of the worm
; EXAMPLES:
(begin-for-test 
  (check-equal? (change-head-posn (cons (make-posn 10 10) MPTY) RIGHT)
                (cons (make-posn 20 10) MPTY)
                "Function failed")
  (check-equal? (change-head-posn (cons (make-posn 10 20) 
                          (cons (make-posn 10 10) MPTY)) DOWN)
                (cons (make-posn 10 20) MPTY)
                "Function failed"))
; STRATEGY: Data Decomposition on wrm : Worm
(define (change-head-posn posn-list d)
  (cond 
    [(empty? (rest posn-list)) 
     (cons (extract-newPosninfo (first posn-list) d) MPTY)]
    [else (change-head-posn (rest posn-list) d)]))

; delete-last-segment: ListOfPosn -> ListOfPosn
; Deletes the last segment from the worm
; EXAMPLES:
(begin-for-test 
  (check-equal? (delete-last-segment (cons (make-posn 10 10) MPTY))
                MPTY
                "Function failed")
  (check-equal? (delete-last-segment (cons (make-posn 10 10) 
                                           (cons (make-posn 0 20)  MPTY)))
                (cons (make-posn 10 10) MPTY)
                "Function failed"))
; STRATEGY: Data Decomposition on posn-list : ListOfPosn
(define (delete-last-segment posn-list)
  (cond
    [(or (empty? posn-list) (empty? (rest posn-list))) MPTY]
    [else 
     (append (cons (first posn-list) MPTY)
             (delete-last-segment (rest posn-list)))]))

; extract-newPosninfo: Posn Direction ->Posn
; Gets the new postion based on the direction 
; EXAMPLES:
(begin-for-test 
  (check-equal? (extract-newPosninfo (make-posn 10 10) "left")
                (make-posn 0 10)
                "Function failed")
  (check-equal? (extract-newPosninfo (make-posn 10 10) "right")
                (make-posn 20 10)
                "Function failed")
  (check-equal? (extract-newPosninfo (make-posn 10 10) "up")
                (make-posn 10 0)
                "Function failed")
  (check-equal? (extract-newPosninfo (make-posn 10 10) "down")
                (make-posn 10 20)
                "Function failed")
  (check-equal? (extract-newPosninfo (make-posn 10 10) "m")
                (make-posn 10 10)
                "Function failed"))
; STRATEGY: Data Decomposition on p : Posn
(define (extract-newPosninfo p d)
  (cond
    [(right? d) (make-posn (+ (posn-x p) WORM-DIAMETER) (posn-y p))]
    [(left? d) (make-posn (- (posn-x p) WORM-DIAMETER) (posn-y p))]
    [(up? d) (make-posn (posn-x p) (- (posn-y p) WORM-DIAMETER))]
    [(down? d) (make-posn (posn-x p) (+ (posn-y p) WORM-DIAMETER))]
    [else (make-posn (posn-x p) (posn-y p))]))

; extract-new-foodinfo: Posn -> Posn
; Gets the new location of food
; EXAMPLES:
(begin-for-test 
  (check-equal? (extract-new-foodinfo (make-posn 10 10))
                (make-posn 10 10)
                "Function failed"))
; STRATEGY: Data Decomposition on fd : Posn
(define (extract-new-foodinfo fd)
  (make-posn (posn-x fd) (posn-y fd)))

; key-handler: World KeyEvent -> World
; Returns updated World with its values on the World's worm direction 
; changed based on KeyEvent
; EXAMPLES:
(begin-for-test 
  (check-equal? (key-handler INITIAL-WORLD "a")
                INITIAL-WORLD
                "Function failed")
  (check-equal? (key-handler INITIAL-WORLD "left")
                (make-World (make-Worm init-worm-posnlist LEFT) 
                            (make-posn food-x food-y))
                "Function failed"))
; STRATEGY: Data Decomposition on w : World
(define (key-handler w ke)
  (if (one-of-directions? ke) 
      (make-World 
       (make-Worm (Worm-psn (World-worm w)) ke) 
       (make-posn food-x food-y))
      w))
; one-of-directions?: KeyEvent -> Boolean
; Checks weather the KeyEvent passed is one of the four allowed 
; directions in the itemization
; EXAMPLES:
(begin-for-test 
  (check-equal? (one-of-directions? "left")
                #true
                "Function failed")
  (check-equal? (one-of-directions? "right")
                #true
                "Function failed")
  (check-equal? (one-of-directions? "up")
                #true
                "Function failed")
  (check-equal? (one-of-directions? "down")
                #true
                "Function failed")
  (check-equal? (one-of-directions? "a")
                #false
                "Function failed"))
; STRATEGY: Function Composition
(define (one-of-directions? ke)
  (or (right? ke) (left? ke) (up? ke) (down? ke)))

; render: World -> Image
; The function is called on the on-draw componen of big bang in run. 
; Renders the current world's image on the canvas
; EXAMPLES:
(begin-for-test 
  (check-equal? (render INITIAL-WORLD)
                (place-image FOOD-IMG food-x food-y 
                             (posnlist-to-image init-worm-posnlist))
                "Function failed"))
; STRATEGY: Funcion Composition
(define (render w)
  (draw-world-on w MT))

; draw-world-on: World Image -> Image
; draws the current world image on empty scene defined
; EXAMPLES:
(begin-for-test 
  (check-equal? (draw-world-on INITIAL-WORLD MT)
                (place-image FOOD-IMG food-x food-y 
                             (posnlist-to-image init-worm-posnlist))
                "Function failed"))
; STRATEGY: Data Decomposition on w : World
(define (draw-world-on w img)
  (place-image FOOD-IMG food-x food-y 
               (posnlist-to-image (Worm-psn (World-worm w)))))

; end?: World -> Boolean
; Checks conditions for stopping the runnning of big-bang in run
; EXAMPLES:
(begin-for-test 
  (check-equal? (end? INITIAL-WORLD)
                #false
                "Function failed"))
; STRATEGY: Data Decomposition on w: World
(define (end? w)
  (failed-game-rules? (World-worm w)))

; failed-game-rules?: Worm -> Boolean
; Checks whether the worm has violated the game rules of hitting the borders 
; or colliding with itself
; EXAMPLES:
(begin-for-test 
  (check-equal? (failed-game-rules? 
                 (make-Worm (cons (make-posn 20 30) MPTY) DOWN)) 
                #false
                "Function failed")
  (check-equal? (failed-game-rules? 
                 (make-Worm MPTY DOWN)) 
                #true
                "Function failed"))                
; STRATEGY: Data Decomposition on wrm : Worm
(define (failed-game-rules? wrm)
  (cond
    [(empty? (Worm-psn wrm)) #true]
    [else (or (check-outside-boundary? (first (Worm-psn wrm)) (Worm-dir wrm)) 
              (worm-self-collided? (Worm-psn wrm)))]))

; check-outside-boundary?: Posn Direction -> Boolean
; Checks whether the worm is moved outside the canvas boundary limits 
; based on its direction
; EXAMPLES:
(begin-for-test 
  (check-equal? (check-outside-boundary? (make-posn 295 295) DOWN) 
                #true
                "Function failed")
  (check-equal? (check-outside-boundary? (make-posn 20 30) RIGHT) 
                #false
                "Function failed")
  (check-equal? (check-outside-boundary? (make-posn 5 5) UP) 
                #true
                "Function failed")
  (check-equal? (check-outside-boundary? (make-posn 40 30) LEFT) 
                #false
                "Function failed")
  (check-equal? (check-outside-boundary? (make-posn 20 30) "a") 
                #false
                "Function failed"))
; STRATEGY: Data Decomposition on p : Posn
(define (check-outside-boundary? p d)
  (cond
    [(right? d) (> (+ (posn-x p) WORM-DIAMETER) WIDTH)]
    [(left? d) (< (- (posn-x p) WORM-DIAMETER) 0)]
    [(up? d) (< (- (posn-y p) WORM-DIAMETER) 0)]
    [(down? d) (> (+ (posn-y p) WORM-DIAMETER) HEIGHT)]
    [else #false]))
; worm-self-collided?: ListOfPosn -> Boolean
; Checks whether the worm collided with itself
; EXAMPLES:
(begin-for-test 
  (check-equal? (worm-self-collided? (cons (make-posn 20 30) '())) 
                #false
                "Function failed")
   (check-equal? (worm-self-collided? MPTY) 
                #false
                "Function failed")
    (check-equal? (worm-self-collided? (cons (make-posn 20 30) 
                                             (cons (make-posn 20 30) '()))) 
                #true
                "Function failed"))
; STRATEGY: Data decomposition on posn-list : ListOfPosn
(define (worm-self-collided? posn-list)
  (cond
    [(empty? posn-list) #false]
    [else (member? (first posn-list) (rest posn-list))]))

; render-last: World -> Image
; Renders the last stage of the world after big bang stops
; EXAMPLES:
(begin-for-test 
  (check-equal? (render-last (make-World 
                              (make-Worm init-worm-posnlist UP)
                              (make-posn 35 45)))
                (place-image 
                 (text "Worm hit border" 10 "black")
                 (get-message-img-xposn (make-Worm init-worm-posnlist UP)) 
                 (get-message-img-yposn (make-Worm init-worm-posnlist UP))
                 (place-image FOOD-IMG food-x food-y 
                              (posnlist-to-image init-worm-posnlist)))
                "Function failed"))
; STRATEGY: Data Decomposition on w : World
(define (render-last w)
  (place-image (game-over-img (World-worm w)) 
               (get-message-img-xposn (World-worm w)) 
               (get-message-img-yposn (World-worm w)) 
               (place-image FOOD-IMG food-x food-y 
                            (posnlist-to-image (Worm-psn (World-worm w))))))

; posnlist-to-image: Worm -> Image
; Recursively gets the image of the worm segment by segment
; EXAMPLES:
(begin-for-test 
  (check-equal? (posnlist-to-image (cons (make-posn 20 30) '())) 
                (place-image WORM-IMG 20 30 MT)
                "Function failed")
  (check-equal? (posnlist-to-image '()) 
                MT
                "Function failed"))
; STRATEGY: Data Decomposition on wrm : Worm
(define (posnlist-to-image posn-list)
  (cond
    [(empty? posn-list) MT]
    [else (place-image WORM-IMG 
                       (get-worm-img-xposn (first posn-list))
                       (get-worm-img-yposn (first posn-list))
                       (posnlist-to-image (rest posn-list)))]))

; game-over-img: Worm -> Image
; Gets the message of world ending as an image to be renderd on the canvas
; EXAMPLES:
(begin-for-test 
  (check-equal? (game-over-img (make-Worm (cons (make-posn 295 295) '()) DOWN)) 
                (text "Worm hit border" 10 "black")
                "Function failed")
  (check-equal? (game-over-img (make-Worm 
                                (cons (make-posn 20 30) 
                                      (cons (make-posn 20 30) '())) RIGHT)) 
                (text "Worm hit itself" 10 "black")
                "Function failed"))
; STRATEGY: Data Decomposition on wrm : Worm
(define (game-over-img wrm)
  (if (check-outside-boundary? (first (Worm-psn wrm)) (Worm-dir wrm)) 
      (text "Worm hit border" 10 "black") 
      (text "Worm hit itself" 10 "black")))

; get-worm-img-xposn: Posn -> Coordinate
; Gets the x coordinate position of the Worm
; EXAMPLES:
(begin-for-test 
  (check-equal? (get-worm-img-xposn (make-posn 5 10))
                5
                "Function failed"))
; STATEGY: Data Decomposition on psn : Posn
(define (get-worm-img-xposn psn)
  (posn-x psn))

; get-worm-img-yposn: Posn -> Coordinate
; Gets the y coordinate position of the Worm
; EXAMPLES:
(begin-for-test 
  (check-equal? (get-worm-img-yposn (make-posn 5 10))
                10
                "Function failed"))
; STATEGY: Data Decomposition on psn : Posn
(define (get-worm-img-yposn psn)
  (posn-y psn))  


; get-message-img-xposn: Worm -> Coordinate
; Gets the x position of the message-image to be displayed on canvas
; EXAMPLES:
(begin-for-test 
  (check-equal? (get-message-img-xposn (make-Worm init-worm-posnlist DOWN))
                (/ (image-width 
                       (game-over-img (make-Worm init-worm-posnlist DOWN))) 2)
                "Function failed"))     
; STRATEGY: Function Composition
(define (get-message-img-xposn wrm) 
  (/ (image-width (game-over-img wrm)) 2))

; get-message-img-yposn: Worm -> Coordinate
; Gets the y position of the message-image to be displayed on canvas
; EXAMPLES:
(begin-for-test 
  (check-equal? (get-message-img-yposn (make-Worm init-worm-posnlist DOWN))
                (- 295 
                   (/ (image-height 
                       (game-over-img (make-Worm init-worm-posnlist DOWN))) 2))
                "Function failed"))     
; STRATEGY: Function Composition
(define (get-message-img-yposn wrm) 
  (- WORM-Y-DOWN-EDGE (/ (image-height (game-over-img wrm)) 2)))

; world-worm : World -> Worm
; Returns a representation of the Worm in the game.
; EXAMPLES:
(begin-for-test 
  (check-equal? (world-worm INITIAL-WORLD)
                (make-World
                 (make-Worm init-worm-posnlist DOWN)
                 (make-posn food-x food-y))
                "Function failed"))
; STRATEGY: Data decomposition on w : World
(define (world-worm w)
  (make-World
   (make-Worm (Worm-psn (World-worm w)) (Worm-dir (World-worm w))) 
   (make-posn (posn-x (World-food w)) (posn-x (World-food w)))))

; create-worm : ListOfPosn -> Worm
; Creates a worm from the given Posns, using the first Posn in the list
; as the worm's head, and the rest of the list, in that order, 
; as the worm's body.
; WHERE: the list of posns are contiguous and form a valid worm
; EXAMPLES: 
(begin-for-test 
  (check-equal? (create-worm init-worm-posnlist)
                (make-Worm init-worm-posnlist DOWN)
                "Function failed"))
; STRATEGY: Function Composition
(define (create-worm posn-list)
  (make-Worm (create-worm-posnlist posn-list) DOWN))

; create-worm-posnlist: ListOfPosn -> ListOfPosn
; creates a list of positions from th given list
; EXAMPLES:
(begin-for-test 
  (check-equal? (create-worm-posnlist init-worm-posnlist)
                init-worm-posnlist
                "Function failed"))
; STRATEGY: Data Decomposition on posn-list : ListOfPOsn
(define (create-worm-posnlist posn-list)
  (cond
    [(empty? posn-list) MPTY]
    [else (append (cons (first posn-list) MPTY) 
                  (create-worm-posnlist (rest posn-list)))]))

; worm-length : Worm -> PosInt
; Returns the number of segments in the given worm.
; EXAMPLES: 
(begin-for-test 
  (check-equal? (worm-length (make-Worm init-worm-posnlist DOWN))
                1
                "Function failed")
  (check-equal? (worm-length (make-Worm MPTY DOWN))
                0
                "Function failed"))
; STRATEGY: Data Decomposition on w : Worm
(define (worm-length w)
  (cond
    [(empty? (Worm-psn w)) 0]
    [else (length (Worm-psn w))]))

; worm-head-x : Worm -> Coordinate
; Returns the x position of the center of the worm's lead segment.
; EXAMPLES: 
(begin-for-test 
  (check-equal? (worm-head-x (make-Worm init-worm-posnlist DOWN))
                WORM-X-LEFT-EDGE
                "Function failed")
  (check-equal? (worm-head-x (make-Worm MPTY DOWN))
                WORM-X-LEFT-EDGE
                "Function failed"))
; STRATEGY: Data decomposition on wrm : Worm
(define (worm-head-x wrm)
  (cond 
    [(empty? (Worm-psn wrm)) WORM-X-LEFT-EDGE]
    [else (get-worm-img-xposn (first (Worm-psn wrm)))]))

; worm-head-y : Worm -> Coordinate
; Returns the y position of the center of the worm's lead segment.
; EXAMPLES: 
(begin-for-test 
  (check-equal? (worm-head-y (make-Worm init-worm-posnlist DOWN))
                WORM-Y-TOP-EDGE
                "Function failed")
  (check-equal? (worm-head-y (make-Worm MPTY DOWN))
                WORM-Y-TOP-EDGE
                "Function failed"))
; STRATEGY: Data decomposition on wrm : Worm 
(define (worm-head-y wrm)
  (cond 
    [(empty? (Worm-psn wrm)) WORM-Y-TOP-EDGE]
    [else (get-worm-img-yposn (first (Worm-psn wrm)))]))

;(run INITIAL-WORLD 0.5)

;.... Alternate Data Definitions............
; A) Preferring flat structures over nested structures.
;    World could be 
; (define-stuct world[Worm Direction foodx foody])


; Pros: 
; The functions will be far more simplified with lesser comparison operations 
; lesser nesting od data decomposition

; Cons:
; There will be a lot of extraneous non comprehendible code as a result of 
; this structure.

; B) USING 1 and 0 as values for DIRECTION

; A Direction can just be one of:
; - 1
; - 0
; INTERP: 1 represents right and down directions and 0 represents left and 
; top directions of the ball 
; (define LEFT 0)
; (define RIGHT 1)
; (define TOP 0)
; (define DOWN 1)

; PROS: Every time the ball hits the wall or boundary on the left 1 has to  
; be added to its direction component of the ball structure to change its  
; direction to represent right and similarly when it hits the right boundary, 
; 1 is subtracted from its direction component. The ease of calculations and not
; necessitating a separate direction function to change the directions could be 
; reasons to use this data definition.

; CONS: The directions of TOP and LEFT hold same values. 
; This could lead to errors in the code.