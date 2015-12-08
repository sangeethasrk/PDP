#lang racket
(require "extras.rkt")
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(define TIME-ON-TASK 15)

(provide Unit<%>)
(provide StatefulWorld<%>)
(provide mk-world)
(provide mk-ally)
(provide mk-enemy)
(provide mk-merc)

;===============================================================================
; Constants
(define ALLY-SIZE 20) ; Pixels
(define ALLY (square ALLY-SIZE "solid" "green")) ; Image
(define ENEMY-SIZE 12) ; Pixels
(define ENEMY (circle ENEMY-SIZE "solid" "red")) ; Image

(define CANVAS-WIDTH 400) ; Pixels
(define CANVAS-HEIGHT 500) ; Pixels
(define MT (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)) ; Image
(define BASE-HEIGHT 50) ; Pixels
(define BASE-WIDTH 400) ; Pixels
(define BASE-COLOR "yellow") ; Pixels
(define BASE-TYPE "solid") ; Pixels
(define CROSS-BOW 
  (add-line (add-line (overlay (circle 5 "outline" "black")
                               (circle 10 "outline" "black"))
                      0 10 20 10 "black") 10 0 10 20 "black")) ; Image

; A Velocity is a Natural, representing Pixels/tick in the downward direction.

;===============================================================================
; Represents a mutable unit in the game.
(define Unit<%>
  (interface ()
    ; get-loc : -> posn
    ; Returns the location of this unit as a posn.
    get-loc
    
    ; get-color : -> Color
    ; Returns the color of this unit.
    get-color
    
    ; handle-tick! : -> Void
    ; EFFECT: mutates this unit to its next state
    handle-tick!
    
    ; within-my-region? : Coordinate Coordinate -> Boolean
    ; Returns true if given coordinates are within the unit's area
    within-my-region?
    
    ; update-score-on-elimination : -> Void
    ; Updates score on elimination
    update-score-on-elimination
    
    ; reached-base? : Pixel -> Boolean
    ; Returns true if unit has reached or gone beyond given base height
    reached-base?
    
    ; update-score-on-reachingBase : -> Void
    ; Updates score on reaching the base
    update-score-on-reachingBase
    
    ; draw : Image -> Image
    ; Renders unit on the given image
    draw))

;===============================================================================
; Represents a mutable world object.
(define StatefulWorld<%>
  (interface ()
    ; on-tick! : -> Void
    ; EFFECT: mutates this world to its next state.
    on-tick!
    
    ; on-mouse! : Coordinate Coordinate MouseEvent -> Void
    ; EFFECT: mutates this world to its next state from the given mouse 
    ; parameters.
    on-mouse!
    
    ; target-loc : -> posn
    ; Returns the center of the target as a posn.
    target-loc
    
    ; get-units : -> ListOf<Unit<%>>
    ; Returns the current units in this world.
    get-units
    
    ; add-unit! : Unit<%> -> Void
    ; EFFECT: adds the given unit to the world
    add-unit!
    
    ; get-base-height : -> Natural
    ; Returns the height of the base, in pixels.
    get-base-height
    
    ; render : -> Image
    ; Renders the current state of world
    render
    
    ; end? : -> Boolean
    ; Returns true if the world should end, else false
    end?
    
    ; render-last : -> Image
    ; Renders last image of the world
    render-last))

;===============================================================================
(define Publisher<%>
  (interface ()
    ; subscribe! : Subscriber<%> -> Void
    ; EFFECT: Adds a new subscriber.
    subscribe!))

;===============================================================================
(define Subscriber<%>
  (interface ()
    ; update-score! : Natural -> Void
    ; EFFECT: updates this score by adding newScore to it
    update-score!))

;===============================================================================
; Represents a mutable score object.
(define Score<%>
  (interface ()
    ; get-score : -> Real
    ; Returns the score value
    get-score))

(define Score%
  (class* object% (Score<%> Subscriber<%>)
    (init-field subscribers) ; ListOf<Unit<%>>, list of units who are publishers
    (field [score 0]) ; Natural, holds score of the world
    
    ; initialize by subscribing to all units
    (for-each (λ (s) (send s subscribe! this)) subscribers)
    
    ; update-score! : Real -> Void     
    ; EFFECT: updates this score by adding newScore to it. 
    (define/public (update-score! newScore)
      (set! score (+ score newScore)))
    
    ; get-score : -> Real
    (define/public (get-score) score)
    
    (super-new)))

;===============================================================================
; A World is a 
; (new World% [units ListOf<Unit<%>>] [minVel Velocity] [maxVel Velocity])

(define World%
  (class* object% (StatefulWorld<%>)
    (init-field units ; ListOf<Unit<%>>,holds the list of units present in world
                minVel ; Velocity, represents min velocity of any unit in world
                maxVel) ;Velocity, represents max velocity of any unit in world
    
    (field [score (new Score% [subscribers units])] ; Score%, holds world score
           [NEW-UNIT-FREQ 3] ; Natural, frequency at which new unit is generated
           [new-unit-countdown NEW-UNIT-FREQ] ; Natural, counter for new unit
           [position (make-posn 0 0)]);Posn, holds current position of cross bow
    
    (define NUM-UNITS 3) ; Natural, number of different units world can hold
    (define ALLY-UNIT 0) ; Natural, represents ally unit
    (define ENEMY-UNIT 1) ; Natural, represents enemy unit
    (define MERC-UNIT 2) ; Natural, represents mercenary unit
    (define MIN-HEIGHT 10) ; Natural, min height below which game ends
    (define MAX-HEIGHT 500) ; Natural, max height beyond which game ends
    (define TXT-COLOR "black") ; String, represents text color
    (define RENDER-LAST-MSG "Game Over:") ; String, represents last message
    (define UNIT-INIT-Y 0) ; Coordinate, initial value of y for new unit
    
    
    ; on-tick! : -> Void
    ; EFFECT: mutates this world to its next state.
    ; Updates units in world by removing units which reached base and updates 
    ; score accordingly. Also generates new unit based on NEW-UNIT-FREQ
    (define/public (on-tick!)
      (map (λ (x) (send x handle-tick!)) units)
      (let* ([units-at-base (filter (λ(u) (send u reached-base? 
                                              (get-base-height))) units)])
        (set! units (filter (λ(u) (not (send u reached-base? 
                                             (get-base-height)))) units))
        (map (λ (x) (send x update-score-on-reachingBase)) units-at-base))
      
      
      (if (zero? new-unit-countdown)
          (let* ([new-unit (generate-random-unit (random NUM-UNITS))])
            (send new-unit subscribe! score)
            (set! new-unit-countdown NEW-UNIT-FREQ)
            (set! units (cons new-unit units)))
          (set! new-unit-countdown (sub1 new-unit-countdown))))
    
    
    ; generate-random-unit : Natural -> Unit<%>
    ; Generates a new unit in world based on val
    ; Where  0 <= val < 3
    (define (generate-random-unit val)
      (if (equal? val ALLY-UNIT) 
          (mk-ally (make-posn (+ (/ ALLY-SIZE 2) 
                                 (* ALLY-SIZE (random (quotient CANVAS-WIDTH 
                                                                ALLY-SIZE))))
                              UNIT-INIT-Y) (randomVel))
          (generate-Enemy/MiscUnit val)))
    
    
    ; generate-Enemy/MiscUnit : Natural -> Unit<%>
    ; Generates enemy or mercenary unit in world based on val
    ; Where  1 <= val < 3
    (define (generate-Enemy/MiscUnit val)
      (if (equal? val ENEMY-UNIT)
          (mk-enemy (make-posn (+ ENEMY-SIZE 
                                  (* (* ENEMY-SIZE 2) 
                                     (random (quotient CANVAS-WIDTH 
                                                       (* ENEMY-SIZE 2))))) 
                               UNIT-INIT-Y) (randomVel))
          (mk-merc (make-posn (+ (/ ALLY-SIZE 2) 
                                 (* ALLY-SIZE 
                                    (random (quotient CANVAS-WIDTH 
                                                      ALLY-SIZE)))) 
                              UNIT-INIT-Y) (randomVel)))) 
    
    ; randomVel : -> Velocity
    ; Returns random velocity in the range of minVel and maxVel
    (define (randomVel)
      (+ (random (+ (- maxVel minVel) 1)) minVel))
    
    ; on-mouse! : Coordinate Coordinate MouseEvent -> Void
    ; EFFECT: mutates this world to its next state from the given mouse 
    ; parameters.
    (define/public (on-mouse! x y me)
      (cond
        [(string=? "button-down" me) (handle-BD x y)]
        [(string=? "move" me) (set! position (make-posn x y))]
        [else (set! new-unit-countdown new-unit-countdown)]))
    
    ; handle-BD : Coordinate Coordinate -> Void
    ; EFFECT: mutates this world by handling button-down event
    (define (handle-BD x y)      
      (let* ([target-units (filter (λ (u) (send u within-my-region? x y)) 
                                  units)])
        (map (λ (x) (send x update-score-on-elimination)) target-units)
        (set! units (filter (λ (u) (not (send u within-my-region? x y)))
                            units))
        (set! position (make-posn x y))))
    
    ; target-loc : -> posn
    (define/public (target-loc) position)
    
    ; get-units : -> ListOf<Unit<%>>
    (define/public (get-units) units)
    
    ; add-unit! : Unit<%> -> Void
    ; EFFECT: adds the given unit to the world
    (define/public (add-unit! unit)
      (set! units (cons unit units))
      (for-each (λ (s) (send s subscribe! score)) units))
    
    ; get-base-height : -> Natural
    (define/public (get-base-height)
      (+ BASE-HEIGHT (/ (send score get-score) 5)))
    
    ; render : -> Image
    (define/public (render)
      (place-image CROSS-BOW (posn-x position) (posn-y position)
                   (render-base (render-units units MT))))
    
    ; render-units : ListOf<Unit<%>> Image -> Image
    ; Renders each of these units on the given image
    ; STRATEGY : data decomposition on lou : ListOf<Unit<%>>
    (define (render-units lou image)
      (cond
        [(empty? lou) image]
        [else (render-units (rest lou) (send (first lou) draw image))]))
    
    ; render-base : Image -> Image
    ; Renders base with score on the given image   
    (define (render-base image)
      (let* ([font (if (> (get-base-height) BASE-HEIGHT) BASE-HEIGHT
                       (get-base-height))])
        (overlay/align
         "center" "bottom"
         (overlay (text (number->string (send score get-score)) font TXT-COLOR)
                  (rectangle BASE-WIDTH (get-base-height) BASE-TYPE BASE-COLOR))
         image)))
    
    ; end? : -> Boolean
    (define/public (end?)
      (not (<= MIN-HEIGHT (get-base-height) MAX-HEIGHT)))
    
    ; render-last : -> Image
    (define/public (render-last)
      (overlay (text (string-append RENDER-LAST-MSG  
                                    (number->string (send score get-score)))
                     (/ BASE-HEIGHT 2) TXT-COLOR) MT))
    
    (super-new)))

;===============================================================================
; An Ally is a (new Ally% [position Posn] [vel Velocity])

(define Ally%
  (class* object% (Unit<%> Publisher<%>)
    (init-field position ; Posn, holds the center position of this ally unit
                vel ) ; Velocity, represents downward veleocity of this unit 
    
    (define ALLY-COLOR "green") ; Color, color of ally unit
    (define CONTROL-REG (/ ALLY-SIZE 2)) ; Natural, control region for ally
    (define ELIMINATION-SCORE -20) ; Real, score on getting eliminated
    (define REACH-BASE-SCORE +20) ;Real, score on reaching base
    (field [score #false]); MaybeSubscriber<%>,Score object which will be 
    ; notified of new score
    
    ; get-loc : -> posn
    (define/public (get-loc)
      position)
    
    ; get-color : -> Color
    (define/public (get-color)
      ALLY-COLOR)
    
    ; handle-tick! : -> Void
    ; EFFECT: mutates this unit to its next state
    (define/public (handle-tick!)
      (set! position (make-posn (posn-x position)
                                (+ (posn-y position) vel)))) 
    
    ; within-my-region? : Coordinate Coordinate -> Boolean
    (define/public (within-my-region? x y)
      (and (<= (- (posn-x position) CONTROL-REG) x 
               (+ (posn-x position) CONTROL-REG))
           (<= (- (posn-y position) CONTROL-REG) y 
               (+ (posn-y position) CONTROL-REG))))
    
    ; update-score-on-elimination : -> Void
    (define/public (update-score-on-elimination)
      (send score update-score! ELIMINATION-SCORE))
    
    ; reached-base? : Pixel -> Boolean
    (define/public (reached-base? base-height)
      (>= (+ (posn-y position) CONTROL-REG) (- CANVAS-HEIGHT base-height)))
    
    ; update-score-on-reachingBase : -> Void
    (define/public (update-score-on-reachingBase)
      (send score update-score! REACH-BASE-SCORE))
    
    ; draw : Image -> Image
    (define/public (draw image)
      (place-image ALLY (posn-x position) (posn-y position) image))
    
    ; subscribe! : Subscriber<%> -> Void
    ; EFFECT: Adds a new subscriber for score
    (define/public (subscribe! new-sub)
      (set! score new-sub))
    
    (super-new)))

;===============================================================================
; A Enemy is a (new Enemy% [position Posn] [vel Velocity])

(define Enemy%
  (class* object% (Unit<%>)
    (init-field position ; Posn, holds the center position of this unit
                vel ) ; Velocity, represents downward veleocity of this unit 
    (define ENEMY-COLOR "red") ; Color, color of enemy unit
    (define ELIMINATION-SCORE +40) ; Real, score on getting eliminated
    (define REACH-BASE-SCORE -40) ; Real, score on reaching base
    (field [score #false]); MaybeSubscriber<%>,Score object which will be 
    ; notified of new score
    
    ; get-loc : -> posn
    (define/public (get-loc)
      position)
    
    ; get-color : -> Color
    (define/public (get-color)
      ENEMY-COLOR)
    
    ; handle-tick! : -> Void
    ; EFFECT: mutates this unit to its next state
    (define/public (handle-tick!)
      (set! position (make-posn (posn-x position)
                                (+ (posn-y position) vel))))
    
    ; within-my-region? : Coordinate Coordinate -> Boolean
    (define/public (within-my-region? x y)
      (<= (sqrt (+ (sqr (- (posn-x position) x)) (sqr (- (posn-y position) y))))
          ENEMY-SIZE))
    
    ; update-score-on-elimination : -> Void
    (define/public (update-score-on-elimination)
      (send score update-score! ELIMINATION-SCORE))
    
    ; reached-base? : Coordinate -> Boolean
    (define/public (reached-base? base-height)
      (>= (+ (posn-y position) ENEMY-SIZE) (- CANVAS-HEIGHT base-height)))
    
    ; update-score-on-reachingBase : -> Void
    (define/public (update-score-on-reachingBase)
      (send score update-score! REACH-BASE-SCORE))
    
    ; draw : Image -> Image
    (define/public (draw image)
      (place-image ENEMY (posn-x position) (posn-y position) image))   
    
    ; subscribe! : Subscriber<%> -> Void
    ; EFFECT: Adds a new subscriber for score
    (define/public (subscribe! new-sub)
      (set! score new-sub))
    
    (super-new)))

;===============================================================================
; A Mercenary is a (new Mercenary% [position Posn] [vel Velocity])

(define Mercenary%
  (class* object% (Unit<%> Publisher<%>)
    (init-field position ; Posn, holds the center position of this ally unit
                vel ); Velocity, represents downward veleocity of this unit 
    
    (define A-ELIMINATION-SCORE -60) ; Real, score on getting eliminated
    (define A-REACH-BASE-SCORE +60) ; Real, score on reaching base 
    (define E-ELIMINATION-SCORE +60) ; Real, score on getting eliminated
    (define E-REACH-BASE-SCORE -60) ; Real, score on reaching base 
    
    (field [stateObj (new Ally% [position position] [vel vel])]
           ; Unit<%>, holds the current state object either Ally or Enemy
           [CHANGE-UNIT-FREQ 2] ; Natural, frequence at which stateObj changes
           [change-unit-countdown CHANGE-UNIT-FREQ] ;Natural, counter for change
           [score #false]); MaybeSubscriber<%>,Score object which will be 
                          ; notified of new score
    
    ; get-loc : -> posn
    (define/public (get-loc)
      position)
    
    ; get-color : -> Color
    (define/public (get-color)
      (send stateObj get-color))
    
    ; handle-tick! : Coordinate -> Void
    ; EFFECT: mutates this unit to its next state
    ; Changes the stateObj to ally or enemy based on change-unit-countdown
    (define/public (handle-tick!)
      (set! position (make-posn (posn-x position)
                                (+ (posn-y position) vel)))
      (send stateObj handle-tick!)
      (if (zero? change-unit-countdown)
          (changeStateObj!)
          (set! change-unit-countdown (sub1 change-unit-countdown)))) 
    
    ; changeStateObj! : -> Void
    ; Updates stateObj by changing it to Ally or Enemy Unit based on its 
    ; current value and resets change-unit-countdown
    (define (changeStateObj!)
      (set! change-unit-countdown CHANGE-UNIT-FREQ)
      (if (is-a? stateObj Ally%)
          (set! stateObj (new Enemy% [position position] [vel vel]))
          (set! stateObj (new Ally% [position position] [vel vel]))))
    
    ; within-my-region? : Coordinate Coordinate -> Boolean
    (define/public (within-my-region? x y)
      (send stateObj within-my-region? x y))
    
    ; update-score-on-elimination : -> Void
    (define/public (update-score-on-elimination)
      (if (is-a? stateObj Ally%)
          (send score update-score! A-ELIMINATION-SCORE)
          (send score update-score! E-ELIMINATION-SCORE))) 
    
    ; reached-base? : Coordinate -> Boolean
    (define/public (reached-base? base-height)
      (send stateObj reached-base? base-height))
    
    ; update-score-on-reachingBase : -> Void
    (define/public (update-score-on-reachingBase)
      (if (is-a? stateObj Ally%)
          (send score update-score! A-REACH-BASE-SCORE)
          (send score update-score! E-REACH-BASE-SCORE)))
    
    ; draw : Image -> Image
    (define/public (draw image)
      (send stateObj draw image))
    
    ; subscribe! : Subscriber<%> -> Void
    ; EFFECT: Adds a new subscriber.
    (define/public (subscribe! new-sub)
      (set! score new-sub))
    
    (super-new)))

;===============================================================================
; mk-world : Velocity Velocity Natural -> StatefulWorld<%>
; Creates a world with num-units initial random units,
; where units have the specified min and max velocity.
; WHERE: minvel <= maxvel
; STRATEGY : function compoisition
(begin-for-test
  (check-equal? (send (mk-world 5 10 '()) get-units)
                '()
                "World created with empty units list"))

(define (mk-world maxvel minvel num-units) 
  (new World% [units num-units] [minVel minvel] [maxVel maxvel]))

;===============================================================================
; mk-enemy : posn Velocity -> Unit<%>
; Creates an enemy unit with the given parameters.
; STRATEGY : function compoisition
(begin-for-test
  (check-equal? (get-field vel (mk-enemy (make-posn 40 50) 2))
                2
                "Enemy created with vel 2"))

(define (mk-enemy position velocity)
  (new Enemy% [position position] [vel velocity]))

;===============================================================================
; mk-ally : posn Velocity -> Unit<%>
; Creates an ally unit with the given parameters.
; STRATEGY : function compoisition
(begin-for-test
  (check-equal? (get-field vel (mk-ally (make-posn 40 50) 2))
                2
                "Ally created with vel 2"))

(define (mk-ally position velocity)
  (new Ally% [position position] [vel velocity]))

;===============================================================================
; mk-merc : posn Velocity -> Unit<%>
; Creates a mercenary unit with the given parameters.
; STRATEGY : function compoisition
(begin-for-test
  (check-equal? (get-field vel (mk-merc (make-posn 40 50) 2))
                2
                "Mercenary created with vel 2"))

(define (mk-merc position velocity)
  (new Mercenary% [position position] [vel velocity]))

;===============================================================================
; Examples for testing
(define all-ex (mk-ally (make-posn 100 45) 5))
(define enemy-ex (mk-enemy (make-posn 350 400) 4))
(define merc-ex (mk-merc (make-posn 220 356) 2))
(define INIT-WORLD (mk-world 5 1 (list all-ex enemy-ex merc-ex)))

;===============================================================================
; run : World -> World
; Runs the given world by handling mouse inputs and rendering the world
(define (run w)
  (big-bang w
            (on-mouse send-world-mouse)
            (to-draw send-world-render)
            (on-tick send-world-tick)
            (stop-when end? render-last)))

;===============================================================================
; send-world-mouse : World<%> Coordinate Coordinate MouseEvent -> World<%>
; Sends mouse event to the world object and returns back the updated world
; STRATEGY : function compoisition
(begin-for-test
  (check-equal? (send (send-world-mouse INIT-WORLD 50 50 "move") target-loc)
                (make-posn 50 50)
                "Handles mouse event"))

(define (send-world-mouse w x y me)
  (send w on-mouse! x y me)
  w)

;===============================================================================
; send-world-render : World<%> -> Image
; Sends render call to the world object which returns the rendered world
; STRATEGY : function compoisition
(begin-for-test
  (check-equal? (send-world-render INIT-WORLD)
                (place-image 
                 CROSS-BOW 50 50 
                 (overlay/align "center" "bottom" 
                                (overlay (text "0" 50 "black") 
                                         (rectangle 400 50 "solid" "yellow"))
                                (place-image 
                                 ENEMY 350 400 
                                 (place-image ALLY 100 45 
                                              (place-image ALLY 220 356 MT)))))
                "Renders world"))

(define (send-world-render w)
  (send w render))

;===============================================================================
; send-world-tick : World<%> -> World<%>
; Sends tick event to the world object and returns back the updated world
; STRATEGY : function compoisition
(begin-for-test
  (check-equal? (map (λ (u) (posn-y (get-field position u))) 
                     (send (send-world-tick INIT-WORLD) get-units)) 
                '(50 404 358)
                "Handles tick event"))

(define (send-world-tick w)
  (send w on-tick!) 
  w)

;===============================================================================
; end? : World<%> -> Boolean
; Sends end? call to the world object and returns #t if world should end
; STRATEGY : function compoisition
(begin-for-test
  (check-equal? (end? INIT-WORLD) #f "Returns false"))

(define (end? w)
  (send w end?))

;===============================================================================
; render-last : World<%> -> Image
; Sends render-last event to the world object and returns the lastworld image 
; STRATEGY : function compoisition
(begin-for-test
  (check-equal? (render-last INIT-WORLD)
                (overlay (text (string-append "Game Over:0") 25 "black") MT)
                "Renders last world"))

(define (render-last w)
  (send w render-last))

;===============================================================================

; Test cases
(begin-for-test
  (local
    ((define merc-ex2 (mk-merc (make-posn 110 200) 2))
     (define world3 (mk-world 5 1 (list merc-ex2))))
    (check-equal? (send (first (send world3 get-units)) get-color) "green")
    (send world3 on-tick!)
    (send world3 on-tick!)
    (send world3 on-tick!)
    (check-equal? (send (first (send world3 get-units)) get-color) "red"
                  "Mercenary changes from ally to enemy after 3 ticks")
    (send world3 on-tick!)
    (send world3 on-tick!)
    (send world3 on-tick!)
    (check-equal? (send (second (send world3 get-units)) get-color) "green"
                  "Mercenary changes back to ally after 3 ticks")
    (send world3 on-mouse! 110 218 "button-down")
    (check-equal? (send (get-field score world3) get-score) -60
                  "Mercenary removed on button down when it is ally")))


(begin-for-test
  (local
    ((define all-ex1 (mk-ally (make-posn 220 45) 5))
     (define enemy-ex1 (mk-enemy (make-posn 220 400) 4))
     (define merc-ex1 (mk-merc (make-posn 220 356) 2))
     (define world1 (mk-world 5 1 (list enemy-ex1 merc-ex1))))
    (send world1 add-unit! all-ex1)
    (send world1 on-tick!)
    (send world1 on-tick!)
    (send world1 on-tick!)
    (send world1 on-tick!)
    (check-equal? (send (second (send world1 get-units)) get-loc) 
                  (make-posn 220 65)
                  "Ally position after 4 ticks")
    (check-equal? (send (third (send world1 get-units)) get-loc) 
                  (make-posn 220 416)
                  "Enemy position after 4 ticks")
    (check-equal? (send (fourth (send world1 get-units)) get-loc) 
                  (make-posn 220 364)
                  "Mercenary position after 4 ticks")
    (check-equal? (map (λ (u) (posn-y (get-field position u))) 
                       (send world1 get-units)) 
                  '(0 65 416 364)
                  "Handles tick event")))


(begin-for-test
  (local
    ((define all-ex1 (mk-ally (make-posn 220 45) 5))
     (define enemy-ex1 (mk-enemy (make-posn 220 400) 4))
     (define merc-ex1 (mk-merc (make-posn 220 356) 2))
     (define world2 (mk-world 5 1 (list all-ex1 enemy-ex1 merc-ex1))))
    (send world2 on-mouse! 220 45 "button-down")
    (check-equal? (send (get-field score world2) get-score) -20
                  "Ally unit removed on button down")
    (send world2 on-mouse! 220 400 "button-down")
    (send world2 on-mouse! 220 400 "button-up")
    (check-equal? (send (get-field score world2) get-score) 20
                  "Enemy unit removed on button down")    
    (send world2 on-tick!)
    (send world2 on-tick!)
    (send world2 on-tick!)
    (send world2 on-mouse! 220 360 "button-down")
    (check-equal? (send (get-field score world2) get-score) 80
                  "Mercenary unit removed on button down")))

(begin-for-test
  (local
    ((define merc-ex1 (mk-merc (make-posn 110 435) 6))
     (define merc-ex2 (mk-merc (make-posn 110 415) 5))
     (define world4 (mk-world 5 1 (list merc-ex1 merc-ex2))))
    (send world4 on-tick!)
    (check-equal? (send (get-field score world4) get-score) 60
                  "Score updated on mercenary reaching base as ally")
    (send world4 on-tick!)
    (send world4 on-tick!)
    (send world4 on-tick!)
    (check-equal? (send (get-field score world4) get-score) 0
                  "Score updated on mercenary reaching base as enemy")))

(begin-for-test
  (local
    ((define merc-ex1 (mk-merc (make-posn 110 435) 6))
     (define world4 (mk-world 5 1 (list merc-ex1))))
    (send world4 on-tick!)
    (check-equal? (send (get-field score world4) get-score) 60
                  "Score updated on mercenary reaching base as ally")
    (check-equal? (send world4 render) 
                  (place-image 
                   CROSS-BOW 0 0 
                   (overlay/align "center" "bottom" 
                                  (overlay (text "60" 50 "black") 
                                           (rectangle 400 62 "solid" "yellow"))
                                  MT))
                  "Rendering world when score is above 0")))

(begin-for-test
  (local
    ((define all-ex1 (mk-ally (make-posn 110 435) 6))
     (define enemy-ex1 (mk-enemy (make-posn 110 430) 5))
     (define world4 (mk-world 5 1 (list all-ex1 enemy-ex1))))
    (send world4 on-tick!)
    (check-equal? (send (get-field score world4) get-score) 20
                  "Score updated on ally reaching base")
    (send world4 on-tick!)
    (check-equal? (send (get-field score world4) get-score) -20
                  "Score updated on enemy reaching base")))



(begin-for-test
  (local
    (; create-units : World<%> -> Boolean
     ; Returns true once all 3 units are generated (written for code coverage as
     ; random function is used to generate units in world)
     ; Termination Argument: May not terminate if random doesn't generate
     ; all 3 units
     (define (create-units w)
       (local
         ((define (create-units/a lou)
            (if (equal? (length lou) 3) #true
                (let ([dummy #t])
                  (send w on-tick!)
                  (send w on-tick!)
                  (send w on-tick!)
                  (send w on-tick!)
                  (create-units/a (check-for-units (send w get-units) lou)))))
          
          ; check-for-units : Units<%> ListOf<String> -> ListOf<String>
          ; Updates newlist based on the type of new unit generated
          (define (check-for-units units newlist)
            (cond
              [(empty? units) newlist]
              [(is-a? (first units) Ally%) 
               (check-for-units (rest units) (add-unit "Ally" newlist))]
              [(is-a? (first units) Enemy%) 
               (check-for-units (rest units) (add-unit "Enemy" newlist))]
              [(is-a? (first units) Mercenary%) 
               (check-for-units (rest units) (add-unit "Merc" newlist))]))
          
          ; add-unit : String ListOf<String> -> ListOf<String>
          ; Adds given string to newlist if it is not preset already
          (define (add-unit name newlist)
            (if (empty? (filter (λ (x) (string=? name x)) newlist))
                (cons name newlist)
                newlist)))
         
         (create-units/a '()))))
    
    (check-equal? (create-units (mk-world 5 1 '())) #t
                  "Checks random generation of all 3 units")))

;===============================================================================

;Alternate Designs
; 1. Pull style for score
;   - Instead of currently used Publisher-subscriber style for score, world 
;     could have pulled the score on every tick and mouse event. Since pull
;     style in this case results in unneccesary polling of data, and as it is
;     easier for the units to update score based on their state, Publisher 
;     subscriber method was chosen for score update.
;
; 2. Push style for unit removal from world
;   - Currently world queries on units to decide whether they need to be removed
;     from the world and thus uses pull style. On the other hand, push style
;     could be used here such that the units decide when they need to be removed
;     and notify the world on such situations. In this case world needs to 
;     identify this particular object in its list of units and drop that object.
;     Since this requires extra object comparison by parameters and if there are
;     two objects with same parameters in the world, this can lead to confusion
;     and hence pull method was chosen.




