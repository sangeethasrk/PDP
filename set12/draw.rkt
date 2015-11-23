#lang racket
(require "extras.rkt")
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require "triangle.rkt")
(define TIME-ON-TASK 15)

(provide INITIAL-WORLD)
(provide handle-mouse)
(provide Shape<%>)
(provide get-world-shapes)
(provide create-rectangle)
(provide create-circle)
(provide create-triangle)

;===============================================================================

; Constants
(define TL-SIZE 20) ; pixels
(define TOOLBAR (square TL-SIZE "outline" "black")) ;Image
(define POINTER (overlay (text "p" 15 "black") TOOLBAR)) ;Image
(define RECTANGLE (overlay (text "r" 15 "black") TOOLBAR)) ;Image 
(define CIRCLE (overlay (text "c" 15 "black") TOOLBAR)) ;Image
(define TRIANGLE (overlay (text "t" 15 "black") TOOLBAR)) ;Image

(define TOOLBAR-SEL (square TL-SIZE "solid" "black")) ;Image
(define POINTER-SEL (overlay (text "p" 15 "white") TOOLBAR-SEL));Image 
(define RECTANGLE-SEL (overlay (text "r" 15 "white") TOOLBAR-SEL)) ;Image
(define CIRCLE-SEL (overlay (text "c" 15 "white") TOOLBAR-SEL)) ;Image
(define TRIANGLE-SEL (overlay (text "t" 15 "white") TOOLBAR-SEL)) ;Image

(define WIDTH 500) ; pixels
(define HEIGHT 500) ; pixels

(define MT (empty-scene WIDTH HEIGHT)) ;Image

; Default posn value for previous mouse position
(define INIT-PREV-POSN (make-posn 0 0))

;===============================================================================

; A SelTool is a Symbol and is one of
; - 'point
; - 'rect
; - 'circ
; INTERP: Represents the currently selected tool amongst the available tools
(define POINT 'point)
(define RECT 'rect)
(define CIRC 'circ)
(define TRI 'tri)

; <selTool-predicates> : SelTool -> Boolean
; Returns true if sel is the symbol indicated by function name
; Strategy : function composition
(define (point? sel) (symbol=? sel POINT))
(define (rect? sel) (symbol=? sel RECT))
(define (circ? sel) (symbol=? sel CIRC))
(define (tri? sel) (symbol=? sel TRI))

; TEMPLATE
; selTool-fn : SelTool -> ??
;(define (selTool-fn sel)
;  (cond
;    [(point? sel) ...]
;    [(rect? sel) ...]
;    [(circ? sel) ...]
;    [(tri? sel) ...]))

;===============================================================================

; A World is a (make-struct ListOf<Shape<%>> SelTool)
; INTERP: where shapes represent list of all shapes present in the world
; selTool represents the current selection in the toolbar
(define-struct world (shapes selTool) #:transparent) 

; Template
; world-fn : World -> ??
;(define (world-fn w)
;  ( ... (lox (world-shapes w)) ... (selTool-fn (world-selTool w)) ...))

; Data example
; INITIAL-WORLD : World
; An initial world, with no Shape<%>s.
(define INITIAL-WORLD (make-world '() POINT))

;===============================================================================

; A BoundingBox is a (list Coordinate Coordinate Coordinate Coordinate)
; INTERPRETATION: (list left top right bottom).
; A BoundingBox represents a box whose left x-coordinate is at "left", whose
; top y-coordinate is at "top", whose right x-coordinate is at "right", and 
; whose bottom y-coordinate is at "bottom".

; Data example
(define boundingBox (list 100 100 110 110))

;===============================================================================

; run : World -> World
; Runs the given world by handling mouse inputs and rendering the world
(define (run w)
  (big-bang w
            (on-mouse handle-mouse)
            (to-draw render)))

;===============================================================================

(define Shape<%>
  (interface ()
    ; get-bounds : -> BoundingBox
    ; Returns the bounding box of the shape
    get-bounds
    
    ; handle-mouse : Coordinate Coordinate MouseEvent -> Shape<%>
    ; Returns new Shape on handling mouse-events
    handle-mouse
    
    ; draw : Image -> Image
    ; Renders the Shape on the given image
    draw))

;===============================================================================

; A Rectangle is a 
; (new Rectangle% [left Coordinate] [top Coordinate] [right Coordinate] 
;                 [bottom Coordinate] [prevPosn Posn] [new? Boolean]) 

(define Rectangle%
  (class* object% (Shape<%>)
    (init-field left ; Coordinate, left x-coordinate of rectangle in Canvas
                top ; Coordinate, top y-coordinate of rectangle in Canvas
                right ; Coordinate, right x-coordinate of rectangle in Canvas
                bottom ; Coordinate, bottom y-coordinate of rectangle in Canvas
                prevPosn ; Posn, holds previous mouse position
                new?) ; Boolean, is true if the rectangle is still not complete
    (define CONTROL_POINT 5) ; Pixels
    
    ; get-bounds : -> BoundingBox
    ; Returns the bounding box of the rectangle
    (define/public (get-bounds)
      (list left top right bottom))
    
    
    ; handle-mouse : Coordinate Coordinate MouseEvent -> Rectannle<%>
    ; Returns new Rectangle on handling mouse events
    ; WHERE event are only "button-down" , "drag", "button-up"
    ; Stategy: data decomposition on event : MouseEvent
    (define/public (handle-mouse x y event)
      (cond
        [(string=? "button-down" event) (handleBD x y)]
        [(string=? "drag" event) (handleDrag x y)]
        [(string=? "button-up" event) (handleBU x y)]))
    
    
    ; formRect: Coordinate Coordinate Coordinate Coordinate Posn Boolean  
    ;            -> Rectangle<%>
    ; Returns new rectangle by ensuring the condition of the rectangle
    (define (formRect l t r b dp n)
      (if (> l r) (checkY r t l b dp n) (checkY l t r b dp n)))
    
    
    ; checkY: Coordinate Coordinate Coordinate Coordinate Posn Boolean  
    ;            -> Rectangle<%>
    ; Returns new rectangle by ensuring the condition of the rectangle
    (define (checkY l t r b dp n)
      (if (> t b)
          (new this% [left l] [top b] [right r] [bottom t] 
               [prevPosn dp] [new? n])
          (new this% [left l] [top t] [right r] [bottom b] 
               [prevPosn dp] [new? n])))
    
    ; handleBD: Coordinate Coordinate -> Rectangle<%>
    ; Returns new rectangle by handling button-down event
    (define (handleBD x y)
      (if (topLeftBound? x y) 
          (formRect x y right bottom (make-posn x y) #false)
          (checkTR-BD x y)))
    
    ; checkTR-BD: Coordinate Coordinate -> Rectangle<%>
    ; Returns new rectangle by checking if mouse event is at top-right corner
    (define (checkTR-BD x y)
      (if (topRightBound? x y)
          (formRect left y x bottom (make-posn x y) #false)
          (checkBL-BD x y)))
    
    ; checkBL-BD: Coordinate Coordinate -> Rectangle<%>
    ; Returns new rectangle by checking if mouse event is at bottom-left corner
    (define (checkBL-BD x y)
      (if (bottomLeftBound? x y)
          (formRect x top right y (make-posn x y) #false)
          (checkBR-BD x y)))
    
    ; checkBR-BD: Coordinate Coordinate -> Rectangle<%>
    ; Returns new rectangle by checking if mouse event is at bottom-right corner
    (define (checkBR-BD x y)
      (if (bottomRightBound? x y)
          (formRect left top x y (make-posn x y) #false)
          (insideRect-BD x y)))
    
    ; insideRect-BD: Coordinate Coordinate -> Rectangle<%>
    ; Returns new rectangle by checking if mouse-event is within rectangle
    (define (insideRect-BD x y)
      (if (and (< left x right) (< top y bottom))
          (formRect left top right bottom (make-posn x y) #false) 
          this))
    
    ; handleDrag: Coordinate Coordinate -> Rectangle<%>
    ; Returns new rectangle by handling drag event
    (define (handleDrag x y)
      (if new?
          (new this% [left left] [top top] [right x] [bottom y] 
               [prevPosn INIT-PREV-POSN] [new? new?])
          (checkTLDrag x y)))
    
    ; checkTLDrag: Coordinate Coordinate -> Rectangle<%>
    ; Returns new rectangle by checking if mouse event is at top-left corner
    ; Strategy : data decomposition on prevPosn : Posn
    (define (checkTLDrag x y)
      (if (and (equal? left (posn-x prevPosn)) 
               (equal? top (posn-y prevPosn)) (prevPosnInitialised?)) 
          (formRect x y right bottom (make-posn x y) new?)
          (checkTRDrag x y)))
    
    ; checkTRDrag: Coordinate Coordinate -> Rectangle<%>
    ; Returns new rectangle by checking if mouse event is at top-right corner
    ; Strategy : data decomposition on prevPosn : Posn
    (define (checkTRDrag x y)
      (if (and (equal? right (posn-x prevPosn)) 
               (equal? top (posn-y prevPosn)) (prevPosnInitialised?))
          (formRect left y x bottom (make-posn x y) new?)
          (checkBLDrag x y)))
    
    ; checkBLDrag: Coordinate Coordinate -> Rectangle<%>
    ; Returns new rectangle by checking if mouse event is at bottom-left corner
    ; Strategy : data decomposition on prevPosn : Posn
    (define (checkBLDrag x y)
      (if (and (equal? left (posn-x prevPosn)) 
               (equal? bottom (posn-y prevPosn)) (prevPosnInitialised?))
          (formRect x top right y (make-posn x y) new?)
          (checkBRDrag x y)))
    
    ; checkBRDrag: Coordinate Coordinate -> Rectangle<%>
    ; Returns new rectangle by checking if mouse event is at bottom-right corner
    ; Strategy : data decomposition on prevPosn : Posn
    (define (checkBRDrag x y)
      (if (and (equal? right (posn-x prevPosn)) 
               (equal? bottom (posn-y prevPosn)) (prevPosnInitialised?))
          (formRect left top x y (make-posn x y) new?)
          (insideRectDrag x y)))
    
    ; insideRectDrag: Coordinate Coordinate -> Rectangle<%>
    ; Returns new rectangle by checking if mouse event is within rectangle
    (define (insideRectDrag x y)
      (if (prevPosnInitialised?)
          (local
            ((define x-diff (- x (posn-x prevPosn)))
             (define y-diff (- y (posn-y prevPosn))))
            
            (formRect (+ x-diff left) (+ y-diff top) 
                      (+ x-diff right) (+ y-diff bottom) (make-posn x y) new?))
          this))
    
    ; prevPosnInitialised?: -> Boolean
    ; Returns true if prePosn has value other than zero
    ; Strategy : data decomposition on prevPosn : Posn
    (define (prevPosnInitialised?)
      (not (and (zero? (posn-x prevPosn)) (zero? (posn-y prevPosn)))))
    
    ; handleBU: Coordinate Coordinate -> Rectangle<%>
    ; Returns new rectangle by handling button-up event
    (define (handleBU x y)
      (if new?
          (formRect left top x y INIT-PREV-POSN #false)
          (checkMove/Resize x y)))
    
    ; checkMove/Resize: Coordinate Coordinate -> Rectangle<%>
    ; Returns new rectangle by checking if mouse event is within rectangle
    (define (checkMove/Resize x y)
      (if (withinBounds? x y)
          (resetPrevPos (checkTLDrag x y))
          this))
    
    ; resetPrevPos: Rectangle<%> -> Rectangle<%>
    ; Returns new rectangle by resetting prevPosn to default value
    (define (resetPrevPos rect)
      (new this% [left (get-field left rect)] [top (get-field top rect)] 
           [right (get-field right rect)] [bottom (get-field bottom rect)] 
           [prevPosn INIT-PREV-POSN] [new? (get-field new? rect)]))
    
    
    ; draw : -> Image
    ; Render the rectangle on the given image
    (define/public (draw image)
      (local
        ((define width (- right left))
         (define height (- bottom top))
         ; get-rect -> Image
         ; returns appropriate rectangle image based on new? state
         (define (get-rect)
           (if new?
               (rectangle (abs width) (abs height) 50 "red")
               (rectangle (abs width) (abs height) "outline" "black"))))
        
        (place-image (get-rect) (+ left (/ width 2)) 
                     (+ top (/ height 2)) image)))
    
    ; withinBounds? : Coordinate Coordinate -> Boolean
    ; Returns true if given coordinates are within rectangle
    (define (withinBounds? x y)
      (or (and (< left x right) (< top y bottom))
          (topLeftBound? x y) (topRightBound? x y) 
          (bottomLeftBound? x y) (bottomRightBound? x y)))
    
    ; topLeftBound? : Coordinate Coordinate -> Boolean
    ; Returns true if given coordinates are within 5pixels of top-left corner
    (define (topLeftBound? x y)
      (and (<= (- left CONTROL_POINT) x (+ left CONTROL_POINT)) 
           (<= (- top CONTROL_POINT) y (+ top CONTROL_POINT))))
    
    ; topRightBound? : Coordinate Coordinate -> Boolean
    ; Returns true if given coordinates are within 5pixels of top-right corner
    (define (topRightBound? x y)
      (and (<= (- right CONTROL_POINT) x (+ right CONTROL_POINT)) 
           (<= (- top CONTROL_POINT) y (+ top CONTROL_POINT))))
    
    ; bottomLeftBound? : Coordinate Coordinate -> Boolean
    ; Returns true if given coordinates are within 5pixels of bottom-left corner
    (define (bottomLeftBound? x y)
      (and (<= (- left CONTROL_POINT) x (+ left CONTROL_POINT)) 
           (<= (- bottom CONTROL_POINT) y (+ bottom CONTROL_POINT))))
    
    ; bottomRightBound? : Coordinate Coordinate -> Boolean
    ; Returns true if given coordinates are within 5pixel of bottom-right corner
    (define (bottomRightBound? x y)
      (and (<= (- right CONTROL_POINT) x (+ right CONTROL_POINT)) 
           (<= (- bottom CONTROL_POINT) y (+ bottom CONTROL_POINT))))
    
    (super-new)))

;===============================================================================
; Rectangle Testing

(define RECT-TEST (new Rectangle% [left 100] [top 100] [right 100]
                       [bottom 100] 
                       [prevPosn INIT-PREV-POSN] [new? #true] ))
(begin-for-test
  (check-equal?
   (send 
    (send (send 
           (send (send (send 
                        (send (send
                               (send (send 
                                      (send 
                                       (send 
                                        (send 
                                         (send 
                                          RECT-TEST
                                          handle-mouse 120 120 "drag")
                                         handle-mouse 150 150 "button-up")
                                        handle-mouse 120 120 "button-down")
                                       handle-mouse 140 130 "drag")
                                      handle-mouse 115 110 "drag")
                                     handle-mouse 150 150 "button-up")
                               handle-mouse 153 152 "button-down")
                              handle-mouse 120 120 "drag")
                        handle-mouse 0 100 "drag")
                       handle-mouse 70 80 "button-up")
                 handle-mouse 5 5 "button-down")
           handle-mouse 6 6 "drag")
          handle-mouse 7 7 "button-up") get-bounds)
   '(-13 -13 37 37)
   "Rectangle test cases")
  
  (check-equal?
   (send 
    (send (send 
           (send (send 
                  (send (send 
                         (send (send
                                (send (send
                                       (send 
                                        (send 
                                         (send 
                                          RECT-TEST handle-mouse 
                                          150 150 "button-up")
                                         handle-mouse 98 98 "button-down")
                                        handle-mouse 103 103 "drag")
                                       handle-mouse 105 105 "button-up")
                                      handle-mouse 152 153 "button-down") 
                                handle-mouse 155 155 "drag")
                               handle-mouse 155 155 "button-up")
                         handle-mouse 106 156 "button-down")
                        handle-mouse 103 153 "drag")
                  handle-mouse 100 155 "button-up")
                 handle-mouse 156 103 "button-down")
           handle-mouse 157 104 "drag")
          handle-mouse 155 100 "button-up")
    get-bounds)
   '(100 100 155 155)
   "Resize")
  
  (check-equal?
   (send (send (send (send (send  RECT-TEST handle-mouse 120 120 "drag")
                           handle-mouse 150 150 "button-up")
                     handle-mouse 152 152 "button-down")
               handle-mouse 80 70 "drag") get-bounds)
   '(80 70 100 100)
   "Reversing rectangle")
  
  (check-equal?
   (send (send (send RECT-TEST handle-mouse 120 120 "button-up")
               handle-mouse 110 110 "drag") get-bounds)
   '(100 100 120 120)
   "Ignored mouse events")
  
  (check-equal?
   (send (send (send RECT-TEST handle-mouse 120 120 "button-up")
               handle-mouse 110 110 "drag") draw MT)
   (place-image (rectangle 20 20 "outline" "black") (+ 100 10) (+ 100 10) MT)
   "Rectanagle rendering"))

;===============================================================================

; A Circle is a 
; (new Circle% [radius Pixels] [center Posn]  
;                 [new? Boolean] [prevPosn Posn]) 

(define Circle%
  (class* object% (Shape<%>)
    (init-field radius ; Pixels, represents the radius of the circle
                center ; Posn, represents the center of the circle in Canvas
                new? ; Boolean, represents if the circle is completely formed
                prevPosn) ;Posn, represents the previous mouse position 
    (define CONTROL_POINT 2)
    
    ; get-bounds : -> BoundingBox
    ; Returns the bounding box of the circle
    ; STRATGEY : data decomposition on center : Posn
    (define/public (get-bounds)
      (list (- (posn-x center) radius) (- (posn-y center) radius) 
            (+ (posn-x center) radius) (+ (posn-y center) radius)))
    
    
    ; handle-mouse : Coordinate Coordinate MouseEvent -> Circle<%>
    ; Returns new Circle on handling mouse events
    ; WHERE event are only "button-down" , "drag", "button-up"
    ; Stategy: data decomposition on event : MouseEvent
    (define/public (handle-mouse x y event)
      (cond
        [(string=? "button-down" event) (handleBD x y)]
        [(string=? "drag" event) (handleDrag x y)]
        [(string=? "button-up" event) (handleBU x y)]))
    
    ; handleBD: Coordinate Coordinate -> Circle<%>
    ; Returns new Circle by handling button-down event
    ; Stategy: data decomposition on center : Posn
    (define (handleBD x y)
      (if (< (sqrt (+ (sqr (- x (posn-x center))) (sqr (- y (posn-y center)))))
             (- radius CONTROL_POINT))
          (new Circle% [radius radius] [center center] 
               [new? new?] [prevPosn (make-posn x y)])
          (checkResize-BD x y)))
    
    ; checkResize-BD: Coordinate Coordinate -> Circle<%>
    ; Returns new Circle by checking if mouse event is close to circumference
    ; Strategy : data decomposition on center : Posn
    (define (checkResize-BD x y)
      (if (<= (- radius CONTROL_POINT) 
              (sqrt (+ (sqr (- x (posn-x center))) (sqr (- y (posn-y center)))))
              (+ radius CONTROL_POINT))
          (new Circle% [radius (distance x y center)] [center center] 
               [new? new?] [prevPosn (make-posn x y)])
          this))
    
    ; handleDrag: Coordinate Coordinate -> Circle<%>
    ; Returns new Circle by handling drag event
    (define (handleDrag x y)
      (if new?
          (new Circle% [radius (distance x y center)] [center center] 
               [new? new?] [prevPosn prevPosn])
          (checkMove/Resize-Drag x y)))
    
    ; checkMove/Resize-Drag: Coordinate Coordinate -> Circle<%>
    ; Returns new Circle by checking if mouse event is close to circumference
    ; or within circle on a drag event
    ; Strategy : data decomposition on center : Posn
    (define (checkMove/Resize-Drag x y)
      (if (and (prevPosnInitialised?)
               (= (distance (posn-x center) (posn-y center) prevPosn) radius))
          (new Circle% [radius (distance x y center)] 
               [center center] [new? new?] [prevPosn (make-posn x y)])
          (checkMove-Drag x y)))
    
    ; checkMove-Drag: Coordinate Coordinate -> Circle<%>
    ; Returns new Circle by checking if mouse event is within circle
    ; Strategy : data decomposition on center : Posn
    (define (checkMove-Drag x y)
      (if (and (prevPosnInitialised?)
               (< (distance (posn-x center) (posn-y center) prevPosn) radius))
          (new Circle% [radius radius] [center (newCenter x y)] 
               [new? new?] [prevPosn (make-posn x y)])
          this))
    
    ; handleBU: Coordinate Coordinate -> Circle<%>
    ; Returns new Circle by handling button-up event
    (define (handleBU x y)
      (if new?
          (new Circle% [radius (distance x y center)] [center center] 
               [new? #false] [prevPosn INIT-PREV-POSN])
          (checkMove/Resize-BU x y)))
    
    ; checkMove/Resize-BU: Coordinate Coordinate -> Circle<%>
    ; Returns new Circle by checking if mouse event is close to circumference
    ; or within circle on a button-up event
    ; Strategy : data decomposition on center : Posn
    (define (checkMove/Resize-BU x y)
      (if (and (prevPosnInitialised?)
               (< (sqrt (+ (sqr (- x (posn-x center))) 
                           (sqr (- y (posn-y center))))) 
                  (- radius CONTROL_POINT)))
          (new Circle% [radius radius] [center (newCenter x y)] 
               [new? new?] [prevPosn INIT-PREV-POSN])
          (checkResize-BU x y)))
    
    ; checkResize-BU: Coordinate Coordinate -> Circle<%>
    ; Returns new Circle by checking if mouse event is close to circumference
    ; Strategy : data decomposition on center : Posn
    (define (checkResize-BU x y)
      (if (and (prevPosnInitialised?)
               (<= (- radius CONTROL_POINT) 
                   (sqrt (+ (sqr (- x (posn-x center))) 
                            (sqr (- y (posn-y center))))) 
                   (+ radius CONTROL_POINT)))
          (new Circle% [radius (distance x y center)] 
               [center center] [new? new?] [prevPosn INIT-PREV-POSN])
          this))
    
    ; newCenter: Coordinate Coordinate -> Posn
    ; Returns new center based on the prevPosn
    (define (newCenter x y)
      (local
        (; newX: -> Coordinate
         ; Returns new x value for center
         ; Strategy : data decomposition on center, prevPosn : Posn
         (define (newX) (if (= x (posn-x prevPosn)) (posn-x center) 
                            (+ (posn-x center) (- x (posn-x prevPosn)))))
         
         ; newY: -> Coordinate
         ; Returns new y value for center
         ; Strategy : data decomposition on center, prevPosn : Posn
         (define (newY) (if (= y (posn-y prevPosn)) (posn-y center) 
                            (+ (posn-y center) (- y (posn-y prevPosn))))))
        
        (make-posn (newX) (newY))))
    
    ; distance: Coordinate Coordinate Posn -> Real
    ; Returns distance of x,y from given position
    ; Strategy : data decomposition on position : Posn
    (define (distance x y position)
      (sqrt (+ (sqr (- x (posn-x position))) (sqr (- y (posn-y position))))))
    
    ; prevPosnInitialised?: -> Boolean
    ; Returns true if prvPosn has values other than zero
    ; Strategy : data decomposition on prevPosn : Posn
    (define (prevPosnInitialised?)
      (not (and (zero? (posn-x prevPosn)) (zero? (posn-y prevPosn)))))
    
    ; draw : Image -> Image
    ; Renders circle on the given image
    ; Strategy : data decomposition on center : Posn
    (define/public (draw image)
      (local
        (; get-circle : -> Image
         ; Returns appropriate circle image based on new? status
         (define (get-circle)
           (if new?
               (circle radius 50 "red")
               (circle radius "outline" "black"))))
        
        (place-image (get-circle) (posn-x center) (posn-y center) image)))
    
    (super-new)))

;===============================================================================
; Circle Testing

(define CIRC-TEST (new Circle% [radius 0] [center (make-posn 100 100)] 
                       [new? #true] [prevPosn (make-posn 0 0)]))

(begin-for-test
  (check-equal?
   (get-field center (send 
                      (send (send 
                             (send (send
                                    (send (send 
                                           (send 
                                            (send 
                                             CIRC-TEST
                                             handle-mouse 110 110 "drag")
                                            handle-mouse 105 105 "drag")
                                           handle-mouse 110 115 "button-up")
                                          handle-mouse 105 106 "button-down")
                                    handle-mouse 110 111 "drag")
                                   handle-mouse 115 115 "button-up")
                             handle-mouse 129 109 "button-down")
                            handle-mouse 120 109 "drag")
                      handle-mouse 120 110 "button-up"))
   (make-posn 110 109)
   "Circle test cases")
  
  (check-equal?
   (send (send (send (send (send  CIRC-TEST  handle-mouse 110 100 "button-up")
                           handle-mouse 110 110 "drag")
                     handle-mouse 120 120 "button-down")
               handle-mouse 110 110 "button-up") get-bounds)
   '(90 90 110 110)
   "Ignored mouse inputs")
  
  (check-equal?
   (get-field center (send (send (send (send CIRC-TEST
                                             handle-mouse 110 110 "drag")
                                       handle-mouse 120 120 "button-up")
                                 handle-mouse 105 105 "button-down")
                           handle-mouse 105 105 "drag"))
   (make-posn 100 100)
   "Circle edge cases")
  
  (check-equal?
   (send (send CIRC-TEST handle-mouse 110 100 "button-up") draw MT)
   (place-image (circle 10 "outline" "black") 100 100 MT)
   "Circle rendering"))

;===============================================================================

(define Triangle%
  (class* object% (Shape<%>)
    (init-field center ; Posn, represents center of the triangle in Canvas
                corner ; Posn, represents an arbitrary corner of triangle 
                new? ; Boolean, represents if the triangle is completely formed
                prevPosn) ;Posn, represents the previous mouse position 
    (define CONTROL_POINT 5)
    
    ; get-bounds : -> BoundingBox
    ; Returns the bounding box of the Triangle
    (define/public (get-bounds)
      (local
        ((define vertices (compute-corners center corner)))
        (calculate-bounds (first vertices) (second vertices)
                          (third vertices))))
    
    ; calculate-bounds: Posn Posn Posn -> BoundingBox
    ; Calculates the bounding box of the triangle based on its vertices
    ; STRATGEY : data decomposition on center : Posn
    (define (calculate-bounds corner1 corner2 corner3)
      (list (min (posn-x corner1) (posn-x corner2) (posn-x corner3))
            (min (posn-y corner1) (posn-y corner2) (posn-y corner3)) 
            (max (posn-x corner1) (posn-x corner2) (posn-x corner3))
            (max (posn-y corner1) (posn-y corner2) (posn-y corner3))))   
    
    ; handle-mouse : Coordinate Coordinate MouseEvent -> Triangle<%>
    ; Returns new Triangle on handling mouse events
    ; WHERE event are only "button-down" , "drag", "button-up"
    ; Stategy: data decomposition on event : MouseEvent
    (define/public (handle-mouse x y event)
      (cond
        [(string=? "button-down" event) (handleBD x y)]
        [(string=? "drag" event) (handleDrag x y)]
        [(string=? "button-up" event) (handleBU x y)]))
    
    ; handleBD: Coordinate Coordinate -> Triangle<%>
    ; Returns new Triangle by handling button-down event
    (define (handleBD x y)
      (local
        ((define nearestVertex (getNearestVertex 
                                (compute-corners center corner) x y)))
        
        (if (false? nearestVertex)
            (checkWithinTri x y)
            (new Triangle% [center center] [corner (make-posn x y)]
                 [new? #false] [prevPosn (make-posn x y)]))))
    
    ; getNearestVertex : ListOf<Posn> Coordinate Coordinate -> Maybe<Posn>
    ; Returns the vertex within the control region of given co-ordinates,
    ; false if not close to any of them
    (define (getNearestVertex vertices x y)
      (local
        ((define matches (filter (λ (vert) (withinCtrlReg? x y vert))
                                 vertices)))
        
        (if (empty? matches) #false
            (first matches))))
    
    ; checkWithinTri: Coordinate Coordinate -> Triangle<%>
    ; Returns new triangle based on whether given co-ordinate is within triangle
    (define (checkWithinTri x y)
      (local
        ((define vertices (compute-corners center corner)))
        
        (if (in-triangle? (make-posn x y) (first vertices) 
                          (second vertices) (third vertices))
            (new Triangle% [center center] [corner corner]
                 [new? #false] [prevPosn (make-posn x y)])
            this)))
    
    ; handleDrag: Coordinate Coordinate -> Triangle<%>
    ; Returns new Triangle by handling drag event
    (define (handleDrag x y)
      (if new?
          (new Triangle% [center center] [corner (make-posn x y)] 
               [new? new?] [prevPosn INIT-PREV-POSN])
          (checkMove/Resize-Drag x y)))
    
    ; checkMove/Resize-Drag: Coordinate Coordinate -> Triangle<%>
    ; Returns new Triangle by checking if mouse event is close to corners
    ; or within Triangle on a drag event
    ; Strategy : data decomposition on center, prevPosn : Posn
    (define (checkMove/Resize-Drag x y)
      (if (and (prevPosnInitialised?)
               (equal? (posn-x corner) (posn-x prevPosn))
               (equal? (posn-y corner) (posn-y prevPosn)))
          (new Triangle% [center center] 
               [corner (make-posn x y)] [new? new?] [prevPosn (make-posn x y)])
          (checkMove-Drag x y)))
    
    ; checkMove-Drag: Coordinate Coordinate -> Triangle<%>
    ; Returns new Triangle by checking if mouse event is within traingle
    (define (checkMove-Drag x y)
      (local
        ((define vertices (compute-corners center corner)))
        
        (if (and (prevPosnInitialised?)
                 (in-triangle? prevPosn (first vertices) 
                               (second vertices) (third vertices)))
            (new Triangle% [center (newPosn x y center)] 
                 [corner (newPosn x y corner)] 
                 [new? new?] [prevPosn (make-posn x y)])
            this)))
    
    ; handleBU: Coordinate Coordinate -> Triangle<%>
    ; Returns new triangle by handling button-up event
    (define (handleBU x y)
      (if new?
          (new Triangle% [center center] [corner (make-posn x y)] 
               [new? #false] [prevPosn INIT-PREV-POSN])
          (checkMove/Resize-BU x y)))
    
    ; checkMove/Resize-BU: Coordinate Coordinate -> Triangle<%>
    ; Returns new triangle by checking if mouse event is close to corners
    ; or within triangle on a button-up event
    ; Strategy : data decomposition on center,prevPosn : Posn
    (define (checkMove/Resize-BU x y)
      (if (and (prevPosnInitialised?)
               (equal? (posn-x corner) (posn-x prevPosn))
               (equal? (posn-y corner) (posn-y prevPosn)))
          (new Triangle% [center center] 
               [corner (make-posn x y)] [new? new?] [prevPosn INIT-PREV-POSN])
          (checkMove-BU x y)))
    
    ; checkMove-BU: Coordinate Coordinate -> Triangle<%>
    ; Returns new triangle by checking if mouse event is within triangle
    (define (checkMove-BU x y)
      (local
        ((define vertices (compute-corners center corner)))
        
        (if (and (prevPosnInitialised?)
                 (in-triangle? (make-posn x y) (first vertices) 
                               (second vertices) (third vertices)))
            (new Triangle% [center (newPosn x y center)] 
                 [corner (newPosn x y corner)] 
                 [new? new?] [prevPosn INIT-PREV-POSN])
            this)))
    
    ; newPosn: Coordinate Coordinate Posn -> Posn
    ; Returns new position based on the prevPosn
    (define (newPosn x y position)
      (local
        (; newX: -> Coordinate
         ; Returns new x value for given posn
         ; Strategy : data decomposition on position, prevPosn : Posn
         (define (newX) (if (= x (posn-x prevPosn)) (posn-x position) 
                            (+ (posn-x position) (- x (posn-x prevPosn)))))
         
         ; newY: -> Coordinate
         ; Returns new y value for given posn
         ; Strategy : data decomposition on position, prevPosn : Posn
         (define (newY) (if (= y (posn-y prevPosn)) (posn-y position) 
                            (+ (posn-y position) (- y (posn-y prevPosn))))))
        
        (make-posn (newX) (newY))))
    
    ; prevPosnInitialised?: -> Boolean
    ; Returns true if prvPosn has values other than zero
    ; Strategy : data decomposition on prevPosn : Posn
    (define (prevPosnInitialised?)
      (not (and (zero? (posn-x prevPosn)) (zero? (posn-y prevPosn)))))
    
    ; withinCtrlReg?: Coordinate Coordinate Posn -> Boolean
    ; Returns true if given coordinates are within control region of given posn
    ; Strategy : data decomposition on vertex : Posn
    (define (withinCtrlReg? x y vertex)
      (and (<= (- (posn-x vertex) CONTROL_POINT) x 
               (+ (posn-x vertex) CONTROL_POINT))
           (<= (- (posn-y vertex) CONTROL_POINT) y
               (+ (posn-y vertex) CONTROL_POINT))))             
    
    ; draw : Image -> Image
    ; Renders triangle on the given image
    (define/public (draw image)
      (if new?
          (render-equilateral-triangle center corner 50 "red" image)
          (render-equilateral-triangle center corner "outline" "black" image))) 
    
    (super-new)))

;===============================================================================
; Triangle-Testing
(define TRI-TEST (new Triangle% [center (make-posn 100 100)] 
                      [corner (make-posn 100 60)] [new? #true] 
                      [prevPosn (make-posn 0 0)]))

(begin-for-test
  (check-equal?
   (get-field 
    corner 
    (send (send 
           (send (send 
                  (send (send 
                         (send (send
                                (send (send
                                       (send 
                                        (send 
                                         (send 
                                          TRI-TEST handle-mouse 
                                          150 150 "button-up")
                                         handle-mouse 98 98 "button-down")
                                        handle-mouse 103 103 "drag")
                                       handle-mouse 105 105 "button-up")
                                      handle-mouse 152 153 "button-down") 
                                handle-mouse 155 155 "drag")
                               handle-mouse 155 155 "button-up")
                         handle-mouse 106 156 "button-down")
                        handle-mouse 103 153 "drag")
                  handle-mouse 100 155 "button-up")
                 handle-mouse 156 103 "button-down")
           handle-mouse 157 104 "drag")
          handle-mouse 155 100 "button-up"))
   (make-posn 155 155)
   "Triangle test cases")
  
  (check-equal?
   (get-field 
    corner 
    (send (send 
           (send (send 
                  (send (send 
                         (send (send
                                (send (send
                                       (send 
                                        (send 
                                         (send 
                                          TRI-TEST handle-mouse 
                                          140 150 "button-up")
                                         handle-mouse 98 100 "button-down")
                                        handle-mouse 110 110 "drag")
                                       handle-mouse 115 115 "button-up")
                                      handle-mouse 152 153 "button-down") 
                                handle-mouse 155 155 "drag")
                               handle-mouse 155 155 "button-up")
                         handle-mouse 106 156 "button-down")
                        handle-mouse 103 153 "drag")
                  handle-mouse 100 155 "button-up")
                 handle-mouse 156 103 "button-down")
           handle-mouse 157 104 "drag")
          handle-mouse 155 100 "button-up"))
   (make-posn 160 167)
   "Resize")
  
  (check-equal?
   (get-field 
    corner (send (send (send (send  TRI-TEST handle-mouse 100 60 "drag")
                             handle-mouse 150 150 "button-up")
                       handle-mouse 152 152 "button-down")
                 handle-mouse 80 70 "drag"))
   (make-posn 80 70)
   "Reversing triangle")
  
  (check-equal?
   (send (send TRI-TEST handle-mouse 110 100 "button-up") draw MT)
   (render-equilateral-triangle 
    (make-posn 100 100) (make-posn 110 100) "outline" "black" MT)
   "Triangle rendering")
  
  (check-equal?
   (send TRI-TEST draw MT)
   (render-equilateral-triangle (make-posn 100 100) (make-posn 100 60)
                                50 "red" MT)
   "New Triangle rendering"))

;===============================================================================

; handle-mouse : World Coordinate Coordinate MouseEvent -> World
; GIVEN: A World, mouse coordinates, and a MouseEvent
; RETURNS: A new World, like the given one, updated to reflect the action of
;    the mouse event, in the ways specified in the problem set.
; Strategy : data decomposition on me : MouseEvent
(begin-for-test
  (check-equal? (handle-mouse INITIAL-WORLD 5 5 "button-down")
                (make-world '() 'point) 
                "Pointer selection")
  (check-equal? (handle-mouse INITIAL-WORLD 5 25 "button-down")
                (make-world '() 'rect) 
                "Rectangle selection")
  (check-equal? (handle-mouse INITIAL-WORLD 5 45 "button-down")
                (make-world '() 'circ) 
                "Circle selection")
  (check-equal? (handle-mouse INITIAL-WORLD 40 40 "button-down")
                (make-world '() 'point) 
                "Button down on pointer")
  (check-equal? (handle-mouse INITIAL-WORLD 5 5 "button-up")
                (make-world '() 'point) 
                "Button up on pointer")
  (check-equal? (handle-mouse INITIAL-WORLD 5 5 "drag")
                (make-world '() 'point) 
                "Drag on pointer")
  (check-equal? (handle-mouse INITIAL-WORLD 5 5 "move")
                (make-world '() 'point) 
                "Other mouse events"))

(define (handle-mouse w x y me)
  (cond
    [(string=? "button-down" me) (handleButtonDown x y w)]
    [(string=? "button-up" me) (handleButtonUp x y w)]
    [(string=? "drag" me) (handleDrag x y w)]
    [else w]))

;===============================================================================

; handleButtonDown: Coordinate Coordinate World -> World
; Returns new world on handling button-down event
; Strategy : data decomposition on w : World
(begin-for-test
  (check-equal? (handleButtonDown  5 5 INITIAL-WORLD)
                (make-world '() 'point) 
                "Pointer selection"))

(define (handleButtonDown x y w)
  (if (and (< 0 x TL-SIZE) (< 0 y (* 4 TL-SIZE)))
      (updateTool y w)
      (newWorldOnBD x y (world-selTool w) (world-shapes w))))

;===============================================================================

; updateTool: Coordinate World -> World
; Returns new world by updating toolbar based on selection
; Stratey: data decmposition on w : World
(begin-for-test
  (check-equal? (updateTool 5 INITIAL-WORLD)
                (make-world '() 'point) 
                "Pointer selection"))

(define (updateTool y w)
  (if (< 0 y TL-SIZE) (make-world (world-shapes w) POINT)
      (checkForShapesSel y w)))

;===============================================================================

; checkForShapesSel: Coordinate World -> World
; Returns new world by updating toolbar based on selection
; Stratey: data decmposition on w : World
(begin-for-test
  (check-equal? (checkForShapesSel 45 INITIAL-WORLD)
                (make-world '() 'circ) 
                "Circle selection"))

(define (checkForShapesSel y w)
  (if (< TL-SIZE y (* 2 TL-SIZE))
      (make-world (world-shapes w) RECT)
      (checkForC/T y w)))

; checkForC/T: Coordinate World -> World
; Returns new world by updating toolbar based on selection
; Stratey: data decmposition on w : World
(begin-for-test
  (check-equal? (checkForC/T 65 INITIAL-WORLD)
                (make-world '() 'tri) 
                "Triangle selection"))

(define (checkForC/T y w)
  (if (< (* 2 TL-SIZE) y (* 3 TL-SIZE))
      (make-world (world-shapes w) CIRC)
      (make-world (world-shapes w) TRI)))

;===============================================================================
; newWorldOnBD: Coordinate Coordinate SelTool ListOf<Shape<%>> -> World
; Returns new world by updating shapes based on button-down event
; Stratey: data decmposition on sel : SelTool
(begin-for-test
  (check-equal? (length (world-shapes (newWorldOnBD 100 100 RECT '())))
                1
                "Updated Shapes list")
  (check-equal? (length (world-shapes (newWorldOnBD 100 100 CIRC '())))
                1
                "Updated Shapes list")
  (check-equal? (length (world-shapes (newWorldOnBD 100 100 TRI '())))
                1
                "Updated Shapes list"))

(define (newWorldOnBD x y sel los)
  (cond
    [(point? sel) (make-world (handleShapesOnME x y "button-down" los)
                              sel)]
    [(rect? sel) (make-world 
                  (cons (new Rectangle% [left x] [top y] [right x] 
                             [bottom y] [prevPosn INIT-PREV-POSN] 
                             [new? #true]) los) sel)]
    [(circ? sel) (make-world 
                  (cons (new Circle% [radius 0] 
                             [center (make-posn x y)] 
                             [new? #true] [prevPosn INIT-PREV-POSN]) 
                        los) sel)]
    [(tri? sel) (make-world 
                 (cons (new Triangle% [center (make-posn x y)] 
                            [corner (make-posn x y)]
                            [new? #true] [prevPosn INIT-PREV-POSN]) 
                       los) sel)]))


;===============================================================================

; handleButtonUp: Coordinate Coordinate World -> World
; Returns new world on handling button-up event
; Strategy : data decomposition on w : World
(begin-for-test
  (check-equal? (get-field right (first 
                                  (world-shapes 
                                   (handleButtonUp
                                    120 120 (newWorldOnBD 100 100 RECT '())))))
                120
                "Updated Rectangle on button-up"))

(define (handleButtonUp x y w) 
  (if (and (< 0 x TL-SIZE) (< 0 y (* 4 TL-SIZE))) w
      (make-world (handleShapesOnME x y "button-up" (world-shapes w))
                  (world-selTool w))))

;===============================================================================

; handleDrag: Coordinate Coordinate World -> World
; Returns new world on handling drag event
; Strategy : data decomposition on w : World
(begin-for-test
  (check-equal? (get-field right (first 
                                  (world-shapes 
                                   (handleDrag 
                                    120 120 (newWorldOnBD 100 100 RECT '())))))
                120
                "Updated Rectangle on drag"))

(define (handleDrag x y w)
  (if (and (< 0 x TL-SIZE) (< 0 y (* 4 TL-SIZE))) w
      (make-world (handleShapesOnME x y "drag" (world-shapes w)) 
                  (world-selTool w))))

;===============================================================================

; handleShapesOnME: Coordinate Coordinate MouseEvent ListOf<Shape<%>> 
;                      -> ListOf<Shape<%>>
; Returns new list of Shapes by handling given mouse event in each shape
; Strategy: function composition
(define (handleShapesOnME x y me los)
  (map (λ (shape) (send shape handle-mouse x y me)) los))

;===============================================================================

; render: World -> World
; Renders the given world
; Strategy : data decomposition on w : World
(begin-for-test
  (check-equal? (render (make-world '() 'point))
                (overlay/align 
                 "left" "top" 
                 (overlay/offset 
                  POINTER-SEL 0 40
                  (overlay/offset RECTANGLE 0 30 
                                  (overlay/offset CIRCLE 0 20 TRIANGLE))) MT)
                "INITIAL-WORLD render")
  (check-equal? (render (make-world (list RECT-TEST) 'rect))
                (send RECT-TEST draw 
                      (overlay/align 
                       "left" "top" 
                       (overlay/offset 
                        POINTER 0 40
                        (overlay/offset 
                         RECTANGLE-SEL 0 30 
                         (overlay/offset CIRCLE 0 20 TRIANGLE))) MT))
                "Rectangle render")
  (check-equal? (render (make-world (list CIRC-TEST) 'circ))
                (send CIRC-TEST draw 
                      (overlay/align 
                       "left" "top" 
                       (overlay/offset
                        POINTER 0 40
                        (overlay/offset 
                         RECTANGLE 0 30 
                         (overlay/offset CIRCLE-SEL 0 20 TRIANGLE))) MT))
                "Circle render"))

(define (render w)
  (render-shapes
   (world-shapes w)
   (overlay/align "left" "top" (render-toolbar (world-selTool w)) MT)))

;===============================================================================

; render-toolbar: SelTool -> Image
; Returns toolbar based on selection
; Stratey: data decmposition on sel : SelTool
(begin-for-test
  (check-equal? (render-toolbar 'tri)
                (overlay/offset 
                 POINTER 0 40
                 (overlay/offset RECTANGLE 0 30 
                                 (overlay/offset CIRCLE 0 20 TRIANGLE-SEL)))
                "rendering toolbar"))

(define (render-toolbar sel)
  (cond
    [(point? sel) (overlay/offset
                   POINTER-SEL 0 40
                   (overlay/offset RECTANGLE 0 30 
                                   (overlay/offset CIRCLE 0 20 TRIANGLE)))]
    [(rect? sel) (overlay/offset 
                  POINTER 0 40
                  (overlay/offset RECTANGLE-SEL 0 30 
                                  (overlay/offset CIRCLE 0 20 TRIANGLE)))]
    [(circ? sel) (overlay/offset 
                  POINTER 0 40
                  (overlay/offset RECTANGLE 0 30 
                                  (overlay/offset CIRCLE-SEL 0 20 TRIANGLE)))]
    [(tri? sel) (overlay/offset 
                 POINTER 0 40
                 (overlay/offset RECTANGLE 0 30 
                                 (overlay/offset CIRCLE 0 20 TRIANGLE-SEL)))]))

;===============================================================================

; render-shapes: ListOf<Shape<%>> Image -> Image
; REnders the list of shapes in given los
; Stratey: data decmposition on los : ListOf<Shape<%>>
(begin-for-test
  (check-equal? (render-shapes (list RECT-TEST) MT)
                (send RECT-TEST draw MT)
                "Rectangle render"))

(define (render-shapes los image)
  (cond
    [(empty? los) image]
    [else (render-shapes (rest los) (send (first los) draw image))]))

;===============================================================================

; get-world-shapes : World -> ListOf<Shape<%>>
; GIVEN: A World,
; RETURNS: All the Shape<%>s which make up that world, i.e. all those that
;    have been created by the user through using the tools.
; STRATEGY : data decomposition on w : World
(begin-for-test
  (check-equal? (get-world-shapes (make-world (list RECT-TEST) 'rect))
                (list RECT-TEST)
                "Returns Shapes list"))

(define (get-world-shapes w)
  (world-shapes w))

;===============================================================================

; create-circle : Posn Integer -> Shape<%>
; GIVEN: A center point and a radius
; RETURNS: A new Circle% object (implementing Shape<%>) with its center at
;    the given point and radius as given.
(begin-for-test
  (check-equal? (get-field radius (create-circle (make-posn 100 100) 10))
                10
                "New circle formed"))

(define (create-circle cen rad)
  (new Circle% [radius rad] [center cen] 
       [new? #true] [prevPosn INIT-PREV-POSN]))

;===============================================================================

; create-rectangle : BoundingBox -> Shape<%>
; GIVEN: A bounding box,
; RETURNS: A new Rectangle% object (implementing Shape<%>) which is bounded
;    by the given BoundingBox.
; STRATEGY : data decomposition on bBox : BoundingBox
(begin-for-test
  (check-equal? (get-field new? (create-rectangle (list 100 100 110 110)))
                #true
                "New Rectangle formed"))

(define (create-rectangle bBox)
  (new Rectangle% [left (first bBox)] [top (second bBox)] [right (third bBox)] 
       [bottom (fourth bBox)] [prevPosn INIT-PREV-POSN] [new? #true]))

;===============================================================================
; create-triangle : posn posn -> Shape<%>
; Creates a triangle Shape<%> object from a center
; posn and one arbitrary corner.
(begin-for-test
  (check-equal? (send (create-triangle (make-posn 100 100) 
                                       (make-posn 100 100)) get-bounds)
                '(100 100 100 100)
                "New triangle formed"))

(define (create-triangle center corner) 
  (new Triangle% [center center] [corner corner]
       [new? #true] [prevPosn INIT-PREV-POSN]))

;===============================================================================

; Advantages of OOP 
; 1. Code extensibility is easy. Eg. Adding new Triangle class had all the 
;    functionality  within the class unlike functional programming where the 
;    changes would have been cluttered throught out the file
; 2. Data absraction. Eg. The world doesn't have to know what is a triangle. All
;    operations of a triangle are within the triangle class
; 3. Localisation of error. Helps debugging as it is easy to spot all the 
;    functionalities at a single place.

; Advantages of functional programming
; 1. Data accumulation is easy when needed across all types of itemized data.
;    Eg. expr -> expr/no-var, information needed to convert expr to expr/no-var
;    was easy to be accumulated and used within the same function whereas in OOP
;    this data had to be passed around all the objects
; 2. Code looks compact and concise. It has no local variables as it doesn't
;    have a state and always operates only on the input data which makes 
;    fucntionality testing easy
; 3. One functionality is within one function which makes it easily
;    comprehedable Eg. subst is one function within 
;    which substitution is handled unlike OOP, where the responsibility is 
;    distributed amongst all objects.
