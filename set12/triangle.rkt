#lang racket
(require lang/posn)
(require 2htdp/image)

; _____    _                _
;|_   _| _(_)__ _ _ _  __ _| |___ ___
;  | || '_| / _` | ' \/ _` | / -_|_-<
;  |_||_| |_\__,_|_||_\__, |_\___/__/
;                     |___/

; Helpful utilities for working with triangles.

;(provide angle-of)
;(provide angle-between)
;(provide rotate-vec)
(provide subtract-posn)
(provide add-posn)
;(provide scale-posn)
(provide distance)
;(provide QUARTER-CIRCLE)
;(provide HALF-CIRCLE)
;(provide FULL-CIRCLE)

(provide in-triangle?)
(provide render-equilateral-triangle)
(provide compute-corners)
;(provide TRIANGLE-RENDERING-ANGLE)
;(provide pinned-triangle)
;(provide place-image-by-pinhole)

;; compute-corners : Posn Posn -> (list Posn Posn Posn)
;; Computes the corners of an equilateral triangle from a center and one corner.
;; The given corner is included in the output.
(define (compute-corners center corner)
  (let* ([vec (subtract-posn corner center)])
    (list corner
          (add-posn center (rotate-vec vec (* FULL-CIRCLE 1/3)))
          (add-posn center (rotate-vec vec (* FULL-CIRCLE 2/3))))))

; subtract-posn : Posn Posn -> Posn
; GIVEN: Two points, considered "positive" and "negative",
; RETURNS: The difference between those points, componentwise. Considering the
;    result as a vector, the vector points from the negative point to the
;    positive one.
(define (subtract-posn pos-posn neg-posn)
  (make-posn (- (posn-x pos-posn) (posn-x neg-posn))
             (- (posn-y pos-posn) (posn-y neg-posn))))

; add-posn : Posn Posn -> Posn
; GIVEN: Two points
; RETURNS: The sum of those points, componentwise.
(define (add-posn addend-1 addend-2)
  (make-posn (+ (posn-x addend-1) (posn-x addend-2))
             (+ (posn-y addend-1) (posn-y addend-2))))

; scale-posn : Posn Real -> Posn
; GIVEN: A point and a scalar factor
; RETURNS: The point, scaled by the given factor.
(define (scale-posn posn scale)
  (make-posn (* scale (posn-x posn)) (* scale (posn-y posn))))

; distance : Posn Posn : Real
; GIVEN: Two points,
; RETURNS: the scalar distance between them.
(define (distance pt-1 pt-2)
  (let* ([dx (- (posn-x pt-1) (posn-x pt-2))]
         [dy (- (posn-y pt-1) (posn-y pt-2))])
  (sqrt (+ (* dx dx) (* dy dy)))))

(define (vector-magnitude point)
  (distance point (make-posn 0 0)))

; The angle of a quarter of a circle, in radians.
(define QUARTER-CIRCLE (/ pi 2))

; The angle of a half of a circle, in radians.
(define HALF-CIRCLE pi)

; The angle of a full circle, in radians.
(define FULL-CIRCLE (* 2 pi))

; angle-of : Posn -> Radians
; GIVEN: a Posn (representing a vector)
; RETURNS: The angle between the negative x-axis and the vector, measured
;   counterclockwise.
(define (angle-of vector)
  (let* ([mag (vector-magnitude vector)])
    (if (= 0 mag)
        0 ; We don't care about the angle in the zero-magnitude case.
        (let* ([normalized-vector (scale-posn vector (/ 1 mag))]
               [half-angle (acos (posn-x normalized-vector))])
          (if (> (posn-y vector) 0)
              (- FULL-CIRCLE (acos (posn-x normalized-vector)))
              (- HALF-CIRCLE (acos (- (posn-x normalized-vector)))))))))

; angle-between : Posn Posn -> Radians
; GIVEN: two points in the plane
; RETURNS: The angle from the negative x-axis to a vector that runs from
;    point 1 to point 2, and measured counterclockwise.
(define (angle-between point-1 point-2)
  (let* ([point-to-point-vector (subtract-posn point-1 point-2)])
    (angle-of point-to-point-vector)))

; The angular offset between an equilateral triangle's base and its
; center-to-corner line.
(define TRIANGLE-RENDERING-ANGLE (atan (/ -1.0 (sqrt 3))))

; rotate-vec : Posn Radians -> Posn
; GIVEN: a vector (as a Posn) and an angle,
; RETURNS: The vector rotated clockwise by that angle.
(define (rotate-vec vec theta)
  (let* ([x (posn-x vec)]
         [y (posn-y vec)]
         [result-x (- (* x (cos theta)) (* y (sin theta)))]
         [result-y (+ (* x (sin theta)) (* y (cos theta)))])
    (make-posn result-x result-y)))

; in-triangle? : Posn Posn Posn Posn -> Boolean
; GIVEN: One "candidate" point and three corner points,
; RETURNS: true iff the candidate is inside the triangle defined
;   by the three points.
(define (in-triangle? p p1 p2 p3)
  (define v p)
  (define v0 p1)
  (define v1 (subtract-posn p2 p1))
  (define v2 (subtract-posn p3 p1))
  (point-in-interior? v v0 v1 v2))


;; point-in-interior? : Posn Posn Posn Posn -> Boolean
;; Returns true if the point v is in the interior of the
;; triangle with vertex at v0 and edges represented
;; by vectors v1 and v2
(define (point-in-interior? v v0 v1 v2)
  (define d (det v1 v2))
  (define a (- (det v v2) (det v0 v2)))
  (define b (- (det v0 v1) (det v v1)))
  (or (and (= d 0)
           (equal? v v0))
      (and (not (= d 0))
           (<= 0 a d)
           (<= 0 b d)
           (<= 0 (- d a b) d))))


;; det : Posn Posn -> Number
;; Returns the determinant of the 2x2 matrix formed by
;; treating u and v as column vectors and joining them side
;; by side
(define (det u v)
  (define u-x (posn-x u))
  (define u-y (posn-y u))
  (define v-x (posn-x v))
  (define v-y (posn-y v))
  (- (* u-x v-y) (* u-y v-x)))

; pinned-triangle : Real String String -> Image
; GIVEN: A side length, opacity, and color
; RETURNS: an equilateral triangle Image, with its base along the horizontal axis
;    and point upward, and with a pinhole placed in the exact center, that is,
;    the point equidistant from all three corners. The given opacity and color
;    will also be applied.
(define (pinned-triangle side opacity color)
  (local [; The perpendicular distance from the base to the opposite corner
          ; of an equilateral triangle.
          (define height (* side (* 1/2 (sqrt 3))))]
    (put-pinhole (* side 1/2) (* height 2/3)
                 (triangle side opacity color))))

; place-image-by-pinhole : Image Coordinate Coordinate Image -> Image
; GIVEN: An image to place onto a background, some x and y coordinates, and a
;    background Image,
; RETURNS: The foreground image placed with its pinhole at the x, y coordinates
;    onto background.
;
;    Note this lets you set a pinhole in an image and place it directly onto
;    a background, without having to set a pinhole for the background image.
(define (place-image-by-pinhole image x y background)
  (clear-pinhole
   (overlay/pinhole
    image
    (put-pinhole x y background))))

; rotate-by-radians : Radians Image -> Image
; GIVEN: A counterclockwise angle, in radians, and an Image,
; RETURNS: That image, rotated by that angle (counterclockwise for positive
;    radians, of course).
(define (rotate-by-radians radians image)
  (rotate (radians->degrees radians) image))

; render-equilateral-triangle : Posn Posn String String Image -> Image
; GIVEN: A center Posn, an arbitrary corner Posn, an opacity String, a color
;    String, and a background Image,
; RETURNS: An Image like the background but with an equilateral triangle placed
;    onto it, such that its center is as given, one of its corners is at the
;    given corner, and its opacity and color as given. (The "center" is the
;    point that's equidistant from the three corners.)
(define (render-equilateral-triangle center corner opacity color background)
  (local [(define side (* (sqrt 3) (distance center corner)))]
    (place-image-by-pinhole
     (rotate-by-radians
      (+ TRIANGLE-RENDERING-ANGLE
         (angle-between center corner))
      (pinned-triangle side opacity color))
     (posn-x center) (posn-y center) background)))
