;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(define TIME-ON-TASK 13)

;Exercise 13
;To calculate the distance of a cartesian point from the origin

;distance : Coordinate Coordinate -> NonNegReal
;A Coordinate is a Real.
;INTERP: 
;Represents either an x or y pixel coordinate on a Universe (big-bang)
;canvas, where (0,0) is in the upper-left corner, and right and down are
;the positive directions.
;The function computes the square root of the sum of squares of the input coordinates and
;Returns the distance between the point and the origin on a plain
(begin-for-test
  (check-equal? (distance 0 0) 
                0
                "'distance' function failed")
  (check-equal? (distance 12 5)
                13
                "'distance' function failed")
  (check-equal? (distance -3 4)
                5
                "'distance' function failed")
  (check-equal? (distance -12 -5)
                13
                "'distance' function failed")
  (check-equal? (distance 3 -4)
                5
                "'distance' function failed"))

(define (distance x y)
         (sqrt(+ (sqr x) (sqr y))))

;Exercise 14 A
;To calculate the volume of a cube when the length of one side is given

;cubeVolume : length -> positiveValue
;A lengh is a positive Real number.Represents the length of one side of the cube
;The function computes the volume of the cube in cubic units by cubing the given length
(begin-for-test
  (check-equal? (cubeVolume 0) 
                0
                "volume of cube with side 0")
  (check-equal? (cubeVolume -1) 
                -1
                "volume of cube with side -1 cannot be calculated")
  (check-equal? (cubeVolume 3) 
                27
                "volume of cube with side 3")
  (check-equal? (cubeVolume 10) 
                1000
                "volume of cube with side 10"))

(define (cubeVolume x)
  (* (sqr x) x))

;Exercise 14 B
;To calculate the surface area/ cube-surface of a cube when the length of one side is given

;cubeSurface : length -> positiveValue
;A lengh is a positive Real number. Represents the length of one side of the cube
;The function computes the cube-surface area in square units using the given length in the cube surface area expression
(begin-for-test
  (check-equal? (cubeSurface 0) 
                0
                "surface of cube with side 0")
  (check-equal? (cubeSurface 3) 
                54
                "surface of cube with side 3")
  (check-equal? (cubeSurface 10) 
                600
                "volume of cube with side 10"))

(define (cubeSurface x)
  (* 6 (sqr x)))

;Exercise 15
;To extract the first character from a non-empty string.

;string-first : inputString -> character
;An inputString is a String. 
;The function prints the first character of the inputString
(begin-for-test
  (check-equal? (string-first "Geeth") 
                "G"
                "The first character of the string Geeth is 'G'. 'string-first function failed'")
  (check-equal? (string-first "test") 
                "t"
                "The first character if the string test is t. 'string-first' function failed"))

(define (string-first inputString)
  (string-ith inputString 0))

;Exercise 16
;To extract the last character from a non-empty string.

;string-last : inputStr -> character
;An inputStr is a String
;The function prints the last character of the inputStr.
(begin-for-test
  (check-equal? (string-last "Geeth") 
                "h"
                "The last character of the string Geeth is h. 'string-last' function failed")
  (check-equal? (string-last "program") 
                "m"
                "The last character if the string program is m. 'string-last' function failed"))

(define (string-last inputStr)
  (string-ith inputStr (- (string-length inputStr) 1)))

;Exercise 17
;To have a function that uses two input Boolean values and returns #true if input1 is false or input2 is true.

;bool-imply : Boolean, Boolean -> Boolean
;b1 and b2 are the two input boolean values
;The function returns true if b1 is true or if b2 is false
(begin-for-test
  (check-equal? (bool-imply (= (string-length "pdp") 3) (= 1 1)) 
                "true"
                "bool-imply function failed")
  (check-equal? (bool-imply (= 1 3) (= 1 2)) 
                "false"
                "bool-imply function failed"))

(define (bool-imply b1 b2)
  (cond [ (not b1) "false"]
        [ b2 "true"]))

;Exercise 18
;To count the number of pixels in an image.

;image-area : inputImage -> nonNegativeRealNumber
;inputImage is an Image
;The function returns the number of pixels in the input image

(begin-for-test
  (check-equal? (image-area(bitmap  "cat1.png"))
                8968
                "'image-area' function failed"))
   
(define (image-area inputImage)
      (* (image-width inputImage) (image-height inputImage)))

;Exercise 19
;To display an image as tall, wide or square based on its dimensions

;image-classify : inputImg -> String
;inputImg is an Image
;the function returns a string; 'tall', 'wide' or 'square' based on the image dimensions

(begin-for-test
  (check-equal? (image-classify (bitmap "cat1.png"))
                "tall"
                "'image-classify' function failed")
  (check-equal? (image-classify (rectangle 10 5 "solid" "red"))
                "wide"
                "'image-classify' function failed")
  (check-equal? (image-classify (rectangle 6 6 "solid" "green"))
                "square"
                "'image-classify' function failed"))

(define (image-classify inputImg)
  (cond 
    [(< (image-height inputImg) (image-width inputImg)) "wide"]
    [(> (image-height inputImg) (image-width inputImg)) "tall"]
    [(= (image-height inputImg) (image-width inputImg)) "square"]))


;Exercise 20
;To join to strings with a '-' between them

;string-join : prefix, suffix -> resultString
;prefix is a String
;suffix is a String
;The function consumes the prefix and suffix strings and appends a '-' between them

(begin-for-test
  (check-equal? (string-join "hello" "world")
                "hello-world"
                "'string-join' function failed"))

(define (string-join prefix suffix)
      (string-append prefix "-" suffix))

;Exercise 21 
;To insert a "-" at the ith position in an input string

;string-insert : ipstring, i -> resultString
;ipString is the input string
;i is the ith position in the string where the "-" has to be inserted
;The functions returns a string with a "-" inserted at the ith position.

(begin-for-test
  (check-equal? (string-insert "hello" 2)
                "he-llo"
                "'string-insert' function failed"))

(define (string-insert ipString i)
    (string-append (substring ipString 0 i) "-" (substring ipString i)))

;Exercise 22
;To omit a character at the ith position in the string

;string-delete : str, i -> resultString
;str is the input string
;i is the position on the input string from where the character is to be omitted
;The function returns a string with the character at the ith position omitted

(begin-for-test
  (check-equal? (string-delete "hello" 2)
                "helo"
                "'string-delete' function failed"))

(define (string-delete str i)
    (string-append (substring str 0 i) (substring str (+ i 1))))