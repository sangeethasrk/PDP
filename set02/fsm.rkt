;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 13)

(provide INITIAL-WORLD)
(provide next-state)
(provide render)
(provide stop?)
(provide accept-state?)
(provide error-state?)

;The Physical Constants

; The FSM World assumes the following states based on the key it recieves as input:
; World is one of:
; - INITIAL-WORLD
; – AA
; – BC
; – DD
; – ER
(define INITIAL-WORLD "")  ; The current world is yet to recieve any input
(define AA "Has recieved an 'a'")
(define BC "Has recieved a 'b' or 'c'")
(define DD "encountered a 'd', final state")
(define ER "error state, invalid input")

; The Graphical Constants

; A FSM-State-Rectangle is a 100 by 100 rectangle image indicating the various states of the machine. It is one of:
; -WHITE-RECTANGLE
; -YELOW-RECTANGLE
; -GREEN-RECTANGLE
; -RED-RECTANGLE
(define WHITE-RECTANGLE (rectangle 100 100 "solid" "white"))
(define YELLOW-RECTANGLE (rectangle 100 100 "solid" "yellow"))
(define GREEN-RECTANGLE (rectangle 100 100 "solid" "green"))
(define RED-RECTANGLE (rectangle 100 100 "solid" "red"))

;The required functions

; run: world -> World (An appropriately coloured rectangle representing the current World state)
; This function uses big-bang, takes the current world state and on recieving a key input from the user it carries out 
; pattern matching and displays an appropriately colored rectangle to represent the current world state 
; and sets the world to the next world state
; The Pattern to be matched is [a (b|c)* d]
(define (run w)
  (big-bang w
            (on-key next-state)
            (to-draw render)
            (stop-when stop?)))

; next-state: World Key -> World
; The function is a key-stroke handler. It returns the next world state based on the key event/input recieved from the user
; For all valid values that matches the pattern [a (b|c)* d] valid world states are returned and for invalid inputs the error state is returned
; STRATEGY: Data Decomposition

(begin-for-test
  (check-equal? (next-state INITIAL-WORLD "a")
                BC
                "The 'next-state' function for the given input failed")
  (check-equal? (next-state AA "a")
                ER
                "The 'next-state' function for the given input failed")
  (check-equal? (next-state BC "b")
                BC
                "The 'next-state' function for the given input failed")
  (check-equal? (next-state BC "c")
                BC
                "The 'next-state' function for the given input failed")
  (check-equal? (next-state BC "r")
                ER
                "The 'next-state' function for the given input failed")
  (check-equal? (next-state BC "d")
                DD
                "The 'next-state' function for the given input failed")
  (check-equal? (next-state ER "a")
                ER
                "The 'next-state' function for the given input failed"))

(define (next-state w ke)
     (cond
       [(and (key=? ke "a") (string=? w INITIAL-WORLD)) BC]
       [(and (or (key=? ke "b") (key=? ke "c")) (or (string=? w AA) (string=? w BC))) BC]
       [(and (key=? ke "d") (string=? w BC)) DD]
       [else ER]))
 
; render: World -> WorldState 
; World State is a colored Rectangle that represents the corresponding state of the World
; STRATEGY: Data Decomposition

(begin-for-test
  (check-equal? (render INITIAL-WORLD)
                WHITE-RECTANGLE
                "The 'render' function for the current world state failed")
  (check-equal? (render AA)
                YELLOW-RECTANGLE
                "The 'render' function for the current world state failed")
  (check-equal? (render BC)
                YELLOW-RECTANGLE
                "The 'render' function for the current world state failed")
  (check-equal? (render DD)
                GREEN-RECTANGLE
                "The 'render' function for the current world state failed")
  (check-equal? (render ER)
                RED-RECTANGLE
                "The 'render' function for the current world state failed"))

(define (render w)
  (cond 
    [(string=? w INITIAL-WORLD) WHITE-RECTANGLE]
    [(or (string=? w AA) (string=? w BC)) YELLOW-RECTANGLE]
    [(string=? w DD) GREEN-RECTANGLE]
    [(string=? w ER) RED-RECTANGLE]))

; stop: World -> Boolean
; This function identifies the stopping condition on when the big-bang should cease executing
; the function returns true when the State of the FSM i.e the current state of the World is 'DD' which is its final state
; STRATEGY: Data Decomposition

(begin-for-test
  (check-equal? (stop? INITIAL-WORLD)
                #false
                "The 'stop?' function for the current world state failed")
  (check-equal? (stop? AA)
                #false
                "The 'stop?' function for the current world state failed")
  (check-equal? (stop? BC)
                #false
                "The 'stop?' function for the current world state failed")
  (check-equal? (stop? DD)
                #true
                "The 'stop?' function for the current world state failed")
  (check-equal? (stop? ER)
                #false
                "The 'stop?' function for the current world state failed"))

(define (stop? w)
  (string=? w DD))

; accept-state?: World -> Boolean
; This function returns a boolean value based on its current world state
; If the current world state is one of the valid FSM states then the value true is returned else false is returned
; STRATEGY: Data Decomposition

(begin-for-test
  (check-equal? (accept-state? INITIAL-WORLD)
                #true
                "The 'accept-state?' function for the current world state failed")
  (check-equal? (accept-state? AA)
                #true
                "The 'accept-state?' function for the current world state failed")
  (check-equal? (accept-state? BC)
                #true
                "The 'accept-state?' function for the current world state failed")
  (check-equal? (accept-state? DD)
                #true
                "The 'accept-state?' function for the current world state failed")
  (check-equal? (accept-state? ER)
                #false
                "The 'accept-state?' function for the current world state failed"))

(define (accept-state? w)
  (cond
    [(or (string=? w INITIAL-WORLD)(string=? w AA) (string=? w BC) (string=? w DD)) #true]
    [else #false]))

; error-state?: World -> Boolean
; This function returns a boolean value based on its current world state
; If the current world state is one of the valid FSM states then the value false is returned else true is returned
; STRATEGY: Data Decomposition

(begin-for-test
  (check-equal? (error-state? INITIAL-WORLD)
                #false
                "The 'error-state?' function for the current world state failed")
  (check-equal? (error-state? AA)
                #false
                "The 'error-state?' function for the current world state failed")
  (check-equal? (error-state? BC)
                #false
                "The 'error-state?' function for the current world state failed")
  (check-equal? (error-state? DD)
                #false
                "The 'error-state?' function for the current world state failed")
  (check-equal? (error-state? ER)
                #true
                "The 'error-state?' function for the current world state failed"))

(define (error-state? w)
  (cond
    [(string=? w ER) #true]
    [else #false]))

(run INITIAL-WORLD)