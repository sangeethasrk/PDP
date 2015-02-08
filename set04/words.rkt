;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname words) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 13)

(provide arrangements)
;(provide insert-everywhere/in-all-words)
;(provide arrangement-main)

; empty-list is an empty list 
; INTERP: Refers to a list that contains no elements in it.
(define EMPTY-LIST (list '()))

; A 1String is a string of length 1
; WHERE: the inputs are only alphabetic characters from a to z
; INTERP: represents a single letter 

; A Word is either 
; – '() or
; – (cons 1String Word)
; INTERP: Word represents a list of characters

; A List-of-words is either 
; – '() or
; – (cons Word List-of-words)
; INTERP: List-of-words represents a list of words

; TEMPLATE:
; arrangements: Word -> List-of-words
; creates a list of all rearrangements of the letters in w
;(define (arrangements w)
;  (cond
;    [(empty? w) ...]
;    [else (... (first w) ... (arrangements (rest w)) ...)]))
; EXAMPLES: 
(begin-for-test
  (check-equal? (arrangements (list "d" "e"))
                (cons "de" (cons "ed" '()))))
; STRATEGY: Data Decomposition on w : Word
(define (arrangements w)
  (cond
    [(empty? w) EMPTY-LIST]
    [else (insert-everywhere/in-all-words (first w)
            (arrangements (rest w)))]))

(define (insert-everywhere/in-all-words s lw)
  (cond
    [(empty? lw) (list '())]
    [else (append (insert-everywhere-in-single-word s (first lw))
            (insert-everywhere/in-all-words s (rest lw)))]))

(define (insert-everywhere-in-single-word s w)
  (cond 
    [(empty? w) (list '())]
    [else (append (insert-at-beginning s w) 
                  (insert-in-between s w) 
                  (insert-at-end s w))]))

(define (insert-at-beginning s w)
  (cond
    [(empty? w) EMPTY-LIST]
    [else (append (cons s '()) w)]))

(define (insert-at-end s w)
  (cond 
    [(empty? w) EMPTY-LIST]
    [else (append w (cons s '()))]))

(define (insert-in-between s  w )
  (cond
    [(empty? (rest w)) EMPTY-LIST]
    [else (cons (append (cons (first w) '()) (cons s '()) (rest w))
                (cons 
                 (append (cons (first w) '()) (infront (rest w) s)) '()))]))
    
(define (infront w s)
  (cond 
    [(empty? w) EMPTY-LIST]
    [else (insert-in-between s w)]))