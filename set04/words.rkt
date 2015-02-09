;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname words) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 20)

(provide arrangements)
(provide insert-everywhere/in-all-words)
(provide arrangement-main)

; empty-list is an empty list 
; INTERP: Refers to a list that contains no elements in it.
(define EMPTY-LIST '())

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

; STRATEGY: Data Decomposition on w : Word
(define (arrangements w)
  (cond
    [(empty? w) EMPTY-LIST]
    [else
     (if (empty? (rest w)) 
         (cons (first w) EMPTY-LIST) 
         (insert-everywhere/in-all-words (first w) (arrangements (rest w))))]))

(define (insert-everywhere/in-all-words s lw)
  (cond
    [(empty? lw) EMPTY-LIST]
    [else (if (empty? (rest lw)) 
              (insert-everywhere-in-single-word s (cons (first lw) '()))
              (append (insert-everywhere-in-single-word s (first lw))
                      (insert-everywhere/in-all-words s (rest lw))))]))

(define (insert-everywhere-in-single-word s w)
  (cond 
    [(empty? w) EMPTY-LIST]
    [else (if (empty? (rest w))
              (cons (insert-at-beginning s (cons w '()))
                (cons (insert-in-between s (cons w '()))
                      (cons (insert-at-end s (cons w '())) EMPTY-LIST)))
              (cons (insert-at-beginning s w)
                (cons (insert-in-between s w)
                      (cons (insert-at-end s w) EMPTY-LIST))))]))

(define (insert-at-beginning s w)
  (cond
    [(empty? w) EMPTY-LIST]
    [else (append (cons s EMPTY-LIST) w)]))

(define (insert-at-end s w)
  (cond 
    [(empty? w) EMPTY-LIST]
    [else (append w (cons s EMPTY-LIST))]))

(define (insert-in-between s  w )
  (cond
    [(empty? (rest w)) EMPTY-LIST]
    [else (insert (cons (first w) EMPTY-LIST) s (rest w))]))

(define (insert l s w)
  (cond
    [(empty? w) EMPTY-LIST]
    [else (if (empty? (rest w))
              (append (append l (cons s EMPTY-LIST)) (rest w))
              (append 
               (append (append l (cons s EMPTY-LIST)) (rest w))
               (insert (append l (cons (first w) EMPTY-LIST)) s (rest w))))]))

(define (implode-each l)
  (cond
    [(empty? l) EMPTY-LIST]
    [else (if 
           (= (string-length (implode (first l))) 0) 
           (implode-each (rest l))
           (append 
            (cons (implode (first l)) EMPTY-LIST) 
            (implode-each (rest l))))]))

(define (arrangement-main str)
  (arrangements (explode str)))

(begin-for-test 
  (check-equal? (arrangement-main "ab")
                (list "ab" "ba")))
;                