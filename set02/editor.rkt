;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 13)

(provide render)
(provide edit)
(provide string->editor)
(provide editor-pre)
(provide editor-post)
(provide editor-pos)

; graphical constants

; MT is an empty-scene
(define MT (empty-scene 200 20))

; CURSOR is an image
; INTERP: It is used to indicate the cursor of the text editor
(define CURSOR (rectangle 1 20 "solid" "red"))

; Physical constants

; An editor is a (make-editor String String)
; INTERP: (make-editor pre post) means the text in the editor is
; (string-append pre post) with the cursor displayed between pre and post
(define-struct editor [preString postString])

; render: editor -> Image
; This function consumes an editor and renders it as an image with the current location of the cursor on the text as well.
(begin-for-test
  (check-equal? (render (make-editor "hello" "world"))
                (overlay/align "left" "center"
                               (beside (text "hello" 16 "black") 
                                       CURSOR
                                       (text "world" 16 "black"))
                               MT)
                "'render' function failed"))

(define (render ed)
  (overlay/align "left" "center"
               (beside (text (editor-preString ed) 16 "black") 
                       CURSOR
                       (text (editor-postString ed) 16 "black"))
               MT))

; edit: editor KeyEvent -> editor
; This function helps carry out the edit operations on the one-line editor
(begin-for-test
  (check-equal? (edit (make-editor "hello" "world") "\b") 
                (make-editor "hell" "world")
                "Function 'edit' failed")
  (check-equal? (edit (make-editor "hello" "world") ".") 
                (make-editor "hello." "world")
                "Function 'edit' failed")
  (check-equal? (edit (make-editor "hell" "world") "a") 
                (make-editor "hella" "world")
                "Function 'edit' failed")
  (check-equal? (edit (make-editor "hello" "world") "right") 
                (make-editor "hellow" "orld")
                "Function 'edit' failed")
  (check-equal? (edit (make-editor "hello" "world") "left") 
                (make-editor "hell" "oworld")
                "Function 'edit' failed")
  (check-equal? (edit (make-editor "h" "world") "\b") 
                (make-editor "" "world")
                "Function 'edit' failed")
  (check-equal? (edit (make-editor "hello" "world") " ") 
                (make-editor "hello " "world")
                "Function 'edit' failed")
  (check-equal? (edit (make-editor "hello" "world") "\t") 
                (make-editor "hello" "world")
                "Function 'edit' failed")
  (check-equal? (edit (make-editor "hello" "world") "\u007F") 
                (make-editor "hello" "world")
                "Function 'edit' failed"))

(define (edit ed ke)
  (cond  
    [(string=? ke "\b") (make-editor (string-remove-last (editor-preString ed)) (editor-postString ed))]
    [(string=? ke "left") (make-editor (string-remove-last (editor-preString ed)) (string-append (string-last (editor-preString ed)) (editor-postString ed)))]
    [(string=? ke "right") (make-editor (string-append (editor-preString ed) (string-first (editor-postString ed))) (string-rest (editor-postString ed)))]
    [(or (string=? ke "\t") (string=? ke "\u007F")) (make-editor (editor-preString ed) (editor-postString ed))]
    [else (make-editor (string-append (editor-preString ed) ke) (editor-postString ed))]))

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

;string-remove-last : str -> resultString
;str is the input string
;The function returns a string with its last character omitted
(begin-for-test
  (check-equal? (string-remove-last "hello")
                "hell"
                "'string-remove-last' function failed"))

(define (string-remove-last str)
    (string-append (substring str 0 (- (string-length str) 1))))

;string-rest : str -> resultString
;str is the input string
;The function returns a string with its first character omitted
(begin-for-test
  (check-equal? (string-rest "hello")
                "ello"
                "'string-rest' function failed"))

(define (string-rest str)
    (string-append (substring str 1 (string-length str))))

; run: editor -> Image
; The function consumes an editor. on key vents the edit function is called and the editor's pre string 
; is consumed as input and edited. the resulting changed editor is consumed by the auxillary function, 
; 'render' and the resulting text is rendered as an image.
(define (run ed)
  (big-bang ed
            (on-key edit)
            (to-draw render)))

; string->editor : String -> Editor
; Returns an Editor containing text str and cursor at position 0.
(begin-for-test
  (check-equal? (string->editor "hello")
                (make-editor "" "hello")
                "'string->editor' function failed"))

(define (string->editor str)
  (make-editor "" str))

; editor-pre : Editor -> String
; Returns the text in editor e before the cursor.
(begin-for-test
  (check-equal? (editor-pre (make-editor "hello" "world"))
                "hello"
                "'editor-pre' function failed"))

(define (editor-pre e) 
  (editor-preString e))

; editor-post : Editor -> String
; Returns the text in editor e after the cursor.
(begin-for-test
  (check-equal? (editor-post (make-editor "hello" "world"))
                "world"
                "'editor-post' function failed"))

(define (editor-post e)
  (editor-postString e))

; editor-pos : Editor -> Natural
; Returns the position of the cursor in editor e.
(begin-for-test
  (check-equal? (editor-pos (make-editor "hello" "world"))
                5
                "'editor-post' function failed"))

(define (editor-pos e) 
  (string-length (editor-preString e)))