;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 24)

(provide edit)
(provide string->editor)
(provide editor-pre)
(provide editor-post)
(provide editor-pos)

; constants 
(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 

; graphical constants 

; MT is an empty-scene 
(define MT (empty-scene WIDTH HEIGHT))
; CURSOR is an image
; INTERP: It is used to indicate the cursor of the text editor
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; A 1String is a string of length 1
; WHERE: the inputs are only alphabetic characters from a to z
; INTERP: represents a single letter 

; An Lo1S is one of: 
; – empty 
; – (cons 1String Lo1S)
; Lo1S is a list of single characters

; An Editor is (make-Editor Lo1S Lo1S)
; INTERP: it is a (make-Editor preString postString)with a cursor between 
; the preString and the postString
(define-struct Editor [preString postString])

; TEMPLATE:
; editor-fn: editor -> ???
; (define (editor-fn ed)
;  (... (Editor-preString ed) ... 
;       (Editor-postString ed) ...))

; run: Editor -> Image
; The function consumes an editor and renders an Image. It launches an 
; interactive editor using the edit and render functions for on-key and to-draw
(define (run ed)
  (big-bang ed
            (on-key edit)
            (to-draw render)))

; render: Editor -> Image
; renders an editor as an image of the two texts separated by the cursor
; EXAMPLES:
(begin-for-test
  (check-equal? (render (make-Editor (list "x" "y" "z") (list "a" "b" "c")))
                (overlay/align "left" "center"
                               (beside (text "xyz" FONT-SIZE FONT-COLOR) 
                                       CURSOR
                                       (text "abc" FONT-SIZE FONT-COLOR))
                               MT)
                "'render' function failed"))
; STRATEGY: Daa Decomposition on ed : Editor
(define (render ed)
  (overlay/align "left" "center"
                 (beside (text (extract-pre ed) FONT-SIZE FONT-COLOR) 
                         CURSOR
                         (text (extract-post ed) FONT-SIZE FONT-COLOR))
                 MT))

; extract-pre: Editor -> String
; extracts the pre list from the editor and renders as a string
; EXAMPLES:
(begin-for-test
  (check-equal? (extract-pre 
                 (make-Editor (list "x" "y" "z") (list "a" "b" "c")))
                "xyz"
                "'extract-pre' function failed"))
; STRATEGY: Data Decomposition on ed : Editor
(define (extract-pre ed)
  (implode (Editor-preString ed)))

; extract-post: Editor -> String
; extracts the post list from the editor and renders as a string
; EXAMPLES:
(begin-for-test
  (check-equal? (extract-post 
                 (make-Editor (list "x" "y" "z") (list "a" "b" "c")))
                "abc"
                "'extract-post' function failed"))
; STRATEGY: Data Decomposition on ed : Editor
(define (extract-post ed)
  (implode (Editor-postString ed)))

; edit: Editor KeyEvent -> Editor
; This function helps carry out the edit operations on the one-line editor
; EXAMPLES:
(begin-for-test
  (check-equal? (edit 
                 (make-Editor (list "x" "y" "z") (list "a" "b" "c")) "\b") 
                (make-Editor (list "x" "y") (list "a" "b" "c"))
                "Function 'edit' failed")
  (check-equal? (edit 
                 (make-Editor (list "x" "y" "z") (list "a" "b" "c")) "a") 
                (make-Editor (list "x" "y" "z" "a") (list "a" "b" "c"))
                "Function 'edit' failed")
  (check-equal? (edit 
                 (make-Editor (list "x" "y" "z") (list "a" "b" "c")) "\t") 
                (make-Editor (list "x" "y" "z") (list "a" "b" "c"))
                "Function 'edit' failed")
  (check-equal? (edit 
                 (make-Editor (list "x" "y" "z") (list "a" "b" "c")) "left") 
                (make-Editor (list "x" "y") (list "z" "a" "b" "c"))
                "Function 'edit' failed")
  (check-equal? (edit 
                 (make-Editor (list "x" "y" "z") (list "a" "b" "c")) "right") 
                (make-Editor (list "x" "y" "z" "a") (list "b" "c"))
                "Function 'edit' failed"))
; STRATEGY: Data Decomposition on ke : KeyEvent
(define (edit ed ke)
  (cond  
    [(key=? ke "\b") (handle-delete ed)]
    [(key=? ke "left") (handle-left-toggle ed)]
    [(key=? ke "right") (handle-right-toggle ed)]
    [(or (key=? ke "\t") (key=? ke "\u007F")) (handle-tab ed)]
    [else (handle-insert ed ke)]))

; handle-delete: Editor -> Editor
; The function handles the backspace key event
; EXAMPLES:
(begin-for-test
  (check-equal? (handle-delete 
                 (make-Editor (list "x" "y" "z") (list "a" "b" "c"))) 
                (make-Editor (list "x" "y") (list "a" "b" "c"))
                "Function 'handle-delete' failed")
  (check-equal? (handle-delete 
                 (make-Editor '() (list "a" "b" "c"))) 
                (make-Editor '() (list "a" "b" "c"))
                "Function 'handle-delete' failed"))
; STRATEGY: Data Decomposition on ed : Editor
(define (handle-delete ed)
  (cond
    [(empty? (Editor-preString ed)) (make-Editor '() (Editor-postString ed))]
    [else (make-Editor 
           (remove-last(Editor-preString ed)) 
           (Editor-postString ed))]))

; remove-last: Lo1S -> Lo1S
; The function consumes a list and renders a list with its last element omitted
; EXAMPLES:
(begin-for-test
  (check-equal? (remove-last (list "x" "y" "z")) 
                (list "x" "y")
                "Function 'remove-last' failed")
  (check-equal? (remove-last '()) 
                '()
                "Function 'remove-last' failed"))
; STRATEGY: Function Composition
(define (remove-last l)
  (cond 
    [(empty? l) '()]
    [else  (explode (string-remove-last (implode l)))]))

; string-remove-last : String -> String
; The function returns a string with its last character omitted
; EXAMPLES: 
(begin-for-test
  (check-equal? (string-remove-last "hello")
                "hell"
                "'string-remove-last' function failed"))
; STRATEGY: Data decomposition on str : String
(define (string-remove-last str)
  (string-append (substring str 0 (- (string-length str) 1))))

; handle-left-toggle: Editor -> Editor
; The function handles the 'left' KeyEvent
; EXAMPLES:
(begin-for-test
  (check-equal? (handle-left-toggle 
                 (make-Editor (list "x" "y" "z") (list "a" "b" "c"))) 
                (make-Editor (list "x" "y") (list "z" "a" "b" "c"))
                "Function 'handle-left-toggle' failed")
  (check-equal? (handle-left-toggle 
                 (make-Editor '() (list "a" "b" "c"))) 
                (make-Editor '() (list "a" "b" "c"))
                "Function 'handle-left-toggle' failed"))
; STRATEGY: Data Decomposition on ed: Editor
(define (handle-left-toggle ed)
  (cond
    [(empty? (Editor-preString ed)) (make-Editor '() (Editor-postString ed))]
    [else (make-Editor 
           (remove-last (Editor-preString ed)) 
           (append (get-last (Editor-preString ed)) (Editor-postString ed)))]))

; get-last: Lo1S -> Lo1S
; The function gets the last element from an input string 
; and will return the result as a list
; EXAMPLES:
(begin-for-test
  (check-equal? (get-last (list "x" "y" "z")) 
                (list "z")
                "Function 'get-last' failed")
  (check-equal? (get-last '()) 
                '()
                "Function 'get-last' failed"))
; STRATEGY: Function Composition
(define (get-last l)
  (cond 
    [(empty? l) '()]
    [else (explode (string-last (implode l)))]))

; string-last : String -> 1String
; The function gets the last character of the input string.
; EXAMPLES:
(begin-for-test
  (check-equal? (string-last "program") 
                "m"
                "'string-last' function failed"))
; STRATEGY: Data Decomposition on inputStr : String
(define (string-last inputStr)
  (string-ith inputStr (- (string-length inputStr) 1)))

; handle-right-toggle: Editor -> Editor
; The function handles the 'left' KeyEvent
; EXAMPLES:
(begin-for-test
  (check-equal? (handle-right-toggle 
                 (make-Editor (list "x" "y" "z") (list "a" "b" "c"))) 
                (make-Editor (list "x" "y" "z" "a") (list "b" "c"))
                "Function 'handle-right-toggle' failed")
  (check-equal? (handle-right-toggle 
                 (make-Editor (list "x" "y" "z") '())) 
                (make-Editor (list "x" "y" "z") '())
                "Function 'handle-right-toggle' failed"))
; STRATEGY: Data Decomposition on ed: Editor
(define (handle-right-toggle ed)
  (cond
    [(empty? (Editor-postString ed)) (make-Editor (Editor-preString ed) '())]
    [else (make-Editor    
           (append (Editor-preString ed) 
                   (cons (first (Editor-postString ed)) '()))
           (rest (Editor-postString ed)))]))

; handle-tab: Editor -> Editor
; The functions handle the KeyEvent of tab and delete
; EXAMPLES:
(begin-for-test
  (check-equal? (handle-tab
                 (make-Editor (list "x" "y" "z") (list "a" "b" "c"))) 
                (make-Editor (list "x" "y" "z") (list "a" "b" "c"))
                "Function 'handle-tab' failed"))
; STRATEGY: Data Decomposition on ed : Editor
(define (handle-tab ed)
  (make-Editor (Editor-preString ed) (Editor-postString ed)))

; handle-insert: Editor Key -> Editor
; Handles insertion of characters after corresponding keys were pressed
; EXAMPLES:
(begin-for-test
  (check-equal? (handle-insert
                 (make-Editor (list "x" "y" "z") (list "a" "b" "c")) "a") 
                (make-Editor (list "x" "y" "z" "a") (list "a" "b" "c"))
                "Function 'handle-insert' failed")
  (check-equal? (handle-insert
                 (make-Editor 
                  (explode "abcdefghijklmnop") 
                  (explode "qrstuvwxyz")) "a") 
                (make-Editor 
                  (explode "abcdefghijklmnop") 
                  (explode "qrstuvwxyz"))
                "Function 'check-if-too-wide?' failed"))
; STRATEGY: Data Decomposition on ed : Editor
(define (handle-insert ed ke)
  (if (check-if-too-wide? ed ke) 
      (handle-tab ed)
      (make-Editor 
       (append (Editor-preString ed) (cons ke '())) 
       (Editor-postString ed))))

; check-if-too-wide? : Editor Key -> Boolean
; Checks if after insertion the text will wrap over the margins of editor
; EXAMPLES:
(begin-for-test 
  (check-equal? (check-if-too-wide? 
                 (make-Editor (list "x" "y" "z") (list "a" "b" "c")) "a") 
                #false
                "Function 'check-if-too-wide?' failed")
  (check-equal? (check-if-too-wide?
                 (make-Editor 
                  (explode "abcdefghijklmnop") 
                  (explode "qrstuvwxyz")) "a") 
                #true
                "Function 'check-if-too-wide?' failed"))
; STRATEGY: Data Decomposition on ed: Editor
(define (check-if-too-wide? ed ke)
  (> (image-width 
      (text 
       (implode 
        (append 
         (append (Editor-preString ed) (cons ke '())) (Editor-postString ed))) 
       FONT-SIZE FONT-COLOR)) WIDTH))

; string->editor : String -> Editor
; Returns an Editor containing text str and cursor at position 0.
; EXAMPLES:
(begin-for-test
  (check-equal? (string->editor "test")
                (make-Editor '() (list "t" "e" "s" "t"))
                "'string->editor' function failed"))
; STRATEGY: Function Composition
(define (string->editor str)
  (make-Editor '() (explode str)))

; editor-pre : Editor -> String
; Returns the text in editor before the cursor.
; EXAMPLES:
(begin-for-test
  (check-equal? (editor-pre (make-Editor (list "a" "s" "d") (list "f" "g" "h")))
                "asd"
                "'editor-pre' function failed"))
; STRATEGY: Function Composition
(define (editor-pre ed) 
  (implode (Editor-preString ed)))

; editor-post : Editor -> String
; Returns the text in editor after the cursor.
; EXAMPLES:
(begin-for-test
  (check-equal? (editor-post 
                 (make-Editor (list "a" "s" "d") (list "f" "g" "h")))
                "fgh"
                "'editor-post' function failed"))
; STRATEGY: Function Composition
(define (editor-post ed) 
  (implode (Editor-postString ed)))

; editor-pos : Editor -> Natural
; Returns the position of the cursor in editor.
; EXAMPLES:
(begin-for-test
  (check-equal? (editor-pos 
                 (make-Editor (explode "hello") (explode "world")))
                5
                "'editor-pos' function failed"))
; STRATEGY: Function Composition
(define (editor-pos e) 
  (string-length (implode (Editor-preString e))))

(run (make-Editor (explode "hello") (explode "world")))


; ............Alternate data definition for editor.rkt................

; [A] 
;     An editor is a (make-editor Lo1S Number)

;    (define-struct Editor [editedstring position])

; INTERP: The editor has a single string storing the entire text
; 'position' here is a number and it denotes the number of characters from 
; left where the cursor is currently placed

; TEMPLATE:
; editor-fn: Editor -> ???

; (define (editor-fn ed)
;   (... Editor-editedstring ed)...)
;   (... Editor-position ed)... ))


; Pros: 
; Separate processing of pre and post string as lists would not be required. 

; Cons: there are more cons to this definition than pros as listed below: 
; 1) Each render function would require a calculation to extract the current 
; position of cursor
; 2) The draw function becomes twice as complicated than it is right now. 
; reason:
; Currently the pre and post part of the editor are rendered as individual 
; text images and the cursor is places between them at all tumes. If the 
; structure is changed to this definition the position of the cursor would be 
; hard to calculate based on changing font sizes etc

; [B] 
;     An editor is a (make-editor Lo1S Lo1S Number)

;    (define-struct Editor [pre post editorsize])

; INTERP: The editor contains two lists, one each for string before the cursor
; and string after the cursor. It also contains the combined sizes of 
; the two lists. 

; Explanation:
; A pre defined variable stating the maximum editor size can be declared.
; Value of any instance of the editor can at all times be evaluated and 
; compared with this variable. 

; TEMPLATE:
; editor-fn: Editor -> ???

; (define (editor-fn ed)
;   (... (Editor-editedstring ed)...)
;   (... (Editor-position ed)... )
;   (... (Editor-editorsize) ... ))

; Pros:
; The insert operation on the editor and the problem of text wrapping over the 
; borders of the empty-scene gets highly simplified as it involves a single 
; operation of comparing values

; Cons:
; 1) This component of the structure will be required only for the single
; operation of insert and not delete, moving left or right etc. The vaue can be 
; always calculated from individual pre and post strings for a single operation.
; 2) Unnecessary decomposition of the third component for all instances
; of sructres in all operations
