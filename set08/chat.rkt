;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chat) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
(define TIME-ON-TASK 50) ; hours
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(require rackunit)

(provide mk-world)
(provide receive)
(provide key-handler)
(provide get-users)
(provide get-editor)
(provide get-editor-pre)
(provide get-editor-post)
(provide get-chat-history)

;---Data Definitions
; A UserName is a String, consisting of only letters and numbers,
; and is between 1 and 12 characters long.
; Represents a chat room participant.
; A Message is a String

; MsgToServer is a:
; - (list 'broadcast Message)
; - (list 'private UserName Message)
; The UserName in the private message is the intended recipient of the message.
; TEMPLATE:
;(define (msg-to-srvr msg)
;  (cond
;    [(broadcast? msg) ...(list-broadcast-fn msg)...]
;    [(private? msg)   ...(list-private-fn msg) ...]))

; A MsgFromServer is a:
; - (list 'userlist ListOf<UserName>) ; all the current chat participants
; - (list 'join UserName) ; the user that just joined
; - (list 'leave UserName) ; the user that just left
; - (list 'error Message) ; an error message from the server
; - (list 'private UserName Message) ; a priv msg from the specified user
; - (list 'broadcast UserName Message) ; broadcast msg from the specified user
; TEMPLATE:
;(define (receive-frm-srvr msg)
;  (cond
;    [(userlist? msg) ...(list-broadcast-fn msg)...]
;    [(join? msg)   ...(list-join-fn msg) ...]
;    [(leave? msg)   ...(list-leave-fn msg) ...]
;    [(error? msg)   ...(list-error-fn msg) ...]
;    [(private? msg)   ...(list-private-fn msg) ...]
;    [(broadcast? msg)   ...(list-broadcast-fn msg) ...]))


; A MsgToLog is a:
; - (list 'join UserName) ; the user that just joined
; - (list 'leave UserName) ; the user that just left
; - (list 'error Message) ; an error message from the server
; - (list 'private Label Message) ; a priv msg from user/s specified in Label.
; - (list 'broadcast UserName Message) ; broadcast msg from the specified user
; TEMPLATE:
;(define (receive-to-log msg)
;  (cond
;    [(join? msg)   ...(list-join-fn msg) ...]
;    [(leave? msg)   ...(list-leave-fn msg) ...]
;    [(error? msg)   ...(list-error-fn msg) ...]
;    [(private? msg)   ...(list-private-label-fn msg) ...]
;    [(broadcast? msg)   ...(list-broadcast-fn msg) ...]))

; A Label is a String, that contains UserName(s) and/or "->"


;-----CANVAS CONSTANTS
(define USERS-BOX-WIDTH 100); Pixels
(define USERS-BOX-HEIGHT 400);Pixels
(define USERS-BOX 
  (empty-scene USERS-BOX-WIDTH USERS-BOX-HEIGHT)); Image

(define EVENT-BOX-WIDTH 300); Pixels
(define EVENT-BOX-HEIGHT 380);Pixels
(define EVENT-BOX 
  (empty-scene EVENT-BOX-WIDTH EVENT-BOX-HEIGHT)); Image

(define CHAT-BOX-WIDTH 300); Pixels
(define CHAT-BOX-HEIGHT 20);Pixels
(define CHAT-BOX 
  (empty-scene CHAT-BOX-WIDTH CHAT-BOX-HEIGHT));Image

(define CURSOR-WIDTH 1); Pixels
(define CURSOR-HEIGHT 20);Pixels
(define CURSOR (rectangle CURSOR-WIDTH CURSOR-HEIGHT "solid" "red"));Image

(define BLACK "string"); String
(define BLUE "blue"); String
(define GRAY "gray"); String
(define RED "red"); String
(define ZERO 0); zero
(define SERVER "cs5010-chat.ccs.neu.edu"); chat server
(define MAX-NO-OF-EVENTS 25); Number
(define TXT-SIZE 12); Number
(define MIN-USR-LEN 1) ; Min User name length
(define MAX-USR-LEN 12) ; Max User name length
(define COLON ":"); String representing :


(define-struct editor [pre post move])
; An Editor is (make-editor ListOf<1String> ListOf<1String> PosInt) 
; WHERE: move ranges between (0,300]
; INTERP: (make-editor s t) means the cursor is
; displayed between s and t. move is the area sweeped
; by the cursor. 

(define-struct world (editor user-list event-list user-name))
; A World is (make-world Editor ListOf<UserName> ListOf<MsgToLog> UserName) 
; WHERE: user-list is alwasys sorted case insensitive ascending order.
; user-name is the current logged in user.



;--DATA EXAMPLES
(define SMALL-TXT (make-list 10 "W"))
(define BIG-TXT (make-list 80 "W"))
(define INIT-EDITOR (make-editor '() '() 1))
(define ED1 (make-editor '("a" "b") '("c" "d") 1))
(define INIT-WORLD (make-world INIT-EDITOR empty empty ""))
(define INIT-WORLD2 (make-world (make-editor '("h") '() 1)
                               '("hi" "bye") empty "hi"))
(define INIT-WORLD3 (make-world (make-editor '("h" "i") '() 1)
                               '("hi" "bye") empty "hi"))
(define INIT-WORLD4 (make-world (make-editor '() '("h") 1)
                               '("hi" "bye") empty "hi"))
(define INIT-WORLD5 (make-world (make-editor '("h") '() 1)
                               '("hi" "bye") empty "hi"))
(define ULST1 '("hero"))
(define IMG1 (empty-scene 100 400))
(define IMG2 (empty-scene 100 200))
(define JOIN-MSG (list 'join "dragon"))
(define LEAVE-MSG (list 'leave "dragon"))
(define ERROR-MSG (list 'error "dragon"))
(define BROADCAST (list 'broadcast "dragon" "hi"))
(define PRIVATE (list 'private "dragon->dracula" "hi"))
(define LOMTL (list JOIN-MSG LEAVE-MSG ERROR-MSG BROADCAST PRIVATE))

(define SAMPLE-TEXT-IMAGE (text "sample text" 12 "black"));Image

;-predicate fn
; colon? : String -> Boolean
; Predicate to determine if the passed string is a colon ":"
; EXAMPLES
(begin-for-test
  (check-equal? (colon? ":")
                #true
                "Function failed")  
  (check-equal? (colon? "a")
                #false
                "Function failed"))
; STRATEGY : Function Composition
(define (colon? s) (string=? COLON s))

; userlist? : MsgToLog or MsgToServer -> Boolean
; Predicate to determine if the passed list/msg is a ListOf<UserName>
; EXAMPLES
(begin-for-test
  (check-equal? (userlist? (list 'broadcast "u" "hi"))
                #false
                "Function failed")  
  (check-equal? (userlist? (list 'userlist (list "admin" "test-user")))
                #true
                "Function failed"))
; STRATEGY : Data Decomposition on msg : MsgToLog or MsgToServer
(define (userlist? msg)
  (symbol=? 'userlist (first msg)))

; join? :  MsgToLog or MsgToServer -> Boolean
; Predicate to determine if the passed list/msg is a UserName who joined chat
; EXAMPLES
(begin-for-test
  (check-equal? (join? (list 'broadcast "u" "hi"))
                #false
                "Function failed")  
  (check-equal? (join? (list 'join "admin"))
                #true
                "Function failed"))
; STRATEGY : Data Decomposition on msg : MsgToLog or MsgToServer
(define (join? msg)
  (symbol=? 'join (first msg)))

; leave? :  MsgToLog or MsgToServer -> Boolean
; Predicate to determine if the passed list/msg is a UserName who left chat
; EXAMPLES
(begin-for-test
  (check-equal? (leave? (list 'broadcast "u" "hi"))
                #false
                "Function failed")  
  (check-equal? (leave? (list 'leave "admin"))
                #true
                "Function failed"))
; STRATEGY : Data Decomposition on msg : MsgToLog or MsgToServer
(define (leave? msg)
  (symbol=? 'leave (first msg)))

; error? :  MsgToLog or MsgToServer -> Boolean
; Predicate to determine if the passed list/msg is an error message from server
; EXAMPLES
(begin-for-test
  (check-equal? (error? (list 'broadcast "u" "hi"))
                #false
                "Function failed")  
  (check-equal? (error? (list 'error "server down"))
                #true
                "Function failed"))
; STRATEGY : Data Decomposition on msg : MsgToLog or MsgToServer
(define (error? msg)
  (symbol=? 'error (first msg)))

; private? :  MsgToLog or MsgToServer -> Boolean
; Predicate to determine if the passed list/msg is a private message
; EXAMPLES
(begin-for-test
  (check-equal? (private? (list 'broadcast "u" "hi"))
                #false
                "Function failed")  
  (check-equal? (private? (list 'private "admin" "meeting at 2"))
                #true
                "Function failed"))
; STRATEGY : Data Decomposition on msg : MsgToLog or MsgToServer
(define (private? msg)
  (symbol=? 'private (first msg)))

; broadcast? :  MsgToLog or MsgToServer -> Boolean
; Predicate to determine if the passed list/msg is a private message
; EXAMPLES
(begin-for-test
  (check-equal? (broadcast? (list 'broadcast "u" "hi"))
                #true
                "Function failed")  
  (check-equal? (broadcast? (list 'private "admin" "meeting at 2"))
                false
                "Function failed"))
; STRATEGY : Data Decomposition on msg : MsgToLog or MsgToServer
(define (broadcast? msg)
  (symbol=? 'broadcast (first msg)))


;---Functions Begin
; run : UserName IPAddress -> World
; Connect to the given chat server with user name nam.
; server here is an IPAddress.
; STRATEGY : Function Composition
(define (run nam server)
  (big-bang (mk-world nam)
            (on-receive receive)
            (to-draw render)
            (on-key key-handler)
            (name nam)
            (register server)
            (port 5010)))

; mk-world : UserName -> World
; Returns the initial world state for user name.
; EXAMPLES :
(begin-for-test
  (check-equal? (mk-world "admin")
                (make-world
                 (make-editor empty empty 1)
                 empty
                 empty
                 "admin")
                "Function failed"))
; STRATEGY: Function Composition
(define (mk-world nam)
  (make-world (make-editor empty empty CURSOR-WIDTH) empty empty nam))

; get-user-name : World -> UserName
; Returns self. the currently logged in user
; EXAMPLES : 
(begin-for-test
  (check-equal? (get-user-name (make-world
                 (make-editor empty empty 0)
                 empty
                 empty
                 "admin"))
                "admin"
                "Function failed"))
; STRATEGY : Data Decomposition on w : World
(define (get-user-name w)
  (world-user-name w))

; text->img : String -> Image
; Converts the passed text into an image
; EXAMPLES
(begin-for-test
  (check-equal? (text->img "sample text")
                SAMPLE-TEXT-IMAGE
                "Function failed"))
(define (text->img txt)
  (text txt TXT-SIZE BLACK))

; receive : World MsgFromServer -> HandlerResult
; Handles messages received from the server.
; EXAMPLES:
(begin-for-test
  (check-equal? (receive (make-world ED1 empty empty "admin") 
                         (list 'broadcast "hello world"))
                (make-world
                 (make-editor (list "a" "b") (list "c" "d") 1)
                 empty
                 (list (list 'broadcast "hello world"))
                 "admin")
                "Function failed")
  (check-equal? (receive (make-world ED1 empty empty "user1") 
                         (list 'leave "user2"))
                (make-world
                 (make-editor (list "a" "b") (list "c" "d") 1)
                 empty
                 (list (list 'leave "user2"))
                 "user1")
                "Function failed")
  (check-equal? (receive (make-world ED1 empty empty "user2") 
                         (list 'join "user1"))
                (make-world
                 (make-editor (list "a" "b") (list "c" "d") 1)
                 (list "user1")
                 (list (list 'join "user1"))
                 "user2")
                "Function failed")
  (check-equal? (receive (make-world ED1 empty empty "admin") 
                         (list 'userlist (list "user1" "admin" "user2")))
                (make-world
                 (make-editor (list "a" "b") (list "c" "d") 1)
                 (list "admin" "user1" "user2")
                 empty
                 "admin")
                "Function failed")
  (check-equal? (receive (make-world ED1 empty empty "admin") 
                         (list 'error "system down"))
                (make-world
                 (make-editor (list "a" "b") (list "c" "d") 1)
                 empty
                 (list (list 'error "system down"))
                 "admin")
                "Function failed")
  (check-equal? (receive (make-world ED1 empty empty "admin") 
                         (list 'private "user1" "Meeting at 2"))
                (make-world
                 (make-editor (list "a" "b") (list "c" "d") 1)
                 empty
                 (list (list 'private "user1" "Meeting at 2"))
                 "admin")
                "Function failed"))
; STRATEGY: Data Decomposition on msg: MsgFromServer
(define (receive w msg)
  (cond
    [(userlist? msg) (generate-user-list w (second msg))]
    
    [(join? msg) (update-event-list
                  (add-user w (second msg)) msg)]
    
    [(leave? msg) (update-event-list 
                   (remove-user w (second msg)) msg)]
    
    [(error? msg) (update-event-list w msg)]
    
    [(private? msg) (update-event-list w msg)]
    
    [(broadcast? msg) (update-event-list w msg)]))

; generate-user-list : World ListOf<UserName> -> World
; Returns the world with updated/sorted userlist 
; EXAMPLES:
(begin-for-test 
  (check-equal? (generate-user-list (make-world
                                     ED1
                                     empty
                                     (list 
                                      (list 'private "user1" "Meeting at 2"))
                                     "admin") (list "usr2" "admin" "user1"))
                (make-world
                 (make-editor (list "a" "b") (list "c" "d") 1)
                 (list "admin" "user1" "usr2")
                 (list (list 'private "user1" "Meeting at 2"))
                 "admin")
                "Function failed"))
; STRATEGY : Data Decomposition on w : World
(define (generate-user-list w usrlst)
  (make-world
   (world-editor w) (sort usrlst string-ci<=?)
   (world-event-list w) (world-user-name w)))

; update-event-list : World MsgToLog -> World
; Returns World with updated event list/updated MsgToLog
; WHERE : The Event list will contains only the set maximum number of latest 
; events and if there exist more events then the older events will be excluded
; with only the most recent retained
; EXAMPLES :
(begin-for-test 
  (check-equal? (update-event-list (make-world
                                    (make-editor 
                                     (list "a" "b") (list "c" "d") 1)
                                    (list "admin" "user1" "usr2")
                                    (list 
                                     (list 'private "user1" "Meeting at 2"))
                                    "admin") (list 'join "user3"))
                (make-world
                 (make-editor (list "a" "b") (list "c" "d") 1)
                 (list "admin" "user1" "usr2")
                 (list
                  (list 'private "user1" "Meeting at 2")
                  (list 'join "user3"))
                 "admin")
                "Function failed")
  (check-equal? (update-event-list (make-world
                                    (make-editor 
                                     (list "a" "b") (list "c" "d") 1)
                                    (list "admin" "user1" "usr2")
                                    (list 
                                     (list 'join "usr1")
                                     (list 'join "usr2")
                                     (list 'join "usr3")
                                     (list 'join "usr4")
                                     (list 'join "usr5")
                                     (list 'join "usr6")
                                     (list 'join "usr7")
                                     (list 'join "usr8")
                                     (list 'join "usr9")
                                     (list 'join "usr10")
                                     (list 'join "usr11")
                                     (list 'join "usr12")
                                     (list 'join "usr13")
                                     (list 'join "usr14")
                                     (list 'join "usr15")
                                     (list 'leave "usr1")
                                     (list 'leave "usr2")
                                     (list 'leave "usr3")
                                     (list 'leave "usr4")
                                     (list 'leave "usr5")
                                     (list 'leave "usr6")
                                     (list 'leave "usr7")
                                     (list 'leave "usr8")
                                     (list 'leave "usr9")
                                     (list 'leave "usr10")
                                     (list 'leave "usr11")
                                     (list 'leave "usr12"))
                                    "admin") (list 'join "user3"))
                (make-world
                 (make-editor (list "a" "b") (list "c" "d") 1)
                 (list "admin" "user1" "usr2")
                 (list
                  (list 'join "usr2")
                  (list 'join "usr3")
                  (list 'join "usr4")
                  (list 'join "usr5")
                  (list 'join "usr6")
                  (list 'join "usr7")
                  (list 'join "usr8")
                  (list 'join "usr9")
                  (list 'join "usr10")
                  (list 'join "usr11")
                  (list 'join "usr12")
                  (list 'join "usr13")
                  (list 'join "usr14")
                  (list 'join "usr15")
                  (list 'leave "usr1")
                  (list 'leave "usr2")
                  (list 'leave "usr3")
                  (list 'leave "usr4")
                  (list 'leave "usr5")
                  (list 'leave "usr6")
                  (list 'leave "usr7")
                  (list 'leave "usr8")
                  (list 'leave "usr9")
                  (list 'leave "usr10")
                  (list 'leave "usr11")
                  (list 'leave "usr12")
                  (list 'join "user3"))
                 "admin")
                "Function failed"))   
; STRATEGY : Data Decomposition on w : World
(define (update-event-list w event)
  (local
    (
     (define event-list (if (< (length (world-event-list w)) MAX-NO-OF-EVENTS)
                            (world-event-list w)
                            (rest (world-event-list w)))))
    (make-world (world-editor w) (world-user-list w)
              (add-at-last event event-list) (world-user-name w))))

; remove-user : World UserName -> World 
; Returns an updated world with the passed username removed from the 
; world's list of user names
; EXAMPLES :
(begin-for-test
  (check-equal? (remove-user (make-world
                              ED1
                              (list "admin" "user1" "usr2")
                              (list 
                               (list 'private "user1" "Meeting at 2"))
                              "admin") "usr2")
                (make-world
                 (make-editor (list "a" "b") (list "c" "d") 1)
                 (list "admin" "user1")
                 (list (list 'private "user1" "Meeting at 2"))
                 "admin")
                "Function failed"))
; STRATEGY : Data Decomposition on w : World
(define (remove-user w usr)
  (make-world (world-editor w) (remove usr (world-user-list w))
              (world-event-list w) (world-user-name w)))

; add-user : World UserName
; Returns the updated world with the passed user name as an added user in 
; the world's list of user names
; EXAMPLES:
(begin-for-test
  (check-equal? (add-user (make-world
                           ED1
                           (list "admin" "user1" "usr2")
                           (list 
                            (list 'private "user1" "Meeting at 2"))
                           "admin") "usr4")
                (make-world
                 (make-editor (list "a" "b") (list "c" "d") 1)
                 (list "admin" "user1" "usr2" "usr4")
                 (list (list 'private "user1" "Meeting at 2"))
                 "admin")
                "Function failed"))
; STRATEGY : Data Decomposition on w : World
(define (add-user w usr)
  (make-world (world-editor w)
              (sort (cons usr (world-user-list w)) string-ci<=?)
              (world-event-list w) (world-user-name w)))

; render: World -> Image
; Accepts a World and renders them on an interactive window
; EXAMPLES: 
(begin-for-test
  (check-equal? (render INIT-WORLD)
                (beside
                 USERS-BOX
                 (above
                  EVENT-BOX (overlay/align "left" "center" CURSOR
                                           CHAT-BOX)))
                "Rendered"))
; STRATEGY: Data Decomposition on w:World
(define (render w)
  (beside
   (render-users (world-user-list w))
   (above
    (render-events (world-event-list w))
    (render-chat (world-editor w)))))
; Test-suite: 
(begin-for-test
  (check-equal? (render INIT-WORLD)
                (beside
                 USERS-BOX
                 (above
                  EVENT-BOX (overlay/align "left" "center" CURSOR
                                           CHAT-BOX)))
                "Rendered"))


; render-users: ListOf<UserName> -> Image
; Accepts a ListOf<UserName> and renders them
; as an Image.
; EXAMPLES: 
(begin-for-test
  (check-equal? (render-users ULST1)
                (overlay/align "left" "top" (text->img "hero") USERS-BOX)
                "Rendered"))
; STRATEGY: Function Composition
(define (render-users user-list)
  (overlay/align "left" "top" (draw-users user-list) USERS-BOX))
; Test-suite: 
(begin-for-test
  (check-equal? (render-users empty)
                USERS-BOX
                "Rendered"))

; draw-users: ListOf<UserName> -> Image
; Accepts a ListOf<UserName> and renders them
; as an Image.
; EXAMPLES: 
(begin-for-test
  (check-equal? (draw-users '("u1" "u2"))
                (foldl draw-or-discard-user empty-image
                       (map text->img '("u1" "u2")))
                "Rendered"))
; STRATEGY: Function Composition
(define (draw-users usrlst)
  (foldl draw-or-discard-user empty-image
         (map text->img usrlst)))
; Test-suite: 
(begin-for-test
  (check-equal? (draw-users '("u1" "u2" "u3"))
                (foldl draw-or-discard-user empty-image
                       (map text->img '("u1" "u2" "u3")))
                "Rendered"))


; draw-or-discard-user: Image Image -> Image
; Accepts a Image constructed with UserName and
; another Image constructed with ListOf<UserName>
; and tries to render them stacking on top of each
; other. If it fits in the USERS-BOX, renders a final
; Image or returns the old Image.
; EXAMPLES: 
(begin-for-test
  (check-equal? (draw-or-discard-user IMG1 (text->img "hero"))
                (text->img "hero")
                "Rendered"))
; STRATEGY: Function Composition
(define (draw-or-discard-user newuser rendered)
  (local
    (
     (define final-img (above/align "left" rendered newuser)))
    (if (<= (image-height final-img) USERS-BOX-HEIGHT)
        final-img
        rendered)))
; Test-suite: 
(begin-for-test
  (check-equal? (draw-or-discard-user (text->img "hero") IMG2)
                (above/align "left" IMG2 (text->img "hero"))                
                "Rendered"))

; render-events: ListOf<MsgToLog> -> Image
; Accepts a ListOf<MsgToLog> and renders them
; as an Image.
; EXAMPLES: 
(begin-for-test
  (check-equal? (render-events (list JOIN-MSG LEAVE-MSG))
                (overlay/align
                 "left" "top"
                 (draw-events (list JOIN-MSG LEAVE-MSG)) EVENT-BOX)
                "Rendered"))
; STRATEGY: Function Composition
(define (render-events event-list)
  (overlay/align "left" "top" (draw-events event-list) EVENT-BOX))
; Test-suite: 
(begin-for-test
  (check-equal? (render-events (list JOIN-MSG))
                (overlay/align
                 "left" "top"
                 (draw-events (list JOIN-MSG)) EVENT-BOX)
                "Rendered"))

; draw-events: ListOf<MsgToLog> -> Image
; Accepts a ListOf<MsgToLog> and renders them
; as an Image.
; EXAMPLES: 
(begin-for-test
  (check-equal? (draw-events (list JOIN-MSG LEAVE-MSG))
                (foldr draw-or-discard-event empty-image
                       (map print-event (list JOIN-MSG LEAVE-MSG))) 
                "Rendered"))
; STRATEGY: Function Composition
(define (draw-events evntlst)
  (foldr draw-or-discard-event empty-image
         (map print-event evntlst)))
; EXAMPLES: 
(begin-for-test
  (check-equal? (draw-events (list LEAVE-MSG))
                (foldr draw-or-discard-event empty-image
                       (map print-event (list LEAVE-MSG)))                
                "Rendered"))

; draw-or-discard-event: Image Image -> Image
; Accepts a Image constructed with MsgToLog and
; another Image constructed with ListOf<MsgToLog>
; and tries to render them stacking on top of each
; other. If it fits in the EVENT-BOX, renders a final
; Image or returns the old Image.
; EXAMPLES: 
(begin-for-test
  (check-equal? (draw-or-discard-event (text->img "hi") IMG2)
                (above/align "left" (text->img "hi") IMG2)                
                "Rendered"))
; STRATEGY: Function Composition
(define (draw-or-discard-event event rendered)
  (local
    (
     (define final-img (above/align "left" event rendered)))
    (if (<= (image-height final-img) EVENT-BOX-HEIGHT)
        final-img
        rendered)))
; Test-suite: 
(begin-for-test
  (check-equal? (draw-or-discard-event (text->img "hi") IMG1)
                IMG1
                "Rendered"))


; print-event: MsgToLog -> Image
; Accepts a MsgToLog and renders it as an Image
; EXAMPLES: 
(begin-for-test
  (check-equal? (print-event JOIN-MSG)
                (text "dragon joined the chat." 12 GRAY)                
                "Rendered")
  (check-equal? (print-event LEAVE-MSG)
                (text "dragon left the chat." 12 GRAY)                
                "Rendered"))
; STRATEGY: Data Decomposition on event: MsgToLog
(define (print-event event)
  (cond
    
    [(join? event)
     (text (string-append (second event) " joined the chat.") TXT-SIZE GRAY)]
    
    [(leave? event)
     (text (string-append (second event) " left the chat.") TXT-SIZE GRAY)]
    
    [(error? event)
     (text (second event) TXT-SIZE RED)]
    
    [(broadcast? event)
     (print-message (append-uname event) BLACK)]
    
    [else
     (print-message (append-uname event) BLUE)]))
; Test-suite: 
(begin-for-test
  (check-equal? (print-event ERROR-MSG)
                (text "dragon" 12 RED)                
                "Rendered")
  (check-equal? (print-event BROADCAST)
                (print-message
                (append-uname BROADCAST) BLACK)
                "Rendered")
  (check-equal? (print-event PRIVATE)
                (print-message
                (append-uname PRIVATE) BLUE)
                "Rendered"))

; append-uname: MsgToLog -> ListOf<1String>
; Adds username to the message taken from MsgToLog
; and adds the braces.
; EXAMPLES: 
(begin-for-test
  (check-equal? (append-uname BROADCAST)
                (explode "< dragon > hi")                
                "Rendered"))
; STRATEGY: Data Decomposition on event: MsgToLog
(define (append-uname event)
  (explode
   (string-append "< " (second event) " > " (third event))))
; Test-suite: 
(begin-for-test
  (check-equal? (append-uname PRIVATE)
                (explode "< dragon->dracula > hi")                
                "Rendered"))


; exceeds-line-width?: ListOf<1String> -> Boolean
; Returns true if the received text fits in the
; EVENT-BOX in a single line
; EXAMPLES: 
(begin-for-test
  (check-equal? (exceeds-line-width? SMALL-TXT)
                #false                
                "false"))
; STRATEGY: Function Composition
(define (exceeds-line-width? txt)
  (> (image-width (text->img (implode txt))) EVENT-BOX-WIDTH))
; Test-suite: 
(begin-for-test
  (check-equal? (exceeds-line-width? BIG-TXT)
                #true                
                "true"))

; print-message: ListOf<1String> String -> Image
; prints the given text splitting across multiple
; lines if needed with the given color.
; EXAMPLES: 
(begin-for-test
  (check-equal? (print-message SMALL-TXT BLACK)
                (text->img (implode SMALL-TXT))
                "Rendered"))
; STRATEGY: Date Decomposition on lst: ListOf<1String>
(define (print-message lst0 color)
  (local
    (
     ; ListOf<1String> String ListOf<1String> -> Image
     ; returns the given text either as a single line or multiple lines.
     ; accumulator rendered-list is the content allowed on current line so far
     (define (print-message/a lst color rendered-list)
       (cond
         [(empty? (rest lst))
          (if (exceeds-line-width? (add-at-last (first lst) rendered-list))
              (above/align "left"
                           (text (implode rendered-list) TXT-SIZE color)
                           (text (first lst) TXT-SIZE color))
              (text (implode (add-at-last (first lst) rendered-list))
                    TXT-SIZE color))]
         
         [else
          (if (exceeds-line-width? (add-at-last (first lst) rendered-list))
              (above/align "left"
                           (text (implode rendered-list) TXT-SIZE color)
                           (print-message/a (rest lst) color
                                            (list (first lst))))
              (print-message/a (rest lst) color
                               (add-at-last (first lst) rendered-list)))])))
    (print-message/a lst0 color empty)))
; Test-suite: 
(begin-for-test
  (check-equal? (print-message SMALL-TXT BLACK)
                (text->img (implode SMALL-TXT))
                "Rendered"))

; render-chat: Editor -> Image
; returns the contents of the given editor
; as an Image.
; EXAMPLES: 
(begin-for-test
  (check-equal? (render-chat INIT-EDITOR)
                (overlay/align "left" "center" CURSOR CHAT-BOX)
                "Rendered"))
; STRATEGY: Function Composition
(define (render-chat ed)
  (overlay/align "left" "center" (content ed) CHAT-BOX))
; Test-suite: 
(begin-for-test
  (check-equal? (render-chat INIT-EDITOR)
                (overlay/align "left" "center" CURSOR CHAT-BOX)
                "Rendered"))

; content: Editor -> Image
; Accepts a Editor and renders them
; with pre, cursor, post as the sequence.
; EXAMPLES:
(begin-for-test
  (check-equal? (content INIT-EDITOR)              
                 CURSOR
                 "generated"))
; STRATEGY: Data Decomposition on ed: Editor
(define (content ed)
  (if (text-exceeds-box? ed)
      (truncate-and-render ed)
      (beside
       (text->img (implode (editor-pre ed)))
       CURSOR
       (text->img (implode (editor-post ed))))))
; Test-suite:
(begin-for-test
  (check-equal? (content ED1)
                (beside
                 (text->img "ab")
                 CURSOR
                 (text->img "cd")))              
                "generated")

; truncate-and-render: Editor -> Image
; Accepts a Editor and renders the content that 
; currently fits in the CHAT-BOXignoring the other
; text with pre, cursor, post as the sequence.
; EXAMPLES:
(begin-for-test
  (check-equal? (truncate-and-render ED1)
                (beside 
                 (text->img (get-pre '() 50))
                 CURSOR
                 (text->img (get-post '("c" "d") 50)))
                "generated"))
(define (truncate-and-render ed)
  (beside
   (text->img (get-pre (editor-pre ed) (editor-move ed)))
   CURSOR
   (text->img (get-post (editor-post ed) (editor-move ed)))))
; Test-suite:
(begin-for-test
  (check-equal? (truncate-and-render ED1)
                (beside 
                 CURSOR
                 (text->img "cd"))
                "generated"))

; text-crosses-cursor?: ListOf<1String> PosInt -> Boolean
; returns true if the given text crosses the current cursor
; position which means the crossed text shouldn't be allowed
; to be rendered.
; INTERP: cursor-pos ranges between (0,300]
; EXAMPLES:
(begin-for-test
  (check-equal? (text-crosses-cursor? SMALL-TXT 10)
                #true
                "generated")) 
(define (text-crosses-cursor? txt cursor-pos)
  (> (image-width (text->img (implode txt)))
     cursor-pos))
; Test-suite:
(begin-for-test
  (check-equal? (text-crosses-cursor? BIG-TXT 16)
                #true
                "generated"))


; get-pre: ListOf<1String> PosInt -> String
; returns the text before the cursor that can be
; rendered starting from the cursor moving left.
; INTERP: pos ranges between (0,300]
; EXAMPLES:
(begin-for-test
  (check-equal? (get-pre SMALL-TXT 10)
                ""
                "generated")) 
; STRATEGY: Data Decomposition in pre: ListOf<1String>
(define (get-pre pre0 pos)
  (local
    (
     ; ListOf<1String> PosInt ListOf<1String> -> String
     ; returns the text before the cursor that can be rendered.
     ; accumulator acc is the text that can fit in the box so far.
     (define (get-pre/a pre pos acc)
       (cond
         [(empty? pre) (implode acc)]
         [else
          (if (text-crosses-cursor? (cons (last pre) acc) (- pos CURSOR-WIDTH))
              (implode acc)
              (get-pre/a (except-last pre)  pos (cons (last pre) acc)))])))
    (get-pre/a pre0 pos empty)))
; Test-suite:
(begin-for-test
  (check-equal? (get-pre SMALL-TXT 5)
                ""
                "generated")) 


; get-post: ListOf<1String> PosInt -> String
; returns the text after the cursor that can be
; rendered starting from the cursor moving right.
; INTERP: pos ranges between (0,300]
; EXAMPLES:
(begin-for-test
  (check-equal? (get-post SMALL-TXT 15)
                "WWWWWWWWWW"
                "generated")) 
; STRATEGY: Data Decomposition in post: ListOf<1String>
(define (get-post post0 pos)
  (local
    (
     ; ListOf<1String> PosInt ListOf<1String> -> String
     ; returns the text after the cursor that can be rendered.
     ; accumulator acc is the text that can fit in the box so far.
     (define (get-post/a post pos acc)
       (cond
         [(empty? post) (implode acc)]
         [else
          (if (text-crosses-cursor? (add-at-last (first post) acc)
                                    (- CHAT-BOX-WIDTH pos))
              (implode acc)
              (get-post/a (rest post) pos (add-at-last (first post) acc)))])))
    (get-post/a post0 pos empty)))
; Test-suite:
(begin-for-test
  (check-equal? (get-post SMALL-TXT 25)
                "WWWWWWWWWW"
                "generated")) 

; text-exceeds-box?: Editor -> Boolean
; true if the contents of the editor exceed the CHAT-BOX
; EXAMPLES:
(begin-for-test
  (check-equal? (text-exceeds-box? ED1)
                #false
                "generated")) 
; STRATEGY: Function Composition
(define (text-exceeds-box? ed)
  (> (+ (image-width (text->img (implode (editor-pre ed))))
        (image-width (text->img (implode (editor-post ed))))
        (image-width CURSOR))
     CHAT-BOX-WIDTH))
; Test-suite:
(begin-for-test
  (check-equal? (text-exceeds-box? INIT-EDITOR)
                #false
                "generated")) 


; key-handler: World KeyEvent -> World
; Returns an World with the ListOf<1String>
; modified after the KeyStroke
; EXAMPLES:
(begin-for-test
  (check-equal? (key-handler INIT-WORLD "\b")
                (handle-bksp INIT-WORLD)
                "true")
  (check-equal? (key-handler INIT-WORLD "\r")
                (send-message INIT-WORLD)
                "true")
  (check-equal? (key-handler INIT-WORLD "\t")
                (search-possible-user INIT-WORLD)
                "true")
  (check-equal? (key-handler INIT-WORLD "\u007F")
                INIT-WORLD
                "true"))
; STRATEGY: Data Decomposition on ke: KeyEvent
(define (key-handler w ke)
  (cond    
    [(key=? "\b" ke) (handle-bksp w)]
    
    [(key=? "\r" ke) (send-message w)]
    
    [(key=? "\t" ke) (search-possible-user w)]
    
    [(key=? "\u007F" ke) w]
    
    [(= (string-length ke) 1) (update-editor w ke)]
    
    [(key=? "left" ke) (handle-left w)]
    
    [(key=? "right" ke) (handle-right w)]
    
    [else w]))
; Test-suite:
(begin-for-test
  (check-equal? (key-handler INIT-WORLD "b")
                (update-editor INIT-WORLD "b")
                "true")
  (check-equal? (key-handler INIT-WORLD "left")
                (handle-left INIT-WORLD)
                "true")
  (check-equal? (key-handler INIT-WORLD "right")
                (handle-right INIT-WORLD)
                "true")
  (check-equal? (key-handler INIT-WORLD "up")
                INIT-WORLD
                "true"))

; search-possible-user: World -> World
; if editor-pre matches any user then the
; user nname is autofilled. otherwise the same world
; is returned.
; EXAMPLES:
(begin-for-test
  (check-equal? (search-possible-user INIT-WORLD)
                INIT-WORLD
                "true"))
; STRATEGY: Data Decomposition on w: World
(define (search-possible-user w)
  (local
    ( 
     (define text (implode (editor-pre (world-editor w))))
     (define possible-user
       (filter
        (λ(l) (prefix-of-str? text l)) (world-user-list w))))
    (if (empty? possible-user)
        w
        (make-world
         (make-editor
          (explode (first possible-user)) empty (editor-move (world-editor w)))
         (world-user-list w) (world-event-list w) (world-user-name w)))))
; Test-suite:
(begin-for-test
  (check-equal? (search-possible-user INIT-WORLD2)
                INIT-WORLD3
                "true"))


; prefix-of-str? : String String -> Boolean
; Returns true if name is str1 is a prefix of str2.
; EXAMPLES:
(begin-for-test
  (check-equal? (prefix-of-str? "he" "heeman")
                #true
                "true"))
; Strategy: function composition
(define (prefix-of-str? str1 str2)
  (and (not (string=? str1 ""))
       (<= (string-length str1) (string-length str2))
       (string=? str1 (substring str2 0 (string-length str1)))))
; Test-suite:
(begin-for-test
  (check-equal? (prefix-of-str? "hes" "heeman")
                #false
                "true"))


; valid-user? : String -> Boolean
; Returns true if name is uname is a valid UserName
; EXAMPLES:
(begin-for-test
  (check-equal? (valid-user? "he")
                #true
                "true"))
; Strategy: function composition
(define (valid-user? uname)
  (local
    (
     (define uname-chars (explode uname)))
    (and (>= (length uname-chars) MIN-USR-LEN)
         (<= (length uname-chars) MAX-USR-LEN)
         (andmap
          (λ(l) (or (string-alphabetic? l)
                    (string-numeric? l))) uname-chars))))
; Test-suite:
(begin-for-test
  (check-equal? (valid-user? "he#")
                #false
                "true"))

; send-message : World -> Boolean
; Sends a message to the intended recipient
; Strategy: Data Decomposition on split-msg: ListOf<String>
(define (send-message w)
  (local
    (
     (define chat-text (get-editor-content w))
     (define split-msg (break-msg chat-text)))
    (if (private-msg? split-msg)
        (send-private-msg w (first split-msg) (second split-msg))
        (send-broadcast-msg w (implode chat-text)))))


; private-msg?: String -> Boolean
; returns true if the message is intended
; to a private recipient.
; EXAMPLES:
(begin-for-test
  (check-equal? (private-msg? '("hi"))
                #false
                "true"))
; STRATEGY: Data Decomposition on msg: ListOf<String>
(define (private-msg? msg)
  (and (> (length msg) 1)
       (valid-user? (first msg))))
; Test-suite:
(begin-for-test
  (check-equal? (private-msg? '("heeman" "hello"))
                #true
                "true"))

; get-editor-content: World -> ListOf<1String>
; returns the current content of the editor
; in the given world.
; EXAMPLES:
(begin-for-test
  (check-equal? (get-editor-content INIT-WORLD)
                '()
                "true"))
; STRATEGY: Data Decomposition on w: World
(define (get-editor-content w)
  (append (editor-pre (world-editor w))
          (editor-post (world-editor w))))
; Test-suite:
(begin-for-test
  (check-equal? (get-editor-content INIT-WORLD2)
                '("h")
                "true"))

; send-private-msg : World String String -> HandlerResult
; Returns a HanlerResult with a new World and a message 
; to server in the form of MsgToServer
; STRATEGY: Data Decomposition on w: World
;EXAMPLES:
(begin-for-test
  (check-equal? (send-private-msg INIT-WORLD2 "u1" "hi")
                (make-package
                 (update-event-list
                  (make-world
                   (make-editor empty empty (editor-move 
                                             (world-editor INIT-WORLD2)))
                    (world-user-list INIT-WORLD2) (world-event-list INIT-WORLD2)
                    (world-user-name INIT-WORLD2))
                      (list 'private "hi->u1" "hi")) (list 'private "u1"
                                                           "hi"))
                     "true"))

(define (send-private-msg w recipient msg)
  (local
    (
     (define msg-to-log
       (list 'private (string-append (get-user-name w) "->" recipient) msg))
     (define msg-to-srvr
       (list 'private recipient msg)))       
    (make-package
     (update-event-list
      (make-world
       (make-editor empty empty (editor-move (world-editor w)))
       (world-user-list w) (world-event-list w) (world-user-name w))
      msg-to-log) msg-to-srvr)))


; send-broadcast-msg : World String -> HandlerResult
; Returns a HanlerResult with a new World and a message 
; to server in the form of MsgToServer
; STRATEGY: Data Decomposition on w: World
(define (send-broadcast-msg w msg)
  (local
    (
     (define msg-to-srvr (list 'broadcast msg))
     (define msg-to-log (list 'broadcast (get-user-name w) msg)))
    (make-package
     (update-event-list
      (make-world
       (make-editor empty empty (editor-move (world-editor w)))
       (world-user-list w) (world-event-list w) (world-user-name w))
      msg-to-log) msg-to-srvr)))

; break-msg: ListOf<1String> -> ListOf<String>
; returns a list of recipient and the content of
; the given message.
; EXAMPLES:
(begin-for-test
  (check-equal? (break-msg (explode "heeman: hello"))
                '("heeman" " hello")
                "true"))
; STRATEGY: Data Decomposition on msg: ListOf<String>
(define (break-msg msg0)
  (local
    (
     ; ListOf<1String> ListOf<1String> -> ListOf<String>
     ; returns a list of content before and after the colon
     ; if colon exists else returns a list with the whole string as the item
     ; accumulator acc is the ocntent before encountering colon so far
     (define (break-msg/a msg acc)
       (cond
         [(empty? msg) (list (implode acc))]
         [else (if (colon? (first msg))
                   (list (implode acc)
                         (implode (rest msg)))
                   (break-msg/a (rest msg) (add-at-last (first msg) acc)))])))
    (break-msg/a msg0 empty)))
; Test-suite:
(begin-for-test
  (check-equal? (break-msg (explode "heeman hello"))
                '("heeman hello")
                "true"))


; handle-bksp: World -> World
; Handles "\b" KeyEvent and deletes the character before CURSOR
; if its possible, otherwise returns same state
; EXAMPLES:
(begin-for-test
  (check-equal? (handle-bksp INIT-WORLD)
                INIT-WORLD
                "true")

  (check-equal? (handle-bksp (make-world
                              ED1
                              (list "admin" "user1" "usr2")
                              (list 
                               (list 'private "user1" "Meeting at 2"))
                              "admin"))
                (make-world
                 (make-editor (list "a") (list "c" "d") 1)
                 (list "admin" "user1" "usr2")
                 (list (list 'private "user1" "Meeting at 2"))
                 "admin")
                "Function failed")
  (check-equal? (handle-bksp (make-world
                              (make-editor empty (list "c" "d") 1)
                              (list "admin" "user1" "usr2")
                              (list (list 'private "user1" "Meeting at 2"))
                              "admin"))
                (make-world
                 (make-editor empty (list "c" "d") 1)
                 (list "admin" "user1" "usr2")
                 (list (list 'private "user1" "Meeting at 2"))
                 "admin")
                "Function failed"))
; STRATEGY: Data Decomposition on w: World
(define (handle-bksp w)
  (local
    (
     (define ed (world-editor w)))
    (if (empty? (editor-pre ed))
        w
        (make-world
         (update-cursor
          (make-editor (except-last (editor-pre ed))
                       (editor-post ed) (editor-move ed))
          (image-width (text->img (last (editor-pre ed)))))
         (world-user-list w) (world-event-list w) (world-user-name w)))))
; Test-suite:
(begin-for-test
  (check-equal? (handle-bksp INIT-WORLD)
                INIT-WORLD
                "true"))


; handle-left: World  -> World
; Handles "left" KeyEvent and moves the CURSOR 1 position left
; if its possible, otherwise returns same state
; EXAMPLES:
(begin-for-test
  (check-equal? (handle-left INIT-WORLD2)
                INIT-WORLD4
                "true"))
; STRATEGY: Data Decomposition on w: World
(define (handle-left w)
  (local
    (
     (define ed (world-editor w)))
    (if (zero? (length (editor-pre ed)))
        w
        (make-world
         (update-cursor
          (make-editor (except-last (editor-pre ed))
                       (cons (last (editor-pre ed))
                             (editor-post ed)) (editor-move ed)) -1)
         (world-user-list w) (world-event-list w) (world-user-name w)))))
; Test-suite:
(begin-for-test
  (check-equal? (handle-left INIT-WORLD)
                INIT-WORLD
                "true"))


; handle-right: World -> World
; Handles "right" KeyEvent and moves the CURSOR 1 position right
; if its possible, otherwise returns same state
; EXAMPLES:
(begin-for-test
  (check-equal? (handle-right INIT-WORLD)
                INIT-WORLD
                "true")
   (check-equal? (handle-right (make-world
                               ED1
                               (list "admin" "user1" "usr2")
                               (list 
                                (list 'private "user1" "Meeting at 2"))
                               "admin"))
                (make-world
                 (make-editor (list "a" "b" "c") (list "d") 7)
                 (list "admin" "user1" "usr2")
                 (list (list 'private "user1" "Meeting at 2"))
                 "admin")
                "Function failed")
  (check-equal? (handle-right (make-world
                               (make-editor (list "c" "d") empty 1)
                               (list "admin" "user1" "usr2")
                               (list (list 'private "user1" "Meeting at 2"))
                               "admin"))
                (make-world
                 (make-editor (list "c" "d") empty 1)
                 (list "admin" "user1" "usr2")
                 (list (list 'private "user1" "Meeting at 2"))
                 "admin")
                "Function failed"))
; STRATEGY: Data Decomposition on w: World
(define (handle-right w)
  (local
    (
     (define ed (world-editor w)))
    (if (zero? (length (editor-post ed)))
        w
        (make-world
         (update-cursor
          (make-editor (add-at-last (first (editor-post ed)) (editor-pre ed))
                       (rest (editor-post ed)) (editor-move ed)) 1)
         (world-user-list w) (world-event-list w) (world-user-name w)))))
; Test-suite:
(begin-for-test
  (check-equal? (handle-right INIT-WORLD)
                INIT-WORLD
                "true"))


; update-editor: World KeyEvent -> World
; Accepts World and KeyEvent and returns new World
; STRATEGY: Data Decomposition on w: World
(define (update-editor w ke)
  (local
    (
     (define ed (world-editor w)))
    (make-world
     (update-cursor
      (make-editor
       (add-at-last ke (editor-pre ed))
       (editor-post ed) (editor-move ed)) 1)
     (world-user-list w) (world-event-list w) (world-user-name w))))


; update-cursor: Editor Number -> Editor
; INTERP: val lies in [-1, 300]
; returns an Editor with updated editor-move
; STRATEGY: Function Composition
(define (update-cursor ed val)
  (local
    (
     (define new-val
       (cond         
         [(> val 1)
          (max (- (editor-move ed) val)
               CURSOR-WIDTH)]
         
         [(= val -1)
          (max (- (editor-move ed)
                  (image-width (text->img (first (editor-post ed)))))
               CURSOR-WIDTH)]
         
         [(= val 1)
          (min (+ (editor-move ed)
                  (image-width (text->img (last (editor-pre ed)))))
               CHAT-BOX-WIDTH)])))
    (make-editor (editor-pre ed) (editor-post ed) new-val)))



; except-last : ListOf<Any> -> ListOf<Any>
; Returns ListOf<Any> with the last character removed
; EXAMPLES
(begin-for-test
  (check-equal? (except-last '("h" "i" "s")) '("h" "i")
                "The result is hi" ))
;STRATEGY: Function Composition
(define (except-last lst)
  (cond
    [(empty? lst) empty]
    [else (reverse (rest (reverse lst)))]))
; Test-suite
(begin-for-test
  (check-equal? (except-last '("h"))
                empty
                "The result is ''")
  (check-equal? (except-last empty)
                empty
                "The result is ''"))

; add-at-last : Any ListOf<Any> -> ListOf<Any>
; Adds the item at the last of the given list
; EXAMPLES: 
(begin-for-test
  (check-equal? (add-at-last 3 (list 0 1 2))
                (list 0 1 2 3)
                "Function failed"))
; STRATEGY: Function Composition
(define (add-at-last item lst)
  (append
   lst
   (list item)))

; get-users : World -> ListOf<UserName>
; Returns a list of current chat participants, in lexicographic order.
; EXAMPLES :
(begin-for-test 
  (check-equal? (get-users (make-world
                 ED1
                 (list "rktusr2" "admin" "user1" "usr2" "test-user" "usr4")
                 (list (list 'private "user1" "Meeting at 2"))
                 "admin"))
                (list "rktusr2" "admin" "user1" "usr2" "test-user" "usr4")
                "Function failed"))
; STRATEGY : Data Decomposition on w : World
(define (get-users w)
  (world-user-list w))

; get-editor : World -> Editor
; Returns a representation of the chat client's input area.
; EXAMPLES:
(begin-for-test 
  (check-equal? (get-editor (make-world
                 ED1
                 (list "rktusr2" "admin" "user1" "usr2" "test-user" "usr4")
                 (list (list 'private "user1" "Meeting at 2"))
                 "admin"))
                (make-editor (list "a" "b") (list "c" "d") 1)
                "Function failed"))
; STRATEGY : Data Decomposition on w : World
(define (get-editor w)
  (world-editor w))

; get-editor-pre : Editor -> String
; Returns an editor's content before the cursor.
; EXAMPLES :
(begin-for-test
  (check-equal? (get-editor-pre ED1)
                "ab"
                "Function failed"))
; STRATEGY : Data Decomposition on ed : Editor
(define (get-editor-pre ed)
  (implode (editor-pre ed)))

; get-editor-post : Editor -> String
; Returns an editor's content after the cursor.
; EXAMPLES :
(begin-for-test
  (check-equal? (get-editor-post ED1)
                "cd"
                "Function failed"))
; STRATEGY : Data Decomposition on ed : Editor
(define (get-editor-post ed)
  (implode (editor-post ed)))

; last : ListOf<Any> -> Any
; Returns the last item in a list
; EXAMPLES:
(begin-for-test
  (check-equal? (last (list 0 1 2 3 4))
                4
                "Function failed")
  (check-equal? (last empty)
                empty
                "Function failed"))
; STRATEGY : Data Decomposition on lst : ListOf<Any>
(define (last lst)
  (if (empty? lst)
      empty
      (first (reverse lst))))

; get-chat-history : World -> ListOf<String>
; Returns a list of chat events, rendered to string, 
; where each string format is the same as when the event is
; rendered to the chat window, except the string should not be broken into
; multiple lines.
; EXAMPLES :
(begin-for-test
  (check-equal? (get-chat-history (make-world
                                   ED1
                                   (list "rktusr2" "admin" "user1" 
                                         "usr2" "test-user" "usr4")
                                   (list 
                                    (list 'join "usr7")
                                    (list 'join "usr8")
                                    (list 'join "usr9")
                                    (list 'join "usr10")
                                    (list 'join "usr11")
                                    (list 'join "usr12")
                                    (list 'private "user1" 
                                          "When is the meeting")
                                    (list 'private "user2" "Meeting at 2")
                                    (list 'private "user1" "Alright! Thanks")
                                    (list 'broadcast "user2" "meeting room 431 
booked for the afternoon")
                                    (list 'leave "usr7")
                                    (list 'leave "usr8")
                                    (list 'leave "usr9")
                                    (list 'private "user1" 
                                          "How long is the meeting")
                                    (list 'private "user2" "For about 2 hours")
                                    (list 'error "system down for maintenence"))
                                   "admin"))
                (list
                 "usr7 joined the chat."
                 "usr8 joined the chat."
                 "usr9 joined the chat."
                 "usr10 joined the chat."
                 "usr11 joined the chat."
                 "usr12 joined the chat."
                 "< user1 > When is the meeting"
                 "< user2 > Meeting at 2"
                 "< user1 > Alright! Thanks"
                 "< user2 > meeting room 431 \nbooked for the afternoon"
                 "usr7 left the chat."
                 "usr8 left the chat."
                 "usr9 left the chat."
                 "< user1 > How long is the meeting"
                 "< user2 > For about 2 hours"
                 "system down for maintenence")
                "Function failed"))   
; STRATEGY : Data Decomposition on w : World
(define (get-chat-history w)
  (map chat-event (world-event-list w)))

; chat-event : MsgToLog or MsgToServer -> String
; Returns the string equivalent of the events
; STRATEGY : Data Decomposition on event : MsgToLog of MsgToServer
(define (chat-event event)
  (cond
    [(join? event)
     (string-append (second event) " joined the chat.")]
    
    [(leave? event)
     (string-append (second event) " left the chat.")]
    
    [(error? event) (second event)]
    
    [(broadcast? event)
     (string-append "< " (second event) " > " (third event))]
    
    [else
     (string-append "< " (second event) " > " (third event))]))





