;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sched-general) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
;(define TIME-ON-TASK 2) 

(provide (all-defined-out))


; A Time is a Symbol
; Represents a day of week and time.
; Data examples
(define TUE135 '1:35pmTues)
(define TUE325 '3:25pmTues)

; A StudentID is a Symbol
; Represents a student (or pair) via their ccs ID(s).
; Data examples
(define studentId 'John)

; A CodeWalk is a (make-codewalk Time ListOf<StudentID> PosInt)
; Represents a codewalk with time, assigned students, and a max capacity.
(define-struct codewalk (time students max))

; Template
; CodeWalk -> ??
;(define (codewalk-fn x)
;  ( ... (codewalk-time x) ... (codewalk-students x) ... (codewalk-max x)))

; Data examples
(define CW-TUE135 (make-codewalk TUE135 empty 1))
(define CW-TUE325 (make-codewalk TUE325 empty 1))

; A CodeWalks is a ListOf<CodeWalk>
; Represents a codewalk schedule.

; Data examples
(define CW-List (list CW-TUE135 CW-TUE325))

; A Preferences is a ListOf<Time>
; Represents a list of code walk times.

; Data examples
(define PREF (list '1:35pmTues '3:25pmTues))

; A StudentUnavail is a (make-student StudentID Preferences)
; Represents a student and their unavailable times.

; A StudentAvail is a (make-student StudentID Preferences)
; Represents a student and their available times, most-preferred first.
; An unlisted time means the student is unavailable.

(define-struct student (id prefs))

; Template
; Student -> ??
;(define (student-fn x)
;  ( ... (student-id x) ... (student-prefs x) ...))

; Data examples
(define studentUnavail (make-student 'A (list TUE135)))
(define studentAvail (make-student 'B (list TUE135 TUE325)))

; A Student is one of:
; - StudentUnavail
; - StudentAvail

; A StudentUnavails is a ListOf<StudentUnavail>
; WHERE: there are no duplicate StudentIDs
; Data examples
(define SU-List (list studentUnavail))

; A StudentAvails is a ListOf<StudentAvail>
; WHERE: there are no duplicate StudentIDs
; Data examples
(define SA-List (list studentAvail))

; A Students is a ListOf<Student>
; WHERE: there are no duplicate StudentIDs
; Data examples
(define Student-List (list studentAvail studentUnavail))


;;==============================================================================

; cwExceedsCapacity? : CodeWalks -> Boolean
; Returns true if any of the code walk in cws is over capacity
; Strategy : function composition
(begin-for-test
  (check-equal? (cwExceedsCapacity? (list (make-codewalk 'T1 '(A B) 2) 
                                          (make-codewalk 'T1 '(B C) 1)))
                #true
                "Second codewalk exceeds capacity"))

(define (cwExceedsCapacity? cws)
  (ormap (λ (x) (> (length (codewalk-students x)) (codewalk-max x))) cws))

;;==============================================================================

; scheduledOnce? : CodeWalks -> Boolean
; Returns true if each student is scheduled exactly once
; Strategy : data decomposition on cws : CodeWalks
(begin-for-test
  (check-equal? (scheduledOnce? (list (make-codewalk 'T1 '(A B) 1) 
                                      (make-codewalk 'T1 '(B C) 1)))
                #false
                "Student B scheduled twice")
  (check-equal? (scheduledOnce? (list (make-codewalk 'T1 '(A B) 1) 
                                      (make-codewalk 'T1 '(D C) 1)))
                #true
                "All students scheduled once"))

(define (scheduledOnce? cws)
  (cond
    [(empty? cws) #true]
    [else (and (listScheduledOnce? (first cws) (rest cws))
               (scheduledOnce? (rest cws)))]))

;;==============================================================================

; listScheduledOnce? : CodeWalk CodeWalks -> Boolean
; Returns true if students of given codewalk cw are not
; memebers of other codewalks
; STRATEGY : function composition
(begin-for-test
  (check-equal? (listScheduledOnce? (make-codewalk 'T1 '(A B) 1)
                                (list (make-codewalk 'T1 '(B C) 1)))
                #false
                "Student B scheduled twice"))

(define (listScheduledOnce? cw loLos)
  (andmap (λ (x) (not (memberOfOtherCW? x loLos))) (codewalk-students cw)))

;;==============================================================================

; memberOfOtherCW? : StudentId CodeWalks -> Boolean
; Returns true if given student is not a memeber of other codewalks
; STRATEGY : function composition
(begin-for-test
  (check-equal? (memberOfOtherCW? 'A (list (make-codewalk 'T1 '(B C) 1)))
                #false
                "Student A scheduled once"))

(define (memberOfOtherCW? student loCW)
  (ormap (λ (x) (member? student (codewalk-students x))) loCW))

;;==============================================================================

; getSchedTimeForStudent : StudentID CodeWalks -> Maybe<Time>
; Returns scheduled code walk time for given student, #f if not scheduled
; STRATEGY : function composition
(begin-for-test
  (check-equal? (getSchedTimeForStudent  'A
                                         (list (make-codewalk 'T1 '(A B) 2) 
                                               (make-codewalk 'T1 '(B C) 2)))
                'T1
                "Scheduled time for student 'A")
  (check-equal? (getSchedTimeForStudent  'E
                                         (list (make-codewalk 'T1 '(A B) 2) 
                                               (make-codewalk 'T1 '(B C) 2)))
                #false
                "Student 'E not scheduled"))
(define (getSchedTimeForStudent student cws)
  (local
    (;getSchedTime : Maybe<CodeWalk> -> Maybe<Time>
     ;Returns the code walk time if cw is not boolean
     ;STRATEGY : data decomposition on cw : CodeWalk
     (define (getSchedTime cw)
       (if (boolean? cw) #false
        (codewalk-time cw))))
    
    (getSchedTime (getSchedCWForStudent student cws))))

;;==============================================================================

; getSchedCWForStudent : StudentID CodeWalks -> Maybe<CodeWalk>
; Returns codewalk in which given student is scheduled, #f if not scheduled
; STRATEGY : function composition
(begin-for-test
  (check-equal? (getSchedCWForStudent 'A
                                      (list (make-codewalk 'T1 '(A B) 2) 
                                            (make-codewalk 'T1 '(B C) 2)))
                (make-codewalk 'T1 '(A B) 2)
                "Scheduled time for student 'A"))

(define (getSchedCWForStudent student cws)
  (local 
    (; getCw : CodeWalks -> Maybe<CodeWalk>
     ; Returns first of the codewalk list if it is not empty
     ; Strategy: data decomposition on loCw : CodeWalks
     (define (getCw loCw)
       (cond
         [(empty? loCw) #false]
         [else (first loCw)])))
    
    (getCw (filter (λ (x) (member? student (codewalk-students x))) cws))))

;;==============================================================================

; schedule/general : Students CodeWalks 
;                   [Students CodeWalks -> Boolean]
;                   [Student CodeWalks -> Preferences] -> Maybe<CodeWalks>
; Creates a codewalk schedule by assigning students to cws, 
; while satisfying students' constraints.
; Returns #f if no schedule is possible.
; STRATEGY: function composition

(define (schedule/general students cws chkFn validSlotsFn) 
  (local
    (; schedule/general/0 : CodeWalks Natural -> Maybe<CodeWalks>
     ; Creates a codewalk schedule by assigning student at given position
     ; to cws, while statisfying student's constraints
     ; WHERE position holds the difference between the number of elements 
     ; processed from the students
     ; Strategy : Generative recursion
     ; Halting Measure : Position keeps incrementing, once it reaches
     ; length of students, this function terminates
     (define (schedule/general/0 cws position)
       (cond
         [(equal? position (length students)) cws]
         [else 
          (local ((define valid-slots (validSlotsFn 
                                       (list-ref students position) cws))
                  (define newCws#f (sched-student valid-slots cws position)))
            (if (boolean? newCws#f) #false newCws#f))]))
     
     ; sched-student: Preferences CodeWalks Natural -> Maybe<CodeWalks>
     ; Assigns student to some time in valid-slots, returns #f if not possible
     ; Strategy : data decomposition on valid-slots : Preferences
     (define (sched-student valid-slots cws position)   
       (cond
         [(empty? valid-slots) #false]
         [else (local ((define newCws (add-student (list-ref students position) 
                                                   (first valid-slots) cws))
                       (define matches#f (schedule newCws position)))
                 
                 (if (boolean? matches#f)
                     (sched-student (rest valid-slots) cws position)
                     matches#f))]))
     
     ; schedule : CodeWalks Natural -> Maybe<CodeWalks>
     ; If given codewalk schedule is ok, schedules next student, else returns #f
     ; Strategy : function composition
     (define (schedule cws position)
       (if (chkFn (take students position) cws) 
           (schedule/general/0 cws (add1 position))
           #false)))
    
    (schedule/general/0 cws 0)))

;;==============================================================================

; add-student: Student Time CodeWalks -> CodeWalks
; Returns new codewalks with given student added at given cw slot
; Strategy : data decomposition on student : Student
(begin-for-test
  (check-equal? (add-student (make-student 'E '(T1)) 'T1 
                             (list (make-codewalk 'T1 '(A B) 3) 
                                   (make-codewalk 'T1 '(B C) 2)))
                (list (make-codewalk 'T1 (list 'E 'A 'B) 3))
                "Student 'E added to codewalk 'T1"))

(define (add-student student slot cws)
  (local
    ((define matchVal (first (filter (λ (x) (symbol=? slot (codewalk-time x)))
                                     cws)))
     (define non-match (filter (λ (x) (not (symbol=? slot (codewalk-time x))))
                               cws)))
    
    (cons (addStudentToCW (student-id student) matchVal) non-match)))

;;==============================================================================

; addStudentToCW: StudentID CodeWalk -> CodeWalk
; Adds given studentId to the list of studentId in given codewalk cw
; Strategy: data decomposition on cw : CodeWalk
(begin-for-test
  (check-equal? (addStudentToCW 'E (make-codewalk 'T1 '(A B) 3))
                (make-codewalk 'T1 (list 'E 'A 'B) 3)
                "Student 'E added to codewalk 'T1"))

(define (addStudentToCW studentId cw)
  (make-codewalk (codewalk-time cw)
                 (cons studentId (codewalk-students cw))
                 (codewalk-max cw)))   

;;==============================================================================

; take: ListOf<X> Natural -> ListOf<X>
; Returns first 'count' elements from given lox
; Strategy : function composition
(begin-for-test
  (check-equal? (take (list (make-codewalk 'T1 '(A B) 3) 
                            (make-codewalk 'T1 '(B C) 2)) 0)
                (list (make-codewalk 'T1 (list 'A 'B) 3))
                "Returns first argument of the given list"))

(define (take lox count)
  (local
    (; take/0: ListOf<X> Natural -> ListOf<X>
     ; Returns elements whose position is below count
     ; WHERE position holds the position of the first element of lox/0 in lox
     ; Strategy: data decomposition on lox/0 : ListOf<X>
     (define (take/0 lox/0 position/0)
       (if (> position/0 count) '()
           (cons (first lox/0) (take/0 (rest lox/0) (add1 position/0))))))
    
    (take/0 lox 0)))

;;==============================================================================
