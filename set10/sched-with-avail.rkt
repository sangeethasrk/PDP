;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sched-with-avail) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(require "sched-general.rkt")
(define TIME-ON-TASK 2)

(provide schedule/avail-ok?)
(provide schedule/avail)
(provide avg-choice)


;;==============================================================================
; schedule/avail-ok? : StudentAvails CodeWalks -> Boolean
; Returns true if cws is a valid schedule according to the given
; student preferences.
; Strategy: function composition
(begin-for-test
  (check-equal? (schedule/avail-ok? (list (make-student 'A (list 'T2)) 
                                          (make-student 'B (list 'T2))
                                          (make-student 'C (list 'T1)))
                                    (list (make-codewalk 'T2 '(A B) 2) 
                                          (make-codewalk 'T1 '(C) 1)))
                #true
                "Meets all criteria")
  (check-equal? (schedule/avail-ok? (list (make-student 'A (list 'T2)) 
                                          (make-student 'G (list 'T2))
                                          (make-student 'C (list 'T1)))
                                    (list (make-codewalk 'T2 '(A B) 2) 
                                          (make-codewalk 'T1 '(C) 1)))
                #false
                "Unassigned student"))

(define (schedule/avail-ok? students cws) 
  (and (not (cwExceedsCapacity? cws)) 
       (scheduledOnce? cws) 
       (acceptableTimeSlot? students cws)))

;;==============================================================================

; acceptableTimeSlot? : StudentUnavails CodeWalks -> Boolean
; Returns true if each student is scheduled in acceptable time slot
; Strategy: function composition
(begin-for-test
  (check-equal? (acceptableTimeSlot? (list (make-student 'A (list 'T2)) 
                                           (make-student 'B (list 'T2))
                                           (make-student 'C (list 'T1)))
                                     (list (make-codewalk 'T2 '(A B) 2) 
                                           (make-codewalk 'T1 '(C) 1)))
                #true
                "All students in their acceptable time slots"))

(define (acceptableTimeSlot? students cws)
  (andmap (λ (x) 
            (local
              ((define schedTime (getSchedTimeForStudent (student-id x) cws)))
              (if (boolean? schedTime) #false
                  (member? schedTime (student-prefs x))))) 
          students))

;;==============================================================================

; schedule/avail : StudentAvails CodeWalks -> Maybe<CodeWalks>
; Creates a codewalk schedule by assigning students to cws, 
; while satisfying students' constraints.
; Returns #f if no schedule is possible.
; Strategy: function composition
(begin-for-test
  (check-equal? (schedule/avail (list (make-student 'A (list 'T2 'T3))
                                      (make-student 'B (list 'T1 'T3))
                                      (make-student 'C (list 'T1 'T2))
                                      (make-student 'D (list 'T2 'T3))
                                      (make-student 'E (list 'T1 'T3)))
                                (list (make-codewalk 'T1 '() 2)
                                      (make-codewalk 'T2 '() 2)
                                      (make-codewalk 'T3 '() 1)))
                (list
                 (make-codewalk 'T3 (list 'E) 1)
                 (make-codewalk 'T2 (list 'D 'A) 2)
                 (make-codewalk 'T1 (list 'C 'B) 2))
                "Schedules students according to their preference")
  (check-equal? (schedule/avail (list (make-student 'A (list 'T2 'T3))
                                      (make-student 'B (list 'T1 'T3))
                                      (make-student 'C (list 'T1 'T2))
                                      (make-student 'D (list 'T2 'T3))
                                      (make-student 'E (list 'T1)))
                                (list (make-codewalk 'T1 '() 2)
                                      (make-codewalk 'T2 '() 2)
                                      (make-codewalk 'T3 '() 1)))
                (list
                 (make-codewalk 'T1 (list 'E 'B) 2)
                 (make-codewalk 'T3 (list 'D) 1)
                 (make-codewalk 'T2 (list 'C 'A) 2))
                "Schedules students according to their preference"))

(define (schedule/avail students cws) 
  (schedule/general students cws schedule/avail-ok? get-valid-slots))

;;==============================================================================

; get-valid-slots: StudentAvail CodeWalks -> Preferences
; Returns acceptable time slots of the given student
; Strategy: data decomposition on student: StudentAvail
(begin-for-test
  (check-equal? (get-valid-slots (make-student 'A (list 'T2 'T3))
                                 (list
                                  (make-codewalk 'T1 (list 'C 'B) 2)
                                  (make-codewalk 'T2 (list 'D 'A) 2)
                                  (make-codewalk 'T3 (list 'E) 1)))
                (list 'T2 'T3)
                "Valid time slots for student 'A"))

(define (get-valid-slots student cws)
  (student-prefs student))

;;==============================================================================

; avg-choice : StudentAvails CodeWalks -> PosReal
; Returns the average of the rank of the codewalks assigned to each student
; WHERE: (schedule/avail-ok? students cws) = #t
; STRATEGY : function composition
(begin-for-test
  (check-equal? (avg-choice (list (make-student 'A (list 'T2 'T3))
                                  (make-student 'B (list 'T1 'T3))
                                  (make-student 'C (list 'T1 'T2))
                                  (make-student 'D (list 'T2 'T3))
                                  (make-student 'E (list 'T3 'T1)))
                            (list
                             (make-codewalk 'T1 (list 'C 'B) 2)
                             (make-codewalk 'T2 (list 'D 'A) 2)
                             (make-codewalk 'T3 (list 'E) 1)))
                1
                "Meets first preference of all students")
  (check-equal? (avg-choice (list (make-student 'A (list 'T2 'T3))
                                  (make-student 'B (list 'T1 'T3))
                                  (make-student 'C (list 'T1 'T2))
                                  (make-student 'D (list 'T2 'T3))
                                  (make-student 'E (list 'T1 'T3)))
                            (list
                             (make-codewalk 'T1 (list 'C 'B) 2)
                             (make-codewalk 'T2 (list 'D 'A) 2)
                             (make-codewalk 'T3 (list 'E) 1)))
                1.2
                "Schedules students according to their preference"))

(define (avg-choice students cws) 
  (get-avg (map (λ(x) (get-rank (student-id x) (student-prefs x) cws)) 
                students)))

;;==============================================================================

; get-rank : StudentId Preferences CodeWalks -> Natural
; Returns rank for given student based on the allotted codewalk slot
; Strategy: function composition
(begin-for-test
  (check-equal? (get-rank 'A (list 'T4)
                          (list
                           (make-codewalk 'T1 (list 'C 'B) 2)
                           (make-codewalk 'T2 (list 'D 'A) 2)
                           (make-codewalk 'T3 (list 'E) 1)))
                0
                "Student schedules slot is other than provided codewalks"))

(define (get-rank id pref cws)
  (local
    ((define matcedCW (getSchedTimeForStudent id cws))
     
     ; get-rank/0 : Natural Preferences -> Natural
     ; Returns the position of matching codewalk slot in the preferences list
     ; WHERE position holds the difference between the number of elements 
     ; processed between pref and pref/0
     ; Strategy: data decomposition on pref/0: Preferences
     (define (get-rank/0 position pref/0)
       (cond
         [(empty? pref/0) 0]
         [else (if (equal? (first pref/0) matcedCW) 
                   position
                   (get-rank/0 (add1 position) (rest pref/0)))])))
    
    (get-rank/0 1 pref)))

;;==============================================================================

; get-avg : ListOf<Natural> -> PosReal
; Returns average of ranks of all the students
; Strategy: function composition
(begin-for-test
  (check-equal? (get-avg (list 1 1 1 1 2))
                1.2
                "Average rank"))

(define (get-avg loRank)
  (/ (foldr + 0 loRank) (length loRank)))

;;==============================================================================







