;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sched-with-unavail) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(require "sched-general.rkt")
(define TIME-ON-TASK 10)

(provide schedule/unavail-ok?)
(provide schedule/unavail)

;;==============================================================================

; schedule/unavail-ok? : StudentUnavails CodeWalks -> Boolean
; Returns true if cws is a valid schedule according to the given
; student preferences.
; Strategy : function composition
(begin-for-test
  (check-equal? (schedule/unavail-ok? (list (make-student 'A (list TUE135)) 
                                            (make-student 'B (list TUE135))
                                            (make-student 'C (list TUE325)))
                                      (list (make-codewalk TUE325 '(A B) 2) 
                                            (make-codewalk TUE135 '(C) 1)))
                #true
                "Meets all criteria")
  (check-equal? (schedule/unavail-ok? (list (make-student 'A (list TUE135)) 
                                            (make-student 'G (list TUE135))
                                            (make-student 'C (list TUE325)))
                                      (list (make-codewalk TUE325 '(A B) 2) 
                                            (make-codewalk TUE135 '(C) 1)))
                #false
                "Unassigned student"))

(define (schedule/unavail-ok? students cws) 
  (and (not (cwExceedsCapacity? cws)) 
       (scheduledOnce? cws) 
       (acceptableTimeSlot? students cws)))

;;==============================================================================

; acceptableTimeSlot? : StudentUnavails CodeWalks -> Boolean
; Returns true if each student is scheduled in acceptable time slot
; Strategy: function composition
(begin-for-test
  (check-equal? (acceptableTimeSlot? (list (make-student 'A (list TUE135)) 
                                           (make-student 'B (list TUE135))
                                           (make-student 'C (list TUE325)))
                                     (list (make-codewalk TUE325 '(A B) 2) 
                                           (make-codewalk TUE135 '(C) 1)))
                #true
                "All students in their acceptable time slots"))

(define (acceptableTimeSlot? students cws)
  (andmap (λ (x) 
            (local
              ((define schedTime (getSchedTimeForStudent (student-id x) cws)))
              (if (boolean? schedTime) #false
                  (not (member? schedTime (student-prefs x)))))) 
          students))

;;==============================================================================
; schedule/unavail : StudentUnavails CodeWalks -> Maybe<CodeWalks>
; Creates a codewalk schedule by assigning students to cws, 
; while satisfying students' constraints.
; Returns #f if no schedule is possible.
; Strategy: function composition

(begin-for-test
  (check-equal? (schedule/unavail (list (make-student 'A (list 'T1))
                                        (make-student 'B (list 'T2))
                                        (make-student 'C (list 'T3))
                                        (make-student 'D (list 'T1))
                                        (make-student 'E (list 'T2)))
                                  (list (make-codewalk 'T1 '() 2)
                                        (make-codewalk 'T2 '() 2)
                                        (make-codewalk 'T3 '() 1)))
                (list
                 (make-codewalk 'T3 (list 'E) 1)
                 (make-codewalk 'T2 (list 'D 'A) 2)
                 (make-codewalk 'T1 (list 'C 'B) 2))
                "Schedules students according to their preference")
  (check-equal? (schedule/unavail (list (make-student 'A (list 'T1))
                                        (make-student 'B (list 'T2))
                                        (make-student 'C (list 'T3))
                                        (make-student 'D (list 'T1))
                                        (make-student 'E (list 'T3 'T2)))
                                  (list (make-codewalk 'T1 '() 2)
                                        (make-codewalk 'T2 '() 2)
                                        (make-codewalk 'T3 '() 1)))
                (list
                 (make-codewalk 'T1 (list 'E 'B) 2)
                 (make-codewalk 'T3 (list 'D) 1)
                 (make-codewalk 'T2 (list 'C 'A) 2))
                "Schedules students according to their preference"))

(define (schedule/unavail students cws)
  (schedule/general students cws schedule/unavail-ok? get-valid-slots))

;;==============================================================================

; get-valid-slots: StudentAvail CodeWalks -> Preferences
; Returns acceptable time slots of the given student
; Strategy: function composition
(begin-for-test
  (check-equal? (get-valid-slots (make-student 'A (list 'T1))
                                 (list (make-codewalk 'T1 '() 2)
                                       (make-codewalk 'T2 '() 2)
                                       (make-codewalk 'T3 '() 1)))
                (list 'T2 'T3)
                "Valid time slots for student 'A"))

(define (get-valid-slots student cws)
  (filter (λ (x) (not (member? x (student-prefs student)))) 
          (available-cws-time cws)))

;;==============================================================================

; available-cws-time : CodeWalks -> Preferences
; Returns a list of all the codewalk time slots
; Strategy : function composition
(begin-for-test
  (check-equal? (available-cws-time (list (make-codewalk 'T1 '() 2)
                                          (make-codewalk 'T2 '() 2)
                                          (make-codewalk 'T3 '() 1)))
                (list 'T1 'T2 'T3)
                "Codewalk times"))

(define (available-cws-time cws)   
  (map (λ (x) (codewalk-time x)) cws))

;;==============================================================================













