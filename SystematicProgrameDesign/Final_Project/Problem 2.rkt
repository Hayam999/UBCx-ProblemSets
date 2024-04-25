;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Problem 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;  PROBLEM 2:
;
;  In UBC's version of How to Code, there are often more than
;  800 students taking
;  the course in any given semester,
;  meaning there are often over 40 Teaching Assistants.
;
;  Designing a schedule for them by
;hand is hard work - luckily we've learned enough now to write
;  a program to do it for us!
;
;  Below are some data definitions for a simplified version of
; a TA schedule. There are some
;  number of slots that must be filled,
; each represented by a natural number. Each TA is
;  available for some of these slots,
;and has a maximum number of shifts they can work.
;
;  Design a search program that consumes a list of TAs
;  and a list of Slots, and produces one
;  valid schedule where each Slot is assigned to a TA,
;  and no TA is working more than their
;  maximum shifts. If no such schedules exist, produce false.
;
;  You should supplement the given check-expects
; and remember to follow the recipe!


;; Slot is Natural
;; interp. each TA slot has a number, is the same length,and none overlap

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))
(define NOODLE-TAs (list SOBA UDON RAMEN))
(define ERIKA (make-ta "Erika" 1 (list 1 3 7 9)))
(define RYAN (make-ta "Ryan" 1 (list 1 8 10)))
(define REECE (make-ta "Reece" 1 (list 5 6)))
(define GORDON (make-ta "Gordon" 2 (list 2 3 9)))
(define DAVID (make-ta "David" 2 (list 2 8 9)))
(define KATIE (make-ta "Katie" 1 (list 4 6)))
(define AASHISH (make-ta "Aashish" 2 (list 1 10)))
(define GRANT (make-ta "Grant" 2 (list 1 11)))
(define RAEANNE (make-ta "Raeanne" 2 (list 1 11 12)))
(define ALEX (make-ta "Alex" 1 (list 7)))
(define ERIN (make-ta "Erin" 1 (list 4)))
(define QUIZ-TAs-1 (list ERIKA RYAN REECE GORDON DAVID KATIE AASHISH GRANT RAEANNE))
(define QUIZ-TAs-2 (list ERIKA RYAN REECE GORDON DAVID KATIE AASHISH GRANT RAEANNE ALEX))
(define QUIZ-TAs-3 (list ERIKA RYAN REECE GORDON DAVID KATIE AASHISH GRANT RAEANNE ERIN))



(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)


;; ============================= FUNCTIONS


;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible

;(define (schedule-tas tas slots) false)
(check-expect (solve empty empty) empty)
(check-expect (solve empty (list 1 2)) false)
(check-expect (solve (list SOBA) empty) empty)
(check-expect (solve (list SOBA) (list 1))
              (list (make-assignment SOBA 1)))
(check-expect (solve (list SOBA) (list 2)) false)

(check-expect (solve (list SOBA) (list 1 3))
              (list (make-assignment SOBA 1)
                    (make-assignment SOBA 3)))

(check-expect (solve NOODLE-TAs (list 1 2 3 4))
              (list
               (make-assignment SOBA 1)
               (make-assignment SOBA 3)
               (make-assignment UDON 4)
               (make-assignment RAMEN 2)))
(check-expect (solve NOODLE-TAs (list 1 2 3 4 5)) false)

(define (solve tas0 slots0)
  ;; schedule is list of assignments that succesfully made
  ;; slot-visits is the tas that the slots have visited and didn't find
  ;; a place for it there
  (local [(define (schedule-tas tas slots schedule slot-visits)
            (cond [(empty? slots) schedule]
                  [(= (length tas) (length slot-visits)) false]
                  [else
                   (local [(define ta (first tas))
                           ;; listOf(Ta) -> listOf(Ta)
                           ;; filter tas from any ta with max of 0
                           (define (filter-tas tas)
                             (filter (lambda (t) (>= (ta-max t) 1))
                                     tas))]
                       (if (and (member (first slots) (ta-avail ta))
                                (>= (ta-max ta) 1))
                           (schedule-tas (filter-tas (append (rest tas) (list (make-ta (ta-name ta)
                                                        (sub1 (ta-max ta))
                                                        (ta-avail ta)))))
                                         (rest slots)
                                         (cons (make-assignment ta
                                                                (first slots))
                                               schedule) empty)
                           (schedule-tas
                            (filter-tas (append (rest tas) (list (first tas))))
                            slots schedule (cons ta slot-visits))))]))]
    (schedule-tas tas0 slots0 empty empty)))
  







