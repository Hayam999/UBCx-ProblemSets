;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Graphs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Data Definition for Graphs

(define-struct room (name exits))
;; Room is (make-room String listOf(Room))
;; Interp. the room's name, and list of room that the exits lead to
(define H1 (make-room "A" (list (make-room "B" empty))))
(define H2 (shared ((-0-
               (make-room "A" (list (make-room "B" (list -0-))))))
             -0-))
(define H3 (shared ((-0-
                    (make-room "A"
                               (list (make-room "B"
                                                (list
                                                 (make-room "C" (list -0-))))))))
             -0-))
(define H4 (shared ((-A- (make-room "A" (list -B- -D-)))
                    (-B- (make-room "B" (list -C- -E-)))
                    (-C- (make-room "C" (list -B-)))
                    (-D- (make-room "D" (list -E-)))
                    (-E- (make-room "E" (list -F- -A-)))
                    (-F- (make-room "F" empty))) -A-))
#;
(define (fn-for-room r0)
  (local [(define (fn-for-room r todo visited)
            (if (member (room-name r) visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited))))
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-room (first todo) (rest todo) visited)]))]
    (fn-for-room r0 empty empty)))


;; Functions

;; Room Sting -> Boolean
;; Produce true if we can enter the given room-name starting from the given room
(check-expect (reachable? H1 "A") true)
(check-expect (reachable? H1 "B") true)
(check-expect (reachable? H1 "C") false)
(check-expect (reachable? H4 "F") true)
;(define (reachable? r n) false)

(define (reachable? r0 n)
  (local [(define (fn-for-room r todo visited)
            (if (string=? (room-name r) n) true
                (if (member (room-name r) visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)))))
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-room (first todo) (rest todo) visited)]))]
    (fn-for-room r0 empty empty)))