;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Chirper) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Problem 1 of the Final Project for How To Code: Complex Data
;; "Chirper"

;; Data Definitions:

(define-struct user (name verified? following))
;; User is (make-user String Boolean listOf(user))
;; Interp A user is ("his name" "Whether or not they verified"
;;                   "a list of other useres that he follows")
;; Note Chirper is a bunch of users that follow each other
;; So there is no need to an independent data definition for chirper

(define chirp1 (shared
                   ((-1- (make-user "Hayam"  true  (list -2- -3- -6-)))
                    (-2- (make-user "Karem"  false (list -1- -4-)))
                    (-3- (make-user "Nesma"  true   empty))
                    (-4- (make-user "Amr"    false (list -1- -3- -4-)))
                    (-5- (make-user "Norhan" true (list -1- -4-)))
                    (-6- (make-user "Eman"   false (list -4-))))
                 -1-))
(define chirp2 (shared
                   ((-1- (make-user "Hayam"  true  (list -2- -3- -6-)))
                    (-2- (make-user "Karem"  false (list -1- -4-)))
                    (-3- (make-user "Nesma"  true   (list -2- -6-)))
                    (-4- (make-user "Amr"    false (list -1- -3- -6-)))
                    (-5- (make-user "Norhan" true (list -1- -2- -5- -4- -6-)))
                    (-6- (make-user "Eman"   false (list -4- -6-))))
                 -1-))
#;
(define (fn-for-chirper u0)
  ;; todo is list of useres, to be visit
  ;; visited is list of names of the users that the function have visited
  (local [(define (fn-for-user u todo visited)
            (if (member (user-name u) visited)
                (fn-for-lou todo visited)
                (fn-for-lou (append (user-following u) todo)
                            (cons (user-name u) visited))))
          (define (fn-for-lou todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-user (first todo) (rest todo) visited)]))]
    (fn-for-user u0 empty empty)))

(define-struct pair (user followers))
;; Pair is (make-pair user Natural)
;; Interp. a user with the number of people that follows him
(define p1 (make-pair chirp1 4))
(define p2 (make-pair chirp2 5))
;; Note examples dosen't express the real number of usere for any of chirps
#;
(define (fn-for-pair p)
  (... (pair-user r)
       (pair-followers p)))


;; Functions

;; User -> User
;; returns the user with maximum followers
;(define (most-followers u) (make-user "" false empty))
(check-expect (most-followers chirp1)
              (shared ((-0-
          (make-user "Amr" #false (list -3- -11- -0-)))
         (-11-
          (make-user "Nesma" #true '()))
         (-3-
          (make-user
           "Hayam"
           #true
           (list
            (make-user
             "Karem"
             #false
             (list -3- -0-))
            -11-
            (make-user
             "Eman"
             #false
             (list -0-))))))
  -0-))
(check-expect (most-followers chirp2)
            (shared ((-1-
          (make-user
           "Eman"
           #false
           (list -4- -1-)))
         (-10-
          (make-user
           "Karem"
           #false
           (list -7- -4-)))
         (-15-
          (make-user
           "Nesma"
           #true
           (list -10- -1-)))
         (-4-
          (make-user
           "Amr"
           #false
           (list -7- -15- -1-)))
         (-7-
          (make-user
           "Hayam"
           #true
           (list -10- -15- -1-))))
  (make-pair -1- 4)))
(define (most-followers u0)
  ;; todo is list of useres, to be visit
  ;; visited is list of names of the users that the function have visited
  ;; list-of-followers is list of pairs (user followers)
  (local [(define chirp u0) 
          (define (fn-for-user u todo visited list-of-followers)
            (local [;; User -> Pair
                    ;; generates a pair of the user and num-of-followers
                    (define (generate-followers u0)
                      (local [(define (fn-for-user u_ todo visited acc)
                                (if (member (user-name u_) visited)
                                    (fn-for-lou todo visited acc)
                                    (fn-for-lou (append (user-following u_) todo)
                                                (cons (user-name u_) visited)
                                                (if (member u (user-following u_))
                                                    (add1 acc) acc))))
                              (define (fn-for-lou todo visited acc)
                                (cond [(empty? todo) (make-pair u acc)]
                                      [else
                                       (fn-for-user (first todo) (rest todo) visited acc)]))]
                        (fn-for-user u0 empty empty 0)))]
              (if (member (user-name u) visited)
                  (fn-for-lou todo visited list-of-followers)
                  (fn-for-lou (append (user-following u) todo)
                              (cons (user-name u) visited)
                              (cons (generate-followers chirp)
                                    list-of-followers)))))
          (define (fn-for-lou todo visited list-of-followers)
            (cond [(empty? todo) (find-max list-of-followers)]
                  [else
                   (fn-for-user (first todo) (rest todo) visited
                                list-of-followers)]))]
    (fn-for-user u0 empty empty empty)))

;; ListOf(User) -> User
;; returns the user with the max num
;; Note: in case of repeated max the first one is returned
(check-expect (find-max (list p1 p2)) p2)
              ;(define (find-max lou ) 0)
              (define (find-max lou0)
                ;; acc is pair, with the room to which are the most exits
                (local [(define (fn-for-lou  lou  acc)
                          (cond [(empty? lou ) acc]
                                [(> (pair-followers (first lou )) (pair-followers acc))
                                 (fn-for-lou  (rest lou ) (first lou ))]
                                [else (fn-for-lou  (rest lou ) acc)]))]
                  (fn-for-lou  lou0 (make-pair "" 0))))

              