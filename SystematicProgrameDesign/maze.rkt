;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname maze) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; maze-problem-set
(require 2htdp/image)

;; Constants:
(define HEIGHT 500)
(define WIDTH HEIGHT)
(define MTS (empty-scene WIDTH HEIGHT "gray"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data Definitions:


(define-struct pos (x y))
;; Pos is (make-pos Number Number)
;; Interp. X and Y are numbers that represents x-axis and y-axis respectfuly
(define up-l   (make-pos 0 0))
(define up-r   (make-pos WIDTH 0))
(define down-l (make-pos 0 HEIGHT))
(define down-r (make-pos WIDTH HEIGHT))   ; positions for ribs
(define x-start1 (make-pos 50 0))
(define x-end1   (make-pos 150 0))
(define y-start1 (make-pos 50 100))
(define y-end1   (make-pos 150 100))
(define x-start2 (make-pos 75 100))
(define x-end2 x-end1)
(define y-start2 (make-pos 75 300))
(define y-end2 (make-pos 150 300))
#;
(define (fn-for-pos p)
  (... (pos-x p)
       (pos-y p)))

(define-struct rib (start end))
;; rib is (make-rib pos pos)
;; Interp. (make-rib start end) is a specific rib that has
;; start point and end points both of them are positions on the screen
(define x-rib1 (make-rib x-start1 x-end1))
(define y-rib1 (make-rib y-start1 y-end1))
(define x-rib2 (make-rib x-start2 x-end2))
(define y-rib2 (make-rib y-start2 y-end2))
#;
(define (fn-for-rib r)
  (... (fn-for-pos (rib-start r))
       (fn-for-pos (rib-end r))))


(define-struct room (up-left up-right down-left down-right))
;; Room is (make-room pos pos pos pos)
;; Interp (make-room up-left up-right down-left down-right) is:
;; Positions that makes up the square of the whole maze
(define room1 (make-room up-l up-r down-l down-r))
#;
(define (fn-for-room r)
  (... (fn-for-pos (room-up-left    r))
       (fn-for-pos (room-up-right   r))
       (fn-for-pos (room-down-left  r))
       (fn-for-pos (room-down-right r))))


(define-struct black-block (x-rib y-rib))
;; Black-block is (make-black-block rib rib)
;; Interp. A black-block is a specific area on the maze which is
;; forbidden to move within it
(define BB1 (make-black-block x-rib1 y-rib1))
(define BB2 (make-black-block x-rib2 y-rib2))
(define B1 (make-black-block (make-rib (make-pos 50 50)
                                       (make-pos 450 50))
                             (make-rib (make-pos 50 100)
                                       (make-pos 450 100))))
(define B2 (make-black-block (make-rib (make-pos 50 100)
                                       (make-pos 100 100))
                             (make-rib (make-pos 50 HEIGHT)
                                       (make-pos 100 HEIGHT))))
(define B3 (make-black-block (make-rib (make-pos 0 450)
                             (make-pos 50 450))
                             (make-rib (make-pos 0 HEIGHT)
                                       (make-pos 100 HEIGHT))))
(define B4 (make-black-block (make-rib (make-pos 300 400)
                                       (make-pos WIDTH 400))
                             (make-rib (make-pos 300 450)
                                       (make-pos WIDTH 450))))
#;
(define (fn-for-black-block b)
  (... (fn-for-rib (black-block-x-rib b))
       (fn-for-rib (black-block-y-rib b))))

;; Forbdn-area is:
;; - empty
;; - (cons Black-block Forbdn-area)
(define empty-fb-area empty)
(define fb-area0 (cons BB1 empty))
(define fb-area1 (cons BB2 fb-area0))
(define square-fb-area (cons (make-black-block
                              (make-rib (make-pos 50 50)
                                        (make-pos 450 50))
                              (make-rib (make-pos 50 HEIGHT)
                                        (make-pos 450 HEIGHT))) empty))
(define complex-fb-area1 (cons (make-black-block
                                (make-rib (make-pos 50 0)
                                          (make-pos WIDTH 0))
                                (make-rib (make-pos 50 50)
                                          (make-pos WIDTH 50)))
                               (cons
                                (make-black-block
                                 (make-rib (make-pos 100 50)
                                           (make-pos 150 50))
                                 (make-rib (make-pos 100 100)
                                           (make-pos 150 100)))
                                (cons
                                 (make-black-block
                                  (make-rib (make-pos 100 100)
                                            (make-pos WIDTH 100))
                                  (make-rib (make-pos 100 (- HEIGHT 50))
                                            (make-pos WIDTH (- HEIGHT 50))))
                                 (cons (make-black-block
                                        (make-rib (make-pos 0 225)
                                                  (make-pos 50 225))
                                        (make-rib (make-pos 0 227)
                                                  (make-pos 50 275)))
                                       empty)))))
(define complex-fb-area2 (list B1 B2 B3 B4))
#;
(define (fn-for-fb-area lobb)
  (cond [(empty? lobb) ...]
        [else
         (... (fn-for-black-block (first lobb))
              (fn-for-fb-area (rest lobb)))]))

(define-struct maze (room forbdn-area))
;; Maze is (make-maze Room Forbdn-area)
;; Interp. (make-maze room forbdn-area) is a maze where
;;                  - room is the square that represents the whole square
;;                  - forbdn-area is the Space on the maze where you aren't
;;                    allowed to move within
(define maze1 (make-maze room1 fb-area1))
(define easy-maze1 (make-maze room1 square-fb-area))
(define easy-maze2 (make-maze room1 (cons (make-black-block
                                           (make-rib (make-pos 50 50)
                                                     (make-pos WIDTH 50))
                                           (make-rib (make-pos 50
                                                               (- HEIGHT 50))
                                                     (make-pos WIDTH
                                                               (- HEIGHT 50))))
                                          empty)))
(define hard-maze1 (make-maze room1 complex-fb-area1))
(define hard-maze2 (make-maze room1 complex-fb-area2))
#;
(define (fn-for-maze m)
  (... (fn-for-room (maze-room m))
       (fn-for-fb-area (maze-forbdn-area m))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constants:
(define STEP 20);
;; Functions:

;; Maze -> Boolean
;; Check wheter given maze is solvable
;; You can only move down or right in the maze
(check-expect (solvable? easy-maze1) true)
(check-expect (solvable? hard-maze1) true)
;(define (solvable? m) false)
(define (solvable? m))



;; Posn -> Posn
;; moves one step to the right
;(define (move-right m p) (make-posn 0 0))
(check-expect (move-right (make-posn 0 0))
              (make-posn (+ STEP 0) 0))
(define (move-right p)
  (make-posn (+ (posn-x p) STEP) (posn-y p)))

;; Posn -> Posn
;; moves one step down
;(define (move-down p) (make-posn 0 0))
(check-expect (move-down (make-posn 0 0)) (make-posn 0 (+ STEP 0)))
(define (move-down p)
  (make-posn (posn-x p) (+ STEP (posn-y p))))

;; Forbd-area Posn -> Boolean
;; checks whether the given position is in the Forbdn-area of the maze
;(define (hit? lobb p) false)
(check-expect (hit? square-fb-area (make-posn 20 15)) false)
(check-expect (hit? square-fb-area (make-posn 55 15)) false)
(check-expect (hit? square-fb-area (make-posn 55 60)) true)

(define (hit? lobb p)
  (cond [(empty? lobb) false]
        [else
         (if (in-black-block? (first lobb) p)
             true
             (hit? (rest lobb) p))]))

;; Black-Block Posn -> Boolean
;; Checks if the given posn is in the Black-Block
;(define (in-black-block? b p) false)
(check-expect (in-black-block? B1 (make-posn 90 90)) true)
(check-expect (in-black-block? B1 (make-posn 10 10)) false)
(define (in-black-block? b p)
  (and (and (> (posn-x p) (pos-x (rib-start (black-block-x-rib b))))
            (< (posn-x p) (pos-x (rib-end (black-block-x-rib b)))))
       (and (> (posn-y p) (pos-y (rib-start (black-block-x-rib b))))
            (< (posn-y p) (pos-y (rib-end (black-block-y-rib b)))))))


;; Forbdn-area posn -> posn
;; Tries to move to the right if it hit tries to move down
;(define (try-right lobb p) (make-posn 0 0))
(define (try-right lobb p)
  (local [(define (step-right (move-right p)))]
    (cond [(hit? lobb step-right) (try-down p)]
          [else
           (try-down step-right)])))  ; i need to fix the case where we
                                      ; are in a corner


;; Forbdn-area posn -> posn
;; Tries to move to the down if it hit tries to move right
;(define (try-down lobb p) (make-posn 0 0))
(define (try-down lobb p)
  (local [(define (step-down (move-down p)))
    (cond [(hit? lobb step-down) (try-right p)]
          [else
           (try-down step-right)])))  ; i need to fix the case where we
                                      ; are in a corner

(define (move lobb p)
  (local [(define step-right (move-right p))
          (define step-down  (move-down p))
          (define right-hit (hit? lobb step-right))
          (define down-hit  (hit? lobb step-down))]
  (cond [(and (> (posn-x p) WIDTH)
              (> (posn-y p) HEIGHT)) true]
        [(and right-hit down-hit)
         (step-back p)]
        [right-hit (move lobb step-down)]
        [donw-hit  (move lobb step-right)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Maze -> Image
;; renders the given maze into a visual image
(check-expect (render-maze easy-maze1)
              (place-images (make-images (maze-forbdn-area easy-maze1))
                            (make-posns  (maze-forbdn-area easy-maze1))
                            MTS))
(check-expect (render-maze easy-maze2)
              (place-images (make-images (maze-forbdn-area easy-maze2))
                            (make-posns  (maze-forbdn-area easy-maze2))
                            MTS))
(check-expect (render-maze hard-maze2)
              (place-images (make-images (maze-forbdn-area hard-maze2))
                            (make-posns  (maze-forbdn-area hard-maze2))
                            MTS))


;(define (render-maze m) MTS)
(define (render-maze m)
  (place-images
   (make-images (maze-forbdn-area m))
   (make-posns  (maze-forbdn-area m))
   MTS))


;; Forbdn-area -> listOf(Image)
;; Note: Forbdn-area is a list of Black-blocks
;; Produces a list of all the black-blocks as black rectangles
(check-expect (make-images square-fb-area)
              (list (rectangle 400 450 "solid" "black")))
;(define (make-images lobb) empty)

(define (make-images lobb)
  (cond [(empty? lobb) empty]
        [else
         (cons (make-image (first lobb))
               (make-images (rest lobb)))]))

;; Black-Block -> Image
;; Produce a rectangle that visually represents the Black-Block
;(define (make-image b) MTS)
(check-expect (make-image B1) (rectangle 400 50 "solid" "black"))
(define (make-image b)
  (rectangle (find-width  (black-block-x-rib b))
             (find-height b)
             "solid" "black"))

;; Rib -> Natural
;; Produce the distance form one position to another
;; produce the given number as the width of rectangle
(check-expect (find-width (make-rib (make-pos 50 50)
                                    (make-pos 450 50))) 400)
;(define (find-width x-rib) 0)
(define (find-width r)
  (- WIDTH (+ (pos-x (rib-start r))
              (- WIDTH (pos-x (rib-end r))))))

;; Rib -> Natural
;; Produce the distance between the y axis of the given rib
;; This distance is considered the height of a rectangle
;(define (find-height y-rib) 0)
(check-expect (find-height B1) 50)
(define (find-height r)
  (- HEIGHT (+ (pos-y (rib-start (black-block-x-rib r)))
              (- HEIGHT (pos-y (rib-end (black-block-y-rib r)))))))



;; Forbdn-area -> listOf(pos)
;; Note: Forbdn-area is a list of Black-Block
;; Produce a list of Posns where each pos is the center of one black-block
;; Those centers are for the place-images function
(check-expect (make-posns square-fb-area) (list (make-posn 250 275)))
;(define (make-posns lobb) empty)
(define (make-posns lobb)
  (cond [(empty? lobb) empty]
        [else
         (cons (center (first lobb))
               (make-posns (rest lobb)))]))

;; Black-Block -> pos
;; Produce the center of a given black block in relation with the whole maze
(check-expect (center (make-black-block
                              (make-rib (make-pos 50 50)
                                        (make-pos 450 50))
                              (make-rib (make-pos 50 HEIGHT)
                                        (make-pos 450 HEIGHT))))
              (make-posn 250 275))
;(define (center b) 0)
(define (center b)
  (make-posn (find-x (black-block-x-rib b))
            (find-y b)))

;; Rib -> Natural
;; finds the center of a given rib
(check-expect (find-x (make-rib (make-pos 50 50) (make-pos 450 50))) 250)
;(define (find-x r) 0)
(define (find-x r)
  (+   (pos-x (rib-start r))
       (/ (find-width r) 2)))

;; Black-Block
;; Produce the y center of given Black-bloc
(check-expect (find-y (make-black-block
                              (make-rib (make-pos 50 50)
                                        (make-pos 450 50))
                              (make-rib (make-pos 50 HEIGHT)
                                        (make-pos 450 HEIGHT)))) 275)
;(define (find-y b) 0)
(define (find-y b)
  (+   (pos-y (rib-start (black-block-x-rib b)))
       (/ (find-height b) 2)))