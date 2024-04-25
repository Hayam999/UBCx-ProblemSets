;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname recursin) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rebellion)
;;; Natural -> Natural
;;; calculates the factorial of a given number
;(check-expect (fact 1) 1)
;(check-expect (fact 2) 2)
;(check-expect (fact 3) 6)
;(check-expect (fact 4) 24)
;;(define (fact n) 0)
;(define (fact n)
;  (if (= n 1) 1
;      (* (fact (- n 1)) n)))
;
;
;
;;; String -> String
;;; reverse a given string
;(check-expect (rev "Hayam") "mayaH")
;(check-expect (rev "racecar") "racecar")
;(check-expect (rev "Happy Birthday") "yadhtriB yppaH")
;;(define (rev s) "")
;(define (rev s)
;  (if (string=? s "") ""
;      (string-append (rev (substring s 1)) (substring s 0 1))))



;; Natural String -> String
;; find the binary representation of a given decimal
(check-expect (binary 2 "") "01")
;(check-expect (binary 3 "") "11")
;(check-expect (binary 10 "") "0101")
;(define (binary num s) 0)

(define (binary num s)
  (if (= num 0) s
  (local [(define val (convert-forward number<->string (modulo num 2)))
          ]
    (binary (floor (/ num 2)) (string-append val s)))))