;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname taxicab) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; October 7th, 2014
;; Assignment 4
;; Problem 3: Taxi Problem

;; A (listof Posn) is one of:
;; * empty
;; * (cons Posn (listof Posn))

(define (my-lop-fn list-of-posns)
  (cond
   [(empty? list-of-posns) ...]
   [else (... (posn-x (first list-of-posns)) ...
          ... (posn-y (first list-of-posns)) ...
              (my-lop-fn (rest list-of-posns)) ...)]))

;; (taxicab-distance list-of-posns) produces the cumulative distance
;;   between all elements of a list of coordinates in order if only vertical and 
;;   horizontal travel is accepted
;; taxicab-distance: (listof Posn) -> Num
;; requires: list-of-posns be non-empty
;; Examples:
(check-expect (taxicab-distance (cons (make-posn 0 0)
                                      (cons (make-posn 1 1) empty)))
              2)
(check-expect (taxicab-distance (cons (make-posn 0 1)
                                      (cons (make-posn 1 1) empty)))
              1)
 
(define (taxicab-distance list-of-posns)
  (cond
   [(empty? (rest list-of-posns)) 0]
   [else (+ (- (posn-x (first (rest list-of-posns)))
               (posn-x (first list-of-posns)))
            (- (posn-y (first (rest list-of-posns)))
               (posn-y (first list-of-posns)))
            (taxicab-distance (rest list-of-posns)))]))

;; Tests:
(check-expect (taxicab-distance (cons (make-posn 1 2)
                                      (cons (make-posn 5 3)
                                            (cons (make-posn 1 2) empty))))
              0)
(check-expect (taxicab-distance (cons (make-posn 1 2)
                                      (cons (make-posn 1 2) empty)))
              0)
(check-expect (taxicab-distance (cons (make-posn 1 2)
                                      (cons (make-posn -5 3) empty)))
              -5)