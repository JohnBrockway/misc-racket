;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname int-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; October 7th, 2014
;; Assignment 4
;; Problem 1: Lists of Integers


;; (count-squares list-of-integers) produces the amount of perfect
;;   squares in a list of integers
;; count-squares: (listof Int) -> Nat
;; Examples:
(check-expect (count-squares (cons 1 (cons 5 (cons 4 (cons 9 empty)))))
              3)
(check-expect (count-squares (cons 1 empty))
              1)

(define (count-squares list-of-integers)
  (cond
    [(empty? list-of-integers) 0]
    [else (cond
            [(integer? (sqrt (first list-of-integers)))
             (add1 (count-squares (rest list-of-integers)))]
            [else (count-squares (rest list-of-integers))])]))

;; Tests
(check-expect (count-squares empty)
              0)
(check-expect (count-squares (cons -1 (cons 16 (cons -9 empty))))
              1)
(check-expect (count-squares (cons 1 (cons 0 empty)))
              2)

;; (square-ints list-of-integers) takes every element from a list of
;;   integers and produces a new list consisting of the squares of those
;;   elements
;; square-ints: (listof Int) -> (listof Int)
;; Examples:
(check-expect (square-ints (cons 1 (cons 4 (cons -9 empty))))
              (cons 1 (cons 16 (cons 81 empty))))
(check-expect (square-ints (cons 5 (cons 4 (cons 7 (cons 0 empty)))))
              (cons 25 (cons 16 (cons 49 (cons 0 empty)))))

(define (square-ints list-of-integers)
  (cond
    [(empty? list-of-integers) empty]
    [else (cons (sqr (first list-of-integers)) (square-ints (rest list-of-integers)))]))

;; Tests:
(check-expect (square-ints empty)
              empty)
(check-expect (square-ints (cons -5 (cons 2 empty)))
              (cons 25 (cons 4 empty)))
(check-expect (square-ints (cons 0 empty))
              (cons 0 empty))

;; (ascending? list) consumes a list of integers and produces true if the list is
;;   ascending, and false otherwise
;; ascending?: (listof Int) -> Bool
;; Examples:
(check-expect (ascending? (cons 1 (cons 4 (cons 6 empty))))
              true)
(check-expect (ascending? (cons 5 (cons 6 (cons 1 empty))))
              false)

(define (ascending? list-of-integers)
  (cond
    [(empty? list-of-integers) true]
    [(empty? (rest list-of-integers)) true]
    [(>= (first list-of-integers) (first (rest list-of-integers))) false]
    [else (ascending? (rest list-of-integers))]))

;; Tests:
(check-expect (ascending? (cons 1 (cons 5 (cons 5 (cons 6 empty)))))
              false)
(check-expect (ascending? (cons 1 (cons 5 (cons 6 (cons 8 empty)))))
              true)
(check-expect (ascending? empty)
              true)
(check-expect (ascending? (cons 1 empty))
              true)

;; (cumulative-gain list-of-integers) produces the total cumulative gain
;;   throughout a list of integers
;; cumulative-gain: (listof Int) -> Int
;; requires: list-of-integers be non-empty
;; Examples:
(check-expect (cumulative-gain (cons 1 (cons 5 (cons 5 (cons 6 empty)))))
              5)
(check-expect (cumulative-gain (cons 1 (cons 100 empty)))
              99)

(define (cumulative-gain list-of-integers)
  (cond
    [(empty? (rest list-of-integers)) 0]
    [else (+ (- (first (rest list-of-integers)) (first list-of-integers))
          (cumulative-gain (rest list-of-integers)))]))

;; Tests
(check-expect (cumulative-gain (cons 3 empty))
              0)
(check-expect (cumulative-gain (cons 10 (cons 0 empty)))
              -10)
(check-expect (cumulative-gain (cons -7 (cons -9 (cons 10 empty))))
              17)