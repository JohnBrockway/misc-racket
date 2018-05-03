;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname solitaire) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "a10lib.rkt")

;; John Brockway
;; 20576999
;; December 2nd, 2014
;; Assignment 10
;; Problem 2: Peg Solitaire

;; A Dimension is an Int
;; requires: 1 <= Dimension <= 9

;; A Peg [position] is an Int
;; requires: 11 <= Peg <= 99
;;           neither digit can be zero or greater than the
;;             Dimension (for the corresponding board)

;; A Board is a (list Dimension (listof Peg))
;; The list contains INVALID Peg positions

;; A State is a (listof Peg)
;; requires: list is non-empty
;;           each Peg is VALID for the corresponding board

;; A Solution is one of:
;; * 'any
;; * Peg


(define no-solution-text (list (list "No Solution Found")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the sample board from the assignment

(define sample (list 4 (list 41 42 43 44)))
#|
....
....
....
    
|#

(define sample/init (list 22 23))
#|
....
.OO.
....
    
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the traditional cross pattern with default init state cross/init
;; with some additional (easier) init states you can use

(define cross (list 7 (list 11 12 16 17 21 22 26 27 61 62 66 67 71 72 76 77)))
#|
  ...  
  ...  
.......
.......
.......
  ...  
  ...  
|#

(define cross/init (list 13 14 15 23 24 25 31 32 33 34 35 36 37 41 42 43
                         45 46 47 51 52 53 54 55 56 57 63 64 65 73 74 75))
#|
  OOO  
  OOO  
OOOOOOO
OOO.OOO
OOOOOOO
  OOO  
  OOO  
|#

(define cross/submarine (list 34 42 43 44 45 46))
#|
  ...  
  ...  
...O...
.OOOOO.
.......
  ...  
  ...  
|#

(define cross/greek (list 24 34 42 43 44 45 46 54 64))
#|
  ...  
  .O.  
...O...
.OOOOO.
...O...
  .O.  
  ...  
|#

(define cross/small-diamond (list 24 33 34 35 42 43 45 46 53 54 55 64))
#|
  ...  
  .O.  
..OOO..
.OO.OO.
..OOO..
  .O.  
  ...  
|#

(define cross/big-diamond (list 14 23 24 25 32 33 34 35 36 41 42 43
                                45 46 47 52 53 54 55 56 63 64 65 74))
#|
  .O.  
  OOO  
.OOOOO.
OOO.OOO
.OOOOO.
  OOO  
  .O.  
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is a provided function: no additional documentation required
;; Uncomment the following line after your neighbours is complete and tested
; (define (make-neighbours board) (lambda (state) (neighbours board state)))

;; try this when you are done: (but leave it commented out when you submit)
; (show (result->text cross (solitaire cross cross/init 'any)))


;; (build-board dim) produces a list of dim lists where each sublist has dim
;;   elements, the last digit of each element incrementing from 1 to dim
;; build-board: Nat -> (list (listof Peg))
;; Examples:
(check-expect (build-board 2)
              '((11 12)
                (21 22)))
(check-expect (build-board 5)
              '((11 12 13 14 15)
                (21 22 23 24 25)
                (31 32 33 34 35)
                (41 42 43 44 45)
                (51 52 53 54 55)))

(define (build-board dim)
  (rest (build-list (add1 dim) (lambda (vfirst)
                                 (build-list dim
                                             (lambda (hfirst)
                                               (+ 1 hfirst (* vfirst 10))))))))

;; Code modified from CS 135 Module 10 example
;; Tests:
(check-expect (build-board 0) empty)
(check-expect (build-board 1) (list (list 11)))

;; (state->los board state) produces a list of strings representing
;;   board, considering state
;; state->los: Board State -> (listof Str)
;; Examples:
(check-expect (state->los (list 3 (list 11 13 31 33)) empty)
              '(" . "
                "..."
                " . "))
(check-expect (state->los (list 5 empty) (list 31 32 33 34 35))
              '("....."
                "....."
                "OOOOO"
                "....."
                "....."))

(define (state->los board state)
  (map list->string (map (lambda (x) (foldr (lambda (y z) 
                                              (cond [(member? y (second board)) 
                                                     (cons #\space z)]
                                                    [(member? y state)
                                                     (cons #\O z)]
                                                    [else (cons #\. z)]))
                                            empty
                                            x))
                         (build-board (first board)))))

;; Tests:
(check-expect (state->los (list 0 empty) empty)
              empty)
(check-expect (state->los (list 1 empty) (list 11))
              (list "O"))

;; (make-solved? soln) produces a predicate function that determines whether
;;   a state is equivalent to soln
;; make-solved?: Solution -> (State -> Bool)
;; Examples:
(check-expect ((make-solved? 'any) (list 41)) true)
(check-expect ((make-solved? 22) (list 23)) false)

(define (make-solved? soln)
  (lambda (state) (and (empty? (rest state))
                       (or (equal? soln 'any)
                           (= (first state) soln)))))

;; Tests:
(check-expect ((make-solved? 11) (list 55 56)) false)
(check-expect ((make-solved? 11) (list 11)) true)

;; (neighbours board state) produces a list of States that are possible
;;   on board from state
;; neighbours: Board State -> (listof State)
;; Examples:
(check-expect (neighbours (list 3 empty) (list 11 12))
              (list (list 13)))
(check-expect (neighbours (list 4 (list 11 14 41 44)) (list 12 13 22))
              (list (list 14 22) (list 13 32)))

(define (neighbours board state)
  (local [;; (valid? space) determines if space is unoccupied and not invalid
          ;; valid?: Peg -> Bool
          (define (valid? space)
            (and (not (member? space (second board)))
                 (< space (* (add1 (first board) 10))
                 (<= (remainder space 10) (first board)))))
          ;; (jump-valid? peg direction determines if peg can jump
          ;;   in the given direction
          ;; jump-valid?: Peg Int -> Bool
          ;; requires: direction be one of -1, 1, 10, -10
          (define (jump-valid? peg direction)
            (and (valid? (+ peg (* 2 direction)))
                 (member? (+ peg direction) state)))
          ;; (new-neighbour peg direction) produces the neighbour if peg
          ;;   moves in direction
          ;; new-neighbour: Peg Int -> State
          (define (new-neighbour peg direction)
            (cons (+ peg (* 2 direction)) 
                  (remove (+ peg direction) (remove peg state))))
          (define peg-neighbours empty)]
    (cond [(empty? (second board)) empty]
          [(empty? (first (second board))) 
           (neighbours (cons (first board) (rest (second board))) state)]
          [else (cond [(jump-valid? (first (first (second board))) -1)
                       (cons (new-neighbour (first (first (second board))) -1) 
                             peg-neighbours)])
                (cond [(= 1 1) (cons true peg-neighbours)])]))) 
            