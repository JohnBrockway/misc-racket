;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; September 23rd, 2014
;; Assignment 2
;; Problem 2: Grades


(define (cs135-exam-grade midterm1 midterm2 final)
  (/ (+ (* midterm1 10) (* midterm2 20) (* final 45)) 75))

(define (final-cs135-exam-grade-needed midterm1 midterm2)
  (/ (- 3750 (* midterm1 10) (* midterm2 20)) 45))

;; (cs135-final-grade participation assignment exam) produces the
;;   weighted average of the grades participation, assignment,
;;   and exam. If either assignment or exam is below 50, the function 
;;   produces the value 46 or the weighted average, whichever is 
;;   lower.
;; cs135-final-grade: Nat Nat Nat -> Num
;;   requires: participation <= 100
;;             assignment <= 100
;;             exam <= 100
;; Examples:
(check-expect (cs135-final-grade 70 80 60) 64.5) 
(check-expect (cs135-final-grade 80 40 100) 46)

(define (cs135-final-grade participation assignment exam)
  (cond
    [(and (>= assignment 50) (>= exam 50))
     (/ (+ (* participation 5) 
           (* assignment 20) 
           (* exam 75)) 
        100)]
    [(<= (/ (+ (* participation 5) 
               (* assignment 20) 
               (* exam 75)) 
            100)
         46)
     (/ (+ (* participation 5) 
               (* assignment 20) 
               (* exam 75)) 
            100)]
    [else 46]))

;; Tests:
(check-expect (cs135-final-grade 20 10 20) 18)
(check-expect (cs135-final-grade 0 0 0) 0)
(check-expect (cs135-final-grade 75 80 100) 94.75)
(check-expect (cs135-final-grade 100 100 40) 46)