;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname student) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; September 30th, 2014
;; Assignment 3
;; Problem 1: Student Structure


(define classes-per-year 4)

(define-struct student (id faculty-name cav year failed))
;; A Student is a (make-student Nat Sym Num Nat Nat)


;; (join-faculty id faculty-name) consumes a 4 digit ID number and a 
;;   symbol representing faculty and creates a new student with
;;   that information. All other fields are set to zero.
;; join-faculty: Nat Sym -> Student
;; Example:
(check-expect (join-faculty 1234 'Mathematics) 
              (make-student 1234 'Mathematics 0 0 0))

(define (join-faculty id faculty-name)
  (make-student id faculty-name 0 0 0))

;; Tests:
(check-expect (join-faculty 2468 'Arts) 
              (make-student 2468 'Arts 0 0 0))

;; (status student) consumes a Student and produces a symbol
;;   representing the academic standing of the given Student
;; status: Student -> Sym
;; Examples:
(check-expect (status (make-student 1123 'Arts 75 2 1))
              'Good)
(check-expect (status (make-student 5813 'Arts 30 2 8))
              'Failing)
(check-expect (status (make-student 2134 'Mathematics 95 2 5))
              'Removed)

(define (status student)
  (cond [(= (student-year student) 0)
         'Undefined]
        [(and (symbol=? (student-faculty-name student) 'Mathematics)
              (> (student-failed student) 4))
         'Removed]
        [(< (student-cav student) 50)
         'Failing]
        [(< (student-cav student) 60)
         'Probation]
        [(< (student-cav student) 70)
         'Satisfactory]
        [(< (student-cav student) 80)
         'Good]
        [else 'Excellent]))

;; Tests
(check-expect (status (make-student 1123 'Arts 75 0 1))
              'Undefined)
(check-expect (status (make-student 1123 'Mathematics 75 2 1))
              'Good)
(check-expect (status (make-student 1123 'Mathematics 75 2 5))
              'Removed)
(check-expect (status (make-student 1123 'Arts 10 2 1))
              'Failing)
(check-expect (status (make-student 1123 'Arts 50 2 1))
              'Probation)
(check-expect (status (make-student 1123 'Arts 55 2 1))
              'Probation)
(check-expect (status (make-student 1123 'Arts 60 2 1))
              'Satisfactory)
(check-expect (status (make-student 1123 'Arts 65 2 1))
              'Satisfactory)
(check-expect (status (make-student 1123 'Arts 70 2 1))
              'Good)
(check-expect (status (make-student 1123 'Arts 75 2 1))
              'Good)
(check-expect (status (make-student 1123 'Arts 80 2 1))
              'Excellent)
(check-expect (status (make-student 1123 'Arts 90 2 1))
              'Excellent)

;; (next-year stud g1 g2 g3 g4) consumes a student and 4 grades
;;   and adds one to the year of the student, as well as increasing
;;   the value of failed if appropriate and updating the cav
;; next-year: Student Nat Nat Nat Nat -> Student
;; Examples:
(check-expect (next-year (make-student 1111 'AHS 75 3 1)
                         80 100 4 60)
              (make-student 1111 'AHS 71.5 4 2))
(check-expect (next-year (make-student 9999 'Mathematics 50 1 0)
                         40 20 25 100)
              (make-student 9999 'Mathematics 48.125 2 3))

(define (next-year stud g1 g2 g3 g4)
  (make-student (student-id stud)
                (student-faculty-name stud)
                (/ (+ (* (student-cav stud) 
                         (* (student-year stud) classes-per-year)) 
                      g1 g2 g3 g4) 
                   (+ (* (student-year stud) classes-per-year) 
                      classes-per-year))
                (+ 1 (student-year stud))
                (+ (failed-calc g1 g2 g3 g4) (student-failed stud))))

;; Tests:
(check-expect (next-year (make-student 1111 'AHS 0 0 0)
                         80 100 4 60)
              (make-student 1111 'AHS 61 1 1))
(check-expect (next-year (make-student 1111 'Mathematics 80 2 2)
                         10 100 4 50)
              (make-student 1111 'Mathematics 67 3 4))

;; (failed-calc g1 g2 g3 g4) consumes 4 grades and returns the number of them
;;   that are below 50
;; failed-calc: Nat Nat Nat Nat -> Nat
;; Examples:
(check-expect (failed-calc 40 20 100 75) 2)
(check-expect (failed-calc 10 50 76 83) 1)

(define (failed-calc g1 g2 g3 g4)
  (cond
    [(< g1 50)
     (cond
       [(< g2 50)
        (cond
          [(< g3 50)
           (cond
             [(< g4 50) 4]
             [else 3])]
          [(< g4 50) 3]
          [else 2])]
       [(< g3 50)
        (cond
          [(< g4 50) 3]
          [else 2])]
       [(< g4 50) 2]
       [else 1])]
    [(< g2 50)
     (cond
       [(< g3 50)
        (cond
          [(< g4 50) 3]
          [else 2])]
       [(< g4 50) 2]
       [else 1])]
    [(< g3 50) 
     (cond
       [(< g4 50) 2]
       [else 1])]
    [(< g4 50) 1]
    [else 0]))

;; Tests:
(check-expect (failed-calc 50 50 50 50) 0)
(check-expect (failed-calc 75 75 75 75) 0)
(check-expect (failed-calc 75 75 75 40) 1)
(check-expect (failed-calc 75 75 40 75) 1)
(check-expect (failed-calc 75 75 40 40) 2)
(check-expect (failed-calc 75 40 75 75) 1)
(check-expect (failed-calc 75 40 75 40) 2)
(check-expect (failed-calc 75 40 40 75) 2)
(check-expect (failed-calc 75 40 40 40) 3)
(check-expect (failed-calc 40 75 75 75) 1)
(check-expect (failed-calc 40 75 75 40) 2)
(check-expect (failed-calc 40 75 40 75) 2)
(check-expect (failed-calc 40 75 40 40) 3)
(check-expect (failed-calc 40 40 75 75) 2)
(check-expect (failed-calc 40 40 75 40) 3)
(check-expect (failed-calc 40 40 40 75) 3)
(check-expect (failed-calc 40 40 40 40) 4)