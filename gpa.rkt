;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname gpa) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; November 11th, 2014
;; Assignment 8
;; Problem 1: GPA

(define-struct student-record (name id grades))
;; A Student-Record is a (make-student-record Str Nat (listof Num))


;; (gpa sr) produces the average of the grades of sr, or 'nogrades if
;;   grades is empty
;; gpa: Student-Record -> (anyof Num 'nogrades)
;; Examples:
(check-expect (gpa (make-student-record "bob" 123 (list 100 100 100 75)))
              93.75)
(check-expect (gpa (make-student-record "jane" 234 empty))
              'nogrades)

(define (gpa sr)
  (local [;; (avg list) produces the average of all elements of list
          ;; avg: (listof Num) -> Num
          (define (avg list amount total)
            (cond
              [(empty? list) (/ amount total)]
              [else (avg (rest list) (+ (first list) amount) (add1 total))]))]
    (cond
      [(empty? (student-record-grades sr)) 'nogrades]
      [else (avg (student-record-grades sr) 0 0)])))

;; Tests:
(check-expect (gpa (make-student-record "John" 123 (list 53 70 60 80 85)))
              69.6)
(check-expect (gpa (make-student-record "jane" 234 (list 40)))
              40)

  