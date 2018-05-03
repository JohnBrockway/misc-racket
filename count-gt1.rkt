;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname count-gt1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; November 11th, 2014
;; Assignment 8
;; Problem 3: Greater Than One

;; (count>1 list) produces the amount of elements in list 
;;   that are greater than 1
;; count>1: (listof Num) -> Num
;; Examples:
(check-expect (count>1 (list -6 0 0.2 3 1001 -1001))
              2)
(check-expect (count>1 (list 1))
              0)

(define (count>1 list)
  (length (filter positive? (map sub1 list))))

;; Tests:
(check-expect (count>1 empty)
              0)
(check-expect (count>1 (list 1 1 1 1))
              0)
(check-expect (count>1 (list 1 2 1 2))
              2)