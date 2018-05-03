;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; September 16th, 2014
;; Assignment 1
;; Problem 1: Functions

(define g 9.8)


(define (cylinder-volume r h) 
  (* pi (sqr r) h))

(define (future-value p r t) 
  (* p (expt (+ 1 r) t)))

(define (height v t)
  (-(* v t) 
    (/(* g (* t t)) 2)))
