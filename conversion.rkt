;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; September 16th, 2014
;; Assignment 1
;; Problem 2: Conversion

(define liters-per-gallon 3.78541)

(define floz-per-gallon 128)

(define km-per-mile 1.60934)


(define (floz->ml floz-volume) 
  (* (/ floz-volume floz-per-gallon) 
     (* liters-per-gallon 1000)))

(define (mpg->lp100km miles-per-gallon) 
  (* (/ 1 miles-per-gallon) 
     (* (/ liters-per-gallon km-per-mile) 100)))

(define (lp100km->mpg lp100km) 
  (* (/ 1 lp100km) 
     (* (/ liters-per-gallon km-per-mile) 100)))