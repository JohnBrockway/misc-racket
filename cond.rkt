;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; September 23rd, 2014
;; Assignment 2
;; Problem 1: Equivalent Expressions

(define (q1a x)
  (cond
    [(and (p1? x) (p2? x)) 'red]
    [(p1? x) 'blue]
    [(p2? x) 'green]
    [else 'yellow]))

(define (q1b x)
  (cond
    [(p1? x) 'blue]
    [(p2? x) 'red]
    [else 'blue]))

(define (q1c x)
  (cond
    [(and (p1? x) (p2? x) (p3? x)) 'red]
    [(and (p1? x) (p2? x) (not p3? x)) 'blue]
    [else 'green]))
    