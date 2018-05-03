;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname notes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; October 14th, 2014
;; Assignment 5
;; Problem 1: Musical Notes

(define-struct note (letter modifier))
;; A Note is a (make-note Sym Sym)
;; requires: letter be one of 'A through 'G inclusive
;;           modifier be one of 'sharp, 'flat, 'natural

(define (my-note-fn note)
  (... (note-letter note) ...
  ... (note-modifier note) ...))

;; (normalize-note note) consumes a note and produces its
;;   position on the chromatic scale
;; normalize-note: Note -> Nat
;; Examples:
(check-expect (normalize-note (make-note 'C 'sharp))
              2)
(check-expect (normalize-note (make-note 'E 'natural))
              5)

(define (normalize-note note)
  (cond
    [(symbol=? (note-modifier note) 'flat)
     (cond
       [(symbol=? (note-letter note) 'A)
        9]
       [(symbol=? (note-letter note) 'B)
        11]
       [(symbol=? (note-letter note) 'C)
        12]
       [(symbol=? (note-letter note) 'D)
        2]
       [(symbol=? (note-letter note) 'E)
        4]
       [(symbol=? (note-letter note) 'F)
        5]
       [(symbol=? (note-letter note) 'G)
        7])]
    [(symbol=? (note-modifier note) 'natural)
     (cond
       [(symbol=? (note-letter note) 'A)
        10]
       [(symbol=? (note-letter note) 'B)
        12]
       [(symbol=? (note-letter note) 'C)
        1]
       [(symbol=? (note-letter note) 'D)
        3]
       [(symbol=? (note-letter note) 'E)
        5]
       [(symbol=? (note-letter note) 'F)
        6]
       [(symbol=? (note-letter note) 'G)
        8])]
    [(symbol=? (note-modifier note) 'sharp)
     (cond
       [(symbol=? (note-letter note) 'A)
        11]
       [(symbol=? (note-letter note) 'B)
        1]
       [(symbol=? (note-letter note) 'C)
        2]
       [(symbol=? (note-letter note) 'D)
        4]
       [(symbol=? (note-letter note) 'E)
        6]
       [(symbol=? (note-letter note) 'F)
        7]
       [(symbol=? (note-letter note) 'G)
        9])]))

;; Tests:
(check-expect (normalize-note (make-note 'C 'natural))
              1)
(check-expect (normalize-note (make-note 'B 'sharp))
              1)
(check-expect (normalize-note (make-note 'C 'sharp))
              2)
(check-expect (normalize-note (make-note 'D 'flat))
              2)
(check-expect (normalize-note (make-note 'D 'natural))
              3)
(check-expect (normalize-note (make-note 'D 'sharp))
              4)
(check-expect (normalize-note (make-note 'E 'flat))
              4)
(check-expect (normalize-note (make-note 'E 'natural))
              5)
(check-expect (normalize-note (make-note 'F 'flat))
              5)
(check-expect (normalize-note (make-note 'F 'natural))
              6)
(check-expect (normalize-note (make-note 'E 'sharp))
              6)
(check-expect (normalize-note (make-note 'F 'sharp))
              7)
(check-expect (normalize-note (make-note 'G 'flat))
              7)
(check-expect (normalize-note (make-note 'G 'natural))
              8)
(check-expect (normalize-note (make-note 'G 'sharp))
              9)
(check-expect (normalize-note (make-note 'A 'flat))
              9)
(check-expect (normalize-note (make-note 'A 'natural))
              10)
(check-expect (normalize-note (make-note 'A 'sharp))
              11)
(check-expect (normalize-note (make-note 'B 'flat))
              11)
(check-expect (normalize-note (make-note 'B 'natural))
              12)
(check-expect (normalize-note (make-note 'C 'flat))
              12)

;; (normalize-note-list list-of-notes) consumes a list of notes and produces
;;   a corresponding list of positions on the chromatic scale
;; normalize-note-list: (listof Note) -> (listof Nat)
;; Examples:
(check-expect (normalize-note-list (list (make-note 'B 'flat)
                                         (make-note 'C 'natural)
                                         (make-note 'E 'sharp)))
              (list 11 1 6))
(check-expect (normalize-note-list (list (make-note 'C 'flat)))
              (list 12))

(define (normalize-note-list list-of-notes)
  (cond
    [(empty? list-of-notes) empty]
    [else (cons (normalize-note (first list-of-notes))
                (normalize-note-list (rest list-of-notes)))]))

;; Tests:
(check-expect (normalize-note-list empty)
              empty)
(check-expect (normalize-note-list (list (make-note 'F 'flat)
                                         (make-note 'B 'natural)))
              (list 5 12))

;; (interval note1 note2) produces the increasing distance between two Notes on the
;;   chromatic scale
;; interval: Note Note -> Nat
;; Examples:
(check-expect (interval (make-note 'F 'flat)
                        (make-note 'B 'natural))
              7)
(check-expect (interval (make-note 'B 'natural)
                        (make-note 'F 'flat))
              5)

(define (interval note1 note2)
  (cond
    [(<= (normalize-note note1) (normalize-note note2)) 
     (- (normalize-note note2) (normalize-note note1))]
    [else (- 12 (- (normalize-note note1) (normalize-note note2)))]))

;; Tests:
(check-expect (interval (make-note 'C 'natural)
                        (make-note 'C 'natural))
              0)
(check-expect (interval (make-note 'E 'flat)
                        (make-note 'D 'sharp))
              0)
(check-expect (interval (make-note 'C 'natural)
                        (make-note 'C 'flat))
              11)

;; (note-list-to-interval-list list-of-notes) consumes a list of Notes and 
;;   produces a list containing the intervals between each adjacent element
;;   in the list on the chromatic scale, followed by the interval between
;;   the first and last
;; note-list-to-interval-list: (listof Note) -> (listof Nat)
;; Examples:
(check-expect (note-list-to-interval-list (list (make-note 'E 'flat)
                                                (make-note 'D 'sharp)
                                                (make-note 'C 'natural)))
              (list 0 9 3))
(check-expect (note-list-to-interval-list (list (make-note 'E 'flat)
                                                (make-note 'F 'sharp)
                                                (make-note 'E 'flat)))
              (list 3 9 0))

(define (note-list-to-interval-list list-of-notes)
  (cond
    [(empty? list-of-notes) empty]
    [else (first-in-list-interval-fn list-of-notes
                                     (first list-of-notes))]))

;; Tests:
(check-expect (note-list-to-interval-list empty)
              empty)
(check-expect (note-list-to-interval-list (list (make-note 'D 'sharp)
                                                (make-note 'E 'flat)
                                                (make-note 'E 'natural)
                                                (make-note 'F 'flat)))
              (list 0 1 0 11))

;; (first-in-list-interval-fn list first) is a helper funtion for 
;;   note-list-to-interval-list that carries through the first element 
;;   in the list so that it can be used for the final element in the new list
;; first-in-list-interval-fn: (listof Note) Note -> (listof Nat)
;; Example (for more, see examples for (note-list-to-interval-list):
(check-expect (first-in-list-interval-fn (list (make-note 'G 'natural)
                                               (make-note 'C 'sharp)
                                               (make-note 'A 'sharp))
                                         (make-note 'G 'natural))
              (list 6 9 9))

(define (first-in-list-interval-fn list frst)
  (cond
    [(empty? (rest list)) (cons (interval (first list)
                                          frst) empty)]
    [else (cons (interval (first list)
                          (second list))
                (first-in-list-interval-fn (rest list) frst))]))

;; Tests:
(check-expect (first-in-list-interval-fn (list (make-note 'D 'sharp)
                                               (make-note 'E 'flat)
                                               (make-note 'E 'natural)
                                               (make-note 'F 'flat))
                                         (make-note 'D 'sharp))
              (list 0 1 0 11))
(check-expect (first-in-list-interval-fn (list (make-note 'B 'natural))
                                         (make-note 'B 'natural))
              (list 0))
(check-expect (first-in-list-interval-fn (list (make-note 'D 'sharp)
                                               (make-note 'E 'flat))
                                         (make-note 'D 'sharp))
              (list 0 0))