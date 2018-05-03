;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mergesort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; December 2nd, 2014
;; Assignment 10
;; Problem 1: Mergesort

;; (mergesort lst comp) sorts list according to comp using a 
;;    divide-and-conquer approach
;; mergesort: (listof X) (X X -> Bool) -> (listof X)
;; Examples:
(check-expect (mergesort '(1 4 2 5 3 6) <)
              '(1 2 3 4 5 6))
(check-expect (mergesort (list "Assignment" "Ten" "Mergesort" "Woohoo")
                         string>?)
              (list "Woohoo" "Ten" "Mergesort" "Assignment"))

(define (mergesort lst comp)
  (cond [(empty? lst) empty]
        [else
         (local [;; (split l one two length/2) produces a list of two lists by 
                 ;;   splitting l into lists of length length/2 or length/2 - 1
                 ;; split: (listof X) (listof X) Nat -> 
                 ;;        (list (listof X) (listof X))
                 (define (split l r length/2)
                   (cond [(= (length r) length/2) (cons r (list l))]
                         [else (split (rest l) (cons (first l) r) length/2)]))
                 (define halves
                   (local [(define length/2 (/ (length lst) 2))]
                     (cond [(integer? length/2) (split lst empty length/2)]
                           [else (split lst empty (- length/2 0.5))])))
                 (define left (first halves))
                 (define right (second halves))]
           (merge left right comp))]))

;; (merge one two comp) recursively puts one and two together
;; merge: (listof X) (listof X) (X X -> Bool) -> (listof X)
;; Examples:
(check-expect (merge '(1 3 4 5 6) '(2 8) <)
              '(1 2 3 4 5 6 8))
(check-expect (merge '(1 4 8) '(3) >)
              '(8 4 3 1))

(define (merge one two comp)
  (cond [(and (empty? (rest one)) (empty? (rest two)))
         (cond [(comp (first one) (first two)) 
                (cons (first two) (list (first one)))]
               [else)]
        [(empty? (rest one)) (merge one (mergesort two comp) comp)]
        [(empty? (rest two)) (merge two (mergesort one comp) comp)]
        [else (cond [(comp (first one) (first two))
                     (cons (first one) (merge (rest one) two comp))]
                    [else (cons (first two) (merge (rest two) one comp))])]))