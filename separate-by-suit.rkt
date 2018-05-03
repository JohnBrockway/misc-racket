#lang racket

;; John Brockway
;; 20576999
;; CS136 Winter 2015
;; Assignment 1, Problem 1

(require "numbers1.rkt" "card-deck.rkt")

(provide separate-by-suit)

;; (separate-by-suit) converts a list of Cards into a list of 4
;; lists, with every sublist having the same suit value
;; separate-by-suit: (listOf Card) -> (listOf (listOf Card))
(define (separate-by-suit)
  (define n (get-number))
  (define deck (shuffle-deck (build-deck) n))
  (define (sorting d piles)
    (cond [(empty? d) piles]
          [(empty? (first piles)) 
           (sorting (rest d) (cons (list (first d)) (rest piles)))]
          [(symbol=? (get-suit (first d)) (get-suit (first (first piles))))
           (sorting (rest d) (cons (append (first piles) (list (first d))) 
                                   (rest piles)))]
          [(empty? (second piles)) 
           (sorting (rest d) (cons (first piles)
                                   (cons (list (first d)) (rest (rest piles)))))]
          [(symbol=? (get-suit (first d)) (get-suit (first (second piles))))
           (sorting (rest d) (cons (first piles) 
                                   (cons (append (second piles) 
                                                 (list (first d))) 
                                         (rest (rest piles)))))]
          [(empty? (third piles)) 
           (sorting (rest d) (cons (first piles) 
                                   (cons (second piles)
                                         (cons (list (first d)) (rest piles)))))]
          [(symbol=? (get-suit (first d)) (get-suit (first (third piles))))
           (sorting (rest d) (cons (first piles) 
                                   (cons (second piles)
                                         (cons (append (third piles) 
                                                       (list (first d))) 
                                               (rest (rest (rest piles)))))))]
          [(empty? (fourth piles)) 
           (sorting (rest d) (cons (first piles) 
                                   (cons (second piles)
                                         (cons (third piles)
                                               (cons (list (first d)) empty)))))]
          [else (sorting (rest d)
                         (cons (first piles)
                               (cons (second piles)
                                     (cons (third piles)
                                           (list (append (fourth piles)
                                                         (list (first d))))))))]))
  (sorting deck (list empty empty empty empty)))

(separate-by-suit)