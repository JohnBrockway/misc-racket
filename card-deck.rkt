#lang racket

(provide build-deck shuffle-deck move-to-front remove-from-list 
         get-suit get-rank)         

(random-seed 1234)

(struct card (suit rank) #:transparent)

(define num-suits 4)
(define num-ranks 13)
(define num-cards (* num-suits num-ranks))

(define suits '(club diamond heart spade))
(define ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))

;; (get-suit c) produces the suit of c
;; get-suit: Card -> Sym
(define (get-suit c)
  (card-suit c))

;; (get-rank c) produces the rank of c
;; get-rank: Card -> (anyOf Int Sym)
(define (get-rank c)
  (card-rank c))

;; (build-deck) creates a list of unique cards
;; build-deck: -> (listOf Card)
(define (build-deck)
  (flatten (build-list num-suits 
               (lambda (x) (build-list num-ranks 
                                       (lambda (y) 
                                         (card (list-ref suits x) (list-ref ranks y))))))))

;; (shuffle-deck d n) randomizes the order of n elements of d
;; shuffle-deck: (listOf X) Nat -> (listOf X)
(define (shuffle-deck d n)
  (cond [(<= n 0) d]
        [else (shuffle-deck (move-to-front d (random num-cards)) (sub1 n))]))

;; (move-to-front d n) makes the nth element of d the first element, 
;; removing it from its original position
;; move-to-front: (listOf X) Nat -> (listOf X)
(define (move-to-front d n)
  (cons (list-ref d n) (remove-from-list d n)))

;; (remove-from-list d n) removes the nth element from d
;; remove-from-list: (listOf X) Nat -> (listOf X)
;; requires: d be non-empty
(define (remove-from-list d n)
  (cond [(= n 0) (rest d)]
        [else (cons (first d) (remove-from-list (rest d) (sub1 n)))]))