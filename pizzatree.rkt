;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname pizzatree) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; November 4th, 2014
;; Assignment 7
;; Problem 1: Racket Pizza
(require "a7.rkt")

;; An Association List (AL) is one of:
;; * empty
;; * (list (list Str Num) AL)

(define sample-i-tree2
  (make-i-node "italian sausage" 50
               (make-i-node "chicken" 100
                            (make-i-node "bacon" 2000
                                         (make-i-node "artichoke hearts" 100 empty empty)
                                         (make-i-node "broccoli" 50 empty empty))
                            (make-i-node "feta cheese" 0 empty empty))
               (make-i-node "pepperoni" 5000
                            (make-i-node "onions" 2000 
                                         (make-i-node "mushrooms" 1500 empty empty)
                                         empty)
                            (make-i-node "tomatoes" 1
                                         (make-i-node "sundried tomatoes" 100 empty empty)
                                         empty))))

(define sample-i-tree3
  (make-i-node "italian sausage" 50
               (make-i-node "chicken" 100
                            (make-i-node "bacon" 2000
                                         (make-i-node "artichoke hearts" 0 empty empty)
                                         (make-i-node "broccoli" 50 empty empty))
                            (make-i-node "feta cheese" 200 empty empty))
               (make-i-node "pepperoni" 5000
                            (make-i-node "onions" 2000 
                                         (make-i-node "mushrooms" 1500 empty empty)
                                         empty)
                            (make-i-node "tomatoes" 1
                                         (make-i-node "sundried tomatoes" 100 empty empty)
                                         empty))))

(define sample-i-tree4
  (make-i-node "italian sausage" 50
               (make-i-node "chicken" 100
                            (make-i-node "bacon" 2000
                                         (make-i-node "artichoke hearts" 100 
                                                      (make-i-node "apples" 50 empty empty)
                                                      empty)
                                         (make-i-node "broccoli" 50 empty empty))
                            (make-i-node "feta cheese" 0 empty empty))
               (make-i-node "pepperoni" 5001
                            (make-i-node "onions" 2000 
                                         (make-i-node "mushrooms" 1500 empty empty)
                                         empty)
                            (make-i-node "tomatoes" 1
                                         (make-i-node "sundried tomatoes" 100 empty empty)
                                         empty))))

(define sample-i-tree5
  (make-i-node "italian sausage" 0
               (make-i-node "chicken" 0
                            (make-i-node "bacon" 0
                                         (make-i-node "artichoke hearts" 0 
                                                      (make-i-node "apples" 1 empty empty)
                                                      empty)
                                         (make-i-node "broccoli" 0 empty empty))
                            (make-i-node "feta cheese" 0 empty empty))
               (make-i-node "pepperoni" 0
                            (make-i-node "onions" 0 
                                         (make-i-node "mushrooms" 0 empty empty)
                                         empty)
                            (make-i-node "tomatoes" 0
                                         (make-i-node "sundried tomatoes" 0 empty empty)
                                         empty))))


;; (out-of-ingredients? ingredients) produces true if any nodes of ingredients
;;   have quantity zero, and false otherwise
;; out-of-ingredients: I-Tree -> Bool
;; Examples:
(check-expect (out-of-ingredients? sample-i-tree) false)
(check-expect (out-of-ingredients? sample-i-tree2) true)

(define (out-of-ingredients? ingredients)
  (cond
    [(empty? ingredients) false]
    [(zero? (i-node-quantity ingredients)) true]
    [else (or (out-of-ingredients? (i-node-left ingredients))
              (out-of-ingredients? (i-node-right ingredients)))]))

;; Tests:
(check-expect (out-of-ingredients? empty) false)
(check-expect (out-of-ingredients? sample-i-tree3) true)

;; (low-ingredients ingredients cutoff) produces an ordered list of all of the
;;   names of the nodes of ingredients whose quantities are stricly less than cutoff
;; low-ingredients: Num I-Tree -> (listof Str)
;; Examples:
(check-expect (low-ingredients sample-i-tree2 100) (list "broccoli" 
                                                         "feta cheese" 
                                                         "italian sausage" 
                                                         "tomatoes"))
(check-expect (low-ingredients sample-i-tree2 101) (list "artichoke hearts" 
                                                         "broccoli" 
                                                         "chicken" 
                                                         "feta cheese" 
                                                         "italian sausage"
                                                         "sundried tomatoes" 
                                                         "tomatoes"))

(define (low-ingredients ingredients cutoff)
  (cond
    [(empty? ingredients) empty]
    [(< (i-node-quantity ingredients) cutoff)
     (append (low-ingredients (i-node-left ingredients) cutoff)
             (list (i-node-ingredient ingredients))
             (low-ingredients (i-node-right ingredients) cutoff))]
    [else (append (low-ingredients (i-node-left ingredients) cutoff)
                  (low-ingredients (i-node-right ingredients) cutoff))]))

;; Tests:
(check-expect (low-ingredients empty 100) empty)
(check-expect (low-ingredients sample-i-tree 2) (list "tomatoes"))
(check-expect (low-ingredients sample-i-tree 1) empty)

;; (add-ingredient tree ingredient quantity) produces a new I-Tree which has
;;   the quantity of ingredient in I-Tree increased by quantity, or has a new
;;   leaf if ingredient is not already in tree
;; add-ingredient: I-Tree Str Nat -> I-Tree
;; Examples:
(check-expect (add-ingredient sample-i-tree2 "feta cheese" 200) sample-i-tree)
(check-expect (add-ingredient empty "feta cheese" 10) (make-i-node "feta cheese" 10 empty empty))

(define (add-ingredient tree ingredient quantity)
  (cond
    [(empty? tree) (make-i-node ingredient quantity empty empty)]
    [(string=? ingredient (i-node-ingredient tree))
     (make-i-node ingredient
                  (+ (i-node-quantity tree) quantity) 
                  (i-node-left tree) 
                  (i-node-right tree))]
    [(string<? ingredient (i-node-ingredient tree))  
     (make-i-node (i-node-ingredient tree) 
                  (i-node-quantity tree) 
                  (add-ingredient (i-node-left tree) ingredient quantity)
                  (i-node-right tree))]
    [else (make-i-node (i-node-ingredient tree) 
                       (i-node-quantity tree) 
                       (i-node-left tree)
                       (add-ingredient (i-node-right tree) ingredient quantity))]))

;; Tests:
(check-expect (add-ingredient sample-i-tree3 "artichoke hearts" 100)
                              sample-i-tree)
(check-expect (add-ingredient (make-i-node "middle value" 10 empty empty)
                               "ingredient"
                               5)
              (make-i-node "middle value" 
                           10
                           (make-i-node "ingredient" 5 empty empty) 
                           empty))

;; (add-shipment current new) adds all of the quantities of new to the 
;;   corresponding nodes of current, or creates new nodes if needed
;; add-shipment: I-Tree AL -> I-Tree
;; Examples:
(check-expect (add-shipment sample-i-tree2
                            '(("apples" 50) ("pepperoni" 1)))
              sample-i-tree4)
(check-expect (add-shipment (make-i-node "tomatoes" 10 empty empty)
                            '(("tomatoes" 10) ("garlic" 10)))
              (make-i-node "tomatoes" 20 (make-i-node "garlic" 10 empty empty) empty))

(define (add-shipment current new)
  (cond
    [(empty? new) current]
    [else (add-shipment (add-ingredient current
                                        (first (first new))
                                        (second (first new)))
                        (rest new))]))

;; Tests:
(check-expect (add-shipment sample-i-tree empty)
              sample-i-tree)
(check-expect (add-shipment empty '(("feta cheese" 2)))
              (make-i-node "feta cheese" 2 empty empty))

;; (accommodate? ingredients pizza) determines if pizza can be made with
;;   the contents of ingredients
;; accommodate?: I-Tree Pizza -> Bool
;; Examples:
(check-expect (accommodate? sample-i-tree2
                           (make-pizza 'medium '("bacon") '("artichoke hearts")))
              true)
(check-expect (accommodate? sample-i-tree2
                           (make-pizza 'large empty '("feta cheese")))
              false)

(define (accommodate? ingredients pizza)
  (local [;; (subtract-ingredient ingredients ingredient) removes an amount
          ;;   of ingredient from the I-Tree ingredients based on size of pizza
          ;; subtract-ingredient: I-Tree Str -> I-Tree
          (define (subtract-ingredient ingredients ingredient)
            (cond
              [(symbol=? (pizza-size pizza) 'small)
               (add-ingredient ingredients ingredient -1)]
              [(symbol=? (pizza-size pizza) 'medium)
               (add-ingredient ingredients ingredient -1.5)]
              [(symbol=? (pizza-size pizza) 'large)
               (add-ingredient ingredients ingredient -2)]
              [(symbol=? (pizza-size pizza) 'xl)
               (add-ingredient ingredients ingredient -2)]))
          ;; (subtract-all-ingredients ingredients pizza) removes all units of
          ;;   toppings that appear on pizza from ingredients necessary to make pizza
          ;; subtract-all-ingredients: I-Tree Pizza -> I-Tree
          (define (subtract-all-ingredients ingredients pizza)
            (cond
              [(and (empty? (pizza-std-tops pizza)) 
                    (empty? (pizza-prm-tops pizza)))
               ingredients]
              [(not (empty? (pizza-std-tops pizza)))
               (subtract-all-ingredients 
                (subtract-ingredient ingredients 
                                     (first (pizza-std-tops pizza)))
                (make-pizza (pizza-size pizza) 
                            (rest (pizza-std-tops pizza)) 
                            (pizza-prm-tops pizza)))]
              [else (subtract-all-ingredients 
                     (subtract-ingredient ingredients 
                                          (first (pizza-prm-tops pizza)))
                     (make-pizza (pizza-size pizza) 
                                 (pizza-std-tops pizza) 
                                 (rest (pizza-prm-tops pizza))))]))]
    (empty? (low-ingredients (subtract-all-ingredients ingredients pizza) 0))))
    
;; Tests:
(check-expect (accommodate? sample-i-tree5 (make-pizza 'small
                                                      (list "apples")
                                                      empty))
              true)
(check-expect (accommodate? sample-i-tree5 (make-pizza 'small
                                                      (list "pepperoni")
                                                      empty))
              false)
(check-expect (accommodate? sample-i-tree5 (make-pizza 'xl
                                                      empty
                                                      empty))
              true)