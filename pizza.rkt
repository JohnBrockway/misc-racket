;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pizza) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; October 7th, 2014
;; Assignment 4
;; Problem 2: Racket Pizza


(define-struct std-top (topping))
;; A Std-Top is a (make-std-top Sym)
;; requires: topping be one of: 'onions, 'tomatoes, 'pepperoni, 'bacon

(define-struct prm-top (topping))
;; A Prm-Top is a (make-prm-top Sym)
;; requires: topping be one of: 'chicken, 'feta-cheese, 'broccoli

(define-struct pizza (size standards premiums))
;; A Pizza is a (make-pizza Sym (listof Std-Top) (listof Prm-Top))
;; requires: size be one of: 'small, 'medium, 'large, 'xl

;; (revenue list-of-pizzas) calculates the total price of a list of Pizza
;; revenue: (listof Pizza) -> Num
;; Examples:
(check-expect (revenue (cons (make-pizza 'large 
                                   (cons (make-std-top 'onions) empty)
                                   (cons (make-prm-top 'chicken) 
                                         (cons (make-prm-top 'broccoli) 
                                               empty))) 
                             empty))
                       11.75)
(check-expect (revenue (cons (make-pizza 'small
                                   empty
                                   (cons (make-prm-top 'feta-cheese) empty))
                       (cons (make-pizza 'medium
                                   (cons (make-std-top 'pepperoni) empty)
                                   empty) empty)))
                       14)

(define (revenue list-of-pizzas)
  (cond
   [(empty? list-of-pizzas) 0]
   [else (+ (* (length (pizza-standards (first list-of-pizzas))) 0.75)
            (* (length (pizza-premiums (first list-of-pizzas))) 1.25)
            (cond
             [(symbol=? (pizza-size (first list-of-pizzas)) 'small) 5]
             [(symbol=? (pizza-size (first list-of-pizzas)) 'medium) 7]
             [(symbol=? (pizza-size (first list-of-pizzas)) 'large) 8.5]
             [(symbol=? (pizza-size (first list-of-pizzas)) 'xl) 10])
            (revenue (rest list-of-pizzas)))]))
          
;; Tests:
(check-expect (revenue empty)
              0)
(check-expect (revenue (cons (make-pizza 'xl
                                         (cons (make-std-top 'tomatoes) 
                                               (cons (make-std-top 'bacon)
                                                     (cons (make-std-top 'pepperoni) empty)))
                             (cons (make-prm-top 'chicken) empty))
                             (cons (make-pizza 'small
                                               (cons (make-std-top 'onions) empty)
                                               empty)
                                   (cons (make-pizza 'small
                                                     empty
                                                     (cons (make-prm-top 'feta-cheese) empty)) empty))))
              25.5)

;; (accommodate? pizza topping) consumes a pizza and a topping and produces true
;;   if the item is not found on the pizza and false if it is
;; accommodate: Pizza (anyof Std-Top Prm-Top) -> Bool
;; Examples:
(check-expect (accommodate? (make-pizza 'small
                                       (cons (make-std-top 'tomatoes) empty)
                                       empty)
                            (make-std-top 'onions))
              true)
(check-expect (accommodate? (make-pizza 'xl
                                        empty
                                       (cons (make-prm-top 'feta-cheese) empty))
                            (make-prm-top 'feta-cheese))
              false)

(define (accommodate? pizza topping)
  (cond
    [(member? topping (pizza-standards pizza)) false]
    [(member? topping (pizza-premiums pizza)) false]
    [else true]))

;; Tests:
(check-expect (accommodate? (make-pizza 'large
                                        (cons (make-std-top 'onions) empty)
                                        (cons (make-prm-top 'broccoli) empty))
                            (make-std-top 'onions))
              false)
(check-expect (accommodate? (make-pizza 'large
                                        (cons (make-std-top 'onions) empty)
                                        (cons (make-prm-top 'broccoli) empty))
                            (make-prm-top 'broccoli))
              false)
(check-expect (accommodate? (make-pizza 'large
                                        (cons (make-std-top 'onions) empty)
                                        (cons (make-prm-top 'broccoli) empty))
                            (make-std-top 'tomatoes))
              true)
(check-expect (accommodate? (make-pizza 'large
                                        empty
                                        empty)
                            (make-std-top 'onions))
              true)
                                        