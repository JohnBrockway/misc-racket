;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cards) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; September 30th, 2014
;; Assignment 3
;; Problem 3: Best Hand


(define-struct card (value suit))
;; A Card is a (make-card Nat Sym)
;;   requires: Value be an integer in the range 1 to 13, inclusive
;;             Sym be one of the following: 'clubs , 'diamonds, 'hearts, 'spades

(define-struct hand (c1 c2 c3))
;; A Hand is a (make-hand Card Card Card)


;; (hand-calc h) consumes a hand and returns the value of that hand
;; hand-calc: Hand -> Nat
;; Examples:
(check-expect (hand-calc (make-hand (make-card 4 'hearts)
                                    (make-card 13 'hearts)
                                    (make-card 10 'hearts)))
              520)
(check-expect (hand-calc (make-hand (make-card 4 'hearts)
                                    (make-card 13 'hearts)
                                    (make-card 10 'diamonds)))
              67108864)

(define (hand-calc h)
  (cond [(and (symbol=? (card-suit (hand-c1 h)) (card-suit (hand-c2 h)))
              (symbol=? (card-suit (hand-c1 h)) (card-suit (hand-c3 h))))
         (* (card-value (hand-c1 h)) (card-value (hand-c2 h)) (card-value (hand-c3 h)))]
        [(symbol=? (card-suit (hand-c1 h)) (card-suit (hand-c2 h)))
         (cond
           [(> (expt (card-value (hand-c1 h)) (card-value (hand-c2 h)))
               (expt (card-value (hand-c2 h)) (card-value (hand-c1 h))))
            (expt (card-value (hand-c1 h)) (card-value (hand-c2 h)))]
           [else (expt (card-value (hand-c2 h)) (card-value (hand-c1 h)))])]
        [(symbol=? (card-suit (hand-c2 h)) (card-suit (hand-c3 h)))
         (cond
           [(> (expt (card-value (hand-c2 h)) (card-value (hand-c3 h)))
               (expt (card-value (hand-c3 h)) (card-value (hand-c2 h))))
            (expt (card-value (hand-c2 h)) (card-value (hand-c3 h)))]
           [else (expt (card-value (hand-c3 h)) (card-value (hand-c2 h)))])]
        [(symbol=? (card-suit (hand-c1 h)) (card-suit (hand-c3 h)))
         (cond
           [(> (expt (card-value (hand-c1 h)) (card-value (hand-c3 h)))
               (expt (card-value (hand-c3 h)) (card-value (hand-c1 h))))
            (expt (card-value (hand-c1 h)) (card-value (hand-c3 h)))]
           [else (expt (card-value (hand-c3 h)) (card-value (hand-c1 h)))])]
        [else
         (cond
           [(and (>= (* (card-value (hand-c1 h)) (card-value (hand-c2 h)))
                     (* (card-value (hand-c2 h)) (card-value (hand-c3 h))))
                 (>= (* (card-value (hand-c1 h)) (card-value (hand-c2 h)))
                     (* (card-value (hand-c1 h)) (card-value (hand-c3 h)))))
            (* (card-value (hand-c1 h)) (card-value (hand-c2 h)))]
           [(and (>= (* (card-value (hand-c1 h)) (card-value (hand-c3 h)))
                     (* (card-value (hand-c2 h)) (card-value (hand-c3 h))))
                 (>= (* (card-value (hand-c1 h)) (card-value (hand-c3 h)))
                     (* (card-value (hand-c1 h)) (card-value (hand-c2 h)))))
            (* (card-value (hand-c1 h)) (card-value (hand-c3 h)))]
           [else (* (card-value (hand-c2 h)) (card-value (hand-c3 h)))])]))

;; Tests:   
(check-expect (hand-calc (make-hand (make-card 4 'hearts)
                                    (make-card 13 'hearts)
                                    (make-card 10 'hearts)))
              520)
(check-expect (hand-calc (make-hand (make-card 4 'hearts)
                                    (make-card 13 'hearts)
                                    (make-card 10 'diamonds)))
              67108864)
(check-expect (hand-calc (make-hand (make-card 4 'hearts)
                                    (make-card 13 'clubs)
                                    (make-card 10 'diamonds)))
              130)

;; (best-hand h1 h2) consumes h1 and h2 and produces whichever
;;   hand is the better of the two
;; best-hand: Hand Hand -> Hand
;; Examples:
(check-expect (best-hand (make-hand (make-card 4 'hearts)
                                    (make-card 13 'hearts)
                                    (make-card 10 'hearts))
                         (make-hand (make-card 4 'diamonds)
                                    (make-card 10 'clubs)
                                    (make-card 1 'spades)))
              (make-hand (make-card 4 'hearts)
                         (make-card 13 'hearts)
                         (make-card 10 'hearts)))
(check-expect (best-hand (make-hand (make-card 6 'hearts)
                                    (make-card 12 'hearts)
                                    (make-card 4 'clubs))
                         (make-hand (make-card 2 'diamonds)
                                    (make-card 8 'diamonds)
                                    (make-card 1 'diamonds)))
              (make-hand (make-card 6 'hearts)
                         (make-card 12 'hearts)
                         (make-card 4 'clubs)))

(define (best-hand h1 h2)
  (cond
    [(> (hand-calc h1) (hand-calc h2))
     h1]
    [(= (hand-calc h1) (hand-calc h2))
     h1]
    [else h2]))

;; Tests:
(check-expect (best-hand (make-hand (make-card 7 'clubs)
                                    (make-card 12 'hearts)
                                    (make-card 4 'clubs))
                         (make-hand (make-card 2 'hearts)
                                    (make-card 8 'diamonds)
                                    (make-card 9 'diamonds)))
              (make-hand (make-card 2 'hearts)
                         (make-card 8 'diamonds)
                         (make-card 9 'diamonds)))
(check-expect (best-hand (make-hand (make-card 6 'hearts)
                                    (make-card 12 'hearts)
                                    (make-card 4 'clubs))
                         (make-hand (make-card 6 'diamonds)
                                    (make-card 12 'diamonds)
                                    (make-card 4 'clubs)))
              (make-hand (make-card 6 'hearts)
                         (make-card 12 'hearts)
                         (make-card 4 'clubs)))
(check-expect (best-hand (make-hand (make-card 2 'hearts)
                                    (make-card 8 'diamonds)
                                    (make-card 9 'diamonds))
                         (make-hand (make-card 7 'clubs)
                                    (make-card 12 'hearts)
                                    (make-card 4 'clubs)))
              (make-hand (make-card 2 'hearts)
                         (make-card 8 'diamonds)
                         (make-card 9 'diamonds)))
  
  
