;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blackjack) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; September 23rd, 2014
;; Assignment 2
;; Problem 3: Blackjack


;; (blackjack-hard player-score dealer-card) uses a given table to help a Blackjack
;;   player decide what action to take based on their score, player-score, and the dealer's visible
;;   card, dealer-card: hit, stay, double, or surrender
;; blackjack-hard player-score dealer-card: Nat Nat -> Sym
;;   requires: 5 <= player-score <= 20
;;             1 <= dealer-card <= 10
;; Examples:
(check-expect (blackjack-hard  10 7) 'double)
(check-expect (blackjack-hard 16 6) 'stay)
(check-expect (blackjack-hard 5 10) 'hit)

(define (blackjack-hard player-score dealer-card)
  (cond
    [(<= player-score 8) 'hit]
    [(= player-score 9) (cond
                          [(or (<= dealer-card 2) (>= dealer-card 7))
                           'hit]
                          [else 'double])]
    [(= player-score 10) (cond
                           [(or (= dealer-card 1) (= dealer-card 10))
                            'hit]
                           [else 'double])]
    [(= player-score 11) (cond
                           [(= dealer-card 1) 'hit]
                           [else 'double])]
    [(= player-score 12) (cond
                           [(or (<= dealer-card 3) (>= dealer-card 7))
                            'hit]
                           [else 'stay])]
    [(or 
      (= player-score 13)
      (= player-score 14)) (cond
                             [(or (= dealer-card 1) (>= dealer-card 7))
                              'hit]
                             [else 'stay])]
    [(= player-score 15) (cond
                           [(= dealer-card 10) 'surrender]
                           [(or (= dealer-card 1) (>= dealer-card 7))
                            'hit]
                           [else 'stay])]
    [(= player-score 16) (cond
                           [(or (>= dealer-card 9) (= dealer-card 1))  'surrender]
                           [(>= dealer-card 7)
                            'hit]
                           [else 'stay])]
    [else 'stay]))

;; Tests:
(check-expect (blackjack-hard 7 2) 'hit)
(check-expect (blackjack-hard 9 8) 'hit)
(check-expect (blackjack-hard 10 6) 'double)
(check-expect (blackjack-hard 11 2) 'double)
(check-expect (blackjack-hard 12 7) 'hit)
(check-expect (blackjack-hard 13 1) 'hit)
(check-expect (blackjack-hard 15 10) 'surrender)
(check-expect (blackjack-hard 16 10) 'surrender)
(check-expect (blackjack-hard 19 2) 'stay)