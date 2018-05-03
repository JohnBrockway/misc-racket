;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname my-rational) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; October 21st, 2014
;; Assignment 6
;; Problem 1: Rational Numbers

(define-struct my-rational (num denom))
;; A My-Rational is a (make-my-rational Int Int) where num is the 
;;   numerator, and denom is the denominator.
;; requires: denom is not zero


;; (reduce-my-rational rat) reduces a rational number, rat, by dividing both
;;   the numerator and the denominator by their greatest common factor
;; reduce-my-rational: My-Rational -> My-Rational
;; Examples:
(check-expect (reduce-my-rational (make-my-rational 10 25))
              (make-my-rational 2 5))
(check-expect (reduce-my-rational (make-my-rational 1 2))
              (make-my-rational 1 2))

(define (reduce-my-rational rat)
  (cond
    [(= (my-rational-num rat) 0)
     (make-my-rational 0 1)]
    [(< (my-rational-denom rat) 0) (make-my-rational (- (/ (my-rational-num rat)
                                                           (gcd (my-rational-num rat)
                                                                (my-rational-denom rat))))
                                                     (- (/ (my-rational-denom rat)
                                                           (gcd (my-rational-num rat)
                                                                (my-rational-denom rat)))))]
    [else (make-my-rational (/ (my-rational-num rat)
                               (gcd (my-rational-num rat)
                                    (my-rational-denom rat)))
                            (/ (my-rational-denom rat)
                               (gcd (my-rational-num rat)
                                    (my-rational-denom rat))))]))

;; Tests:
(check-expect (reduce-my-rational (make-my-rational 0 5))
              (make-my-rational 0 1))
(check-expect (reduce-my-rational (make-my-rational 5 5))
              (make-my-rational 1 1))
(check-expect (reduce-my-rational (make-my-rational 7 5))
              (make-my-rational 7 5))
(check-expect (reduce-my-rational (make-my-rational 7 -5))
              (make-my-rational -7 5))
(check-expect (reduce-my-rational (make-my-rational -7 -5))
              (make-my-rational 7 5))

;; (add-my-rationals rat1 rat2) consumes two rationals and produces their sum
;;   in lowest terms
;; add-my-rationals: My-Rational My-Rational -> My-Rational
;; Examples:
(check-expect (add-my-rationals (make-my-rational 1 4)
                                (make-my-rational 3 4))
              (make-my-rational 1 1))
(check-expect (add-my-rationals (make-my-rational 6 10)
                                (make-my-rational 14 9))
              (make-my-rational 97 45))

(define (add-my-rationals rat1 rat2)
  (reduce-my-rational (make-my-rational (+ (* (/ (lcm (my-rational-denom rat1) 
                                                      (my-rational-denom rat2))
                                                 (my-rational-denom rat1))
                                              (my-rational-num rat1))
                                           (* (/ (lcm (my-rational-denom rat1) 
                                                      (my-rational-denom rat2))
                                                 (my-rational-denom rat2))
                                              (my-rational-num rat2)))
                                        (lcm (my-rational-denom rat1)
                                             (my-rational-denom rat2)))))

;; Tests:
(check-expect (add-my-rationals (make-my-rational 0 10)
                                (make-my-rational 14 4))
              (make-my-rational 7 2))
(check-expect (add-my-rationals (make-my-rational 0 10)
                                (make-my-rational 0 9))
              (make-my-rational 0 1))
(check-expect (add-my-rationals (make-my-rational 6 10)
                                (make-my-rational 12 20))
              (make-my-rational 6 5))

;; (cumulative-lor lst) produces the list of cumulative sums of list
;; cumulative-lor: (listof My-Rational) -> (listof My-Rational)
;; Examples:
(check-expect (cumulative-lor (list (make-my-rational 1 1)))
              (list (make-my-rational 1 1)))
(check-expect (cumulative-lor (list (make-my-rational 0 10)
                                    (make-my-rational 5 2)
                                    (make-my-rational 3 1)))
              (list (make-my-rational 0 1)
                    (make-my-rational 5 2)
                    (make-my-rational 11 2)))

(define (cumulative-lor lst)
  (cond
    [(empty? lst) empty]
    [else (sums-list-acc lst
                        (make-my-rational 0 1))]))

;; Tests:
(check-expect (cumulative-lor empty)
              empty)
(check-expect (cumulative-lor (list (make-my-rational 0 1)))
              (list (make-my-rational 0 1)))

;; (sums-list-acc lst total) is a helper to cumulative-lor that carries an
;;   accumulator of the currect total to add every element of the list
;; sums-list-acc: (listof My-Rational) My-Rational -> (listof My-Rational)
;; Examples:
(check-expect (sums-list-acc (list (make-my-rational 0 1)
                                   (make-my-rational 5 1)
                                   (make-my-rational 3 1))
                             (make-my-rational 0 1))
              (list (make-my-rational 0 1)
                    (make-my-rational 5 1)
                    (make-my-rational 8 1)))
(check-expect (sums-list-acc (list (make-my-rational 5 1)
                                   (make-my-rational 9 -3))
                             (make-my-rational 0 1))
              (list (make-my-rational 5 1)
                    (make-my-rational 2 1)))

(define (sums-list-acc lst total)
  (cond
    [(empty? lst) empty]
    [else (cons (reduce-my-rational (add-my-rationals (first lst)
                                                      total))
                (sums-list-acc (rest lst) 
                              (add-my-rationals (first lst) total)))]))

;; Tests:
(check-expect (sums-list-acc empty
                             (make-my-rational 0 1))
              empty)
(check-expect (sums-list-acc (list (make-my-rational 5 1))
                             (make-my-rational 0 1))
              (list (make-my-rational 5 1)))
(check-expect (sums-list-acc (list (make-my-rational 0 1)
                                   (make-my-rational 0 -3))
                             (make-my-rational 0 1))
              (list (make-my-rational 0 1)
                    (make-my-rational 0 1)))