;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ft-arithmetic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; October 21st, 2014
;; Assignment 6
;; Problem 3: Fault-Tolerant Arithmetic

;; (ft-add val1 val2) provides the fault-tolerant sum of val1 and val2
;; ft-add: (anyof 'Inf '-Inf 'NaN Num) (anyof 'Inf '-Inf 'NaN Num)
;;         -> (anyof 'Inf '-Inf 'NaN Num)
;; Examples:
(check-expect (ft-add 5 10) 15)
(check-expect (ft-add '-Inf 5) '-Inf)

(define (ft-add val1 val2)
  (cond
    [(number? val1)
     (cond
       [(number? val2) (+ val1 val2)]
       [(symbol=? val2 'Inf) 'Inf]
       [(symbol=? val2 '-Inf) '-Inf]
       [(symbol=? val2 'NaN) 'NaN])]
    [(symbol=? val1 'Inf)
     (cond
       [(number? val2) 'Inf]
       [(symbol=? val2 'Inf) 'Inf]
       [(symbol=? val2 '-Inf) 'NaN]
       [(symbol=? val2 'NaN) 'NaN])]
    [(symbol=? val1 '-Inf)
     (cond
       [(number? val2) '-Inf]
       [(symbol=? val2 'Inf) 'NaN]
       [(symbol=? val2 '-Inf) '-Inf]
       [(symbol=? val2 'NaN) 'NaN])]
    [(symbol=? val1 'NaN) 'NaN]))

;; Tests:
(check-expect (ft-add 'Inf 5) 'Inf)
(check-expect (ft-add 'Inf 'Inf) 'Inf)
(check-expect (ft-add 'Inf '-Inf) 'NaN)
(check-expect (ft-add 'Inf 'NaN) 'NaN)
(check-expect (ft-add '-Inf 5) '-Inf)
(check-expect (ft-add '-Inf 'Inf) 'NaN)
(check-expect (ft-add '-Inf '-Inf) '-Inf)
(check-expect (ft-add '-Inf 'NaN) 'NaN)
(check-expect (ft-add 'NaN 5) 'NaN)
(check-expect (ft-add 'NaN 'Inf) 'NaN)
(check-expect (ft-add 'NaN '-Inf) 'NaN)
(check-expect (ft-add 'NaN 'NaN) 'NaN)
(check-expect (ft-add 6 5) 11)
(check-expect (ft-add 6 'Inf) 'Inf)
(check-expect (ft-add 6 '-Inf) '-Inf)
(check-expect (ft-add 6 'NaN) 'NaN)
     
;; (ft-sum-list lst) adds all the elements of lst according to fault
;;   tolerant arithmetic
;; (listof (anyof 'Inf '-Inf 'NaN Num)) -> (anyof 'Inf '-Inf 'NaN Num)
;; Examples:
(check-expect (ft-sum-list (list '-Inf 8 '-Inf))
              '-Inf)
(check-expect (ft-sum-list (list '-Inf 'NaN 'Inf))
              'NaN)

(define (ft-sum-list lst)
  (cond
    [(empty? lst) 0]
    [else (ft-add (first lst)
                  (ft-sum-list (rest lst)))]))

;; Tests:
(check-expect (ft-sum-list empty)
              0)
(check-expect (ft-sum-list (list '-Inf))
              '-Inf)
(check-expect (ft-sum-list (list 5 6 7))
              18)

;; (ft-sum-many-lists lst) produces a list of the sums of each of its
;;   respective list elements, according to fault tolerant arithmetic
;; ft-sum-many-lists: (listof (listof (anyof 'Inf '-Inf 'NaN Num)))
;;                    -> (listof (anyof 'Inf '-Inf 'NaN Num))
;; Examples:
(check-expect (ft-sum-many-lists (list (list 5 6 7)
                                       (list '-Inf 6 'NaN)
                                       empty))
              (list 18 'NaN 0))
(check-expect (ft-sum-many-lists (list (list 5 6 '-Inf)
                                       (list '-Inf 6 'NaN)
                                       (list 'Inf)))
              (list '-Inf 'NaN 'Inf))

(define (ft-sum-many-lists lst)
  (cond
    [(empty? lst) empty]
    [else (cons (ft-sum-list (first lst))
                (ft-sum-many-lists (rest lst)))]))

;; Tests:
(check-expect (ft-sum-many-lists empty)
              empty)
(check-expect (ft-sum-many-lists (list (list 2 'NaN '-Inf)))
              (list 'NaN))