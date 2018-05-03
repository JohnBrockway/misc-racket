;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a09) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; November 18th, 2014
;; Assignment 9
;; Problem 1: List Functions and Iris Data

(require "irisdata.rkt")


;; A Number Association List (N-AL) is one of:
;; * empty
;; * (cons (list Num Any) N-AL)

;; A Feature Vector (FV) is a (listof Num)

;; A Class Label (CL) is a Nat

;; A Feature Vector Association List (FV-AL) is one of:
;; * empty
;; * (cons (list FV CL) FV-AL)
;; requires: each FV must be of the same length


;; (zip lst2 lst2) consumes two lists and produces a list of pairs
;; zip: (listof Any) (listof Any) -> (listof (list Any Any))
;; requires: lst1 and lst2 be the same length
;; Examples:
(check-expect (zip '(red blue purple) '(1 2 3))
              (list '(red 1) '(blue 2) '(purple 3)))
(check-expect (zip (list 1 3 5) (list 2 4 6))
              (list '(1 2) '(3 4) '(5 6)))

(define (zip lst1 lst2)
  (map (lambda (x y) (cons x (cons y empty))) 
       lst1 lst2))

;; Tests:
(check-expect (zip empty empty) empty)
(check-expect (zip '(1) '(2))
              '((1 2)))

;; (unzip lst) consumes a list of pairs and produces a list of 2 lists
;; unzip: (listof (list Any Any)) -> (list (listof Any) (listof Any))
;; Examples:
(check-expect (unzip (list '(red 1) '(blue 2) '(purple 3)))
              (list '(red blue purple) '(1 2 3)))
(check-expect (unzip (list (list "hello" 1) (list "hi" 2)))
              (list (list "hello" "hi") (list 1 2)))

(define (unzip lst)
  (cons (map (lambda (x) (first x)) lst)
        (list (map (lambda (x) (second x)) lst))))

;; Tests:
(check-expect (unzip empty) (list empty empty))
(check-expect (unzip (list '(1 2) '(3 4)))
              (list '(1 3) '(2 4)))

;; (dedup lst) produces a list with all of the unique elements of lst,
;;   removing any duplicates
;; dedup: (listof Num) -> (listof Num)
;; Examples:
(check-expect (dedup '(1 3 4 2 4 2 7))
              '(1 3 4 2 7))
(check-expect (dedup '(1 1 1 1 1 1 1))
              '(1))

(define (dedup lst)
  (foldr (lambda (x y) (cond
                         [(equal? y empty) (list x)]
                         [(member? x y) y]
                         [else (cons x y)]))
         empty lst))

;; Tests:
(check-expect (dedup empty) empty)
(check-expect (dedup '(1)) '(1))
(check-expect (dedup '(1 2 3)) '(1 2 3))

;; (occurrences lst num) produces the amount of times num appears in lst
;; occurrences: (listof Num) Num -> Nat
;; Examples:
(check-expect (occurrences '(1 2 3 4) 1) 1)
(check-expect (occurrences '(1 2 1 2) 1) 2)

(define (occurrences lst num)
  (foldr (lambda (x y) (cond
                         [(= x num) (add1 y)]
                         [else y]))
         0 lst))

;; Tests:
(check-expect (occurrences empty 3) 0)
(check-expect (occurrences '(2) 4) 0)
(check-expect (occurrences '(2 4 6 2 4 6) 6) 2)

;; (occurrences-al lst) produces an N-AL where the keys are the unique
;;   elements of lst and the values are the amount of occurrences
;; occurrences-al: (listof Num) -> N-AL
;; Examples:
(check-expect (occurrences-al '(1 2 3))
              (list '(1 1) '(2 1) '(3 1)))
(check-expect (occurrences-al '(1 1 2 3 4 1))
              (list '(2 1) '(3 1) '(4 1) '(1 3)))

(define (occurrences-al lst)
  (dedup (foldr (lambda (x y) (cons (cons x (list (occurrences lst x))) y)) 
                empty
                lst)))

;; Tests:
(check-expect (occurrences-al '(1 1 1 1))
              (list (list 1 4)))
(check-expect (occurrences-al empty) empty)

;; (max-pair n-al) produces the pair in n-al with the largest key
;; max-pair: N-AL -> (list Num Any)
;; Examples:
(check-expect (max-pair (list '(1 3) '(2 1) '(3 1) '(4 1)))
              '(4 1))
(check-expect (max-pair (list '(4 2) '(3 1) '(3.5 7)))
              '(4 2))

(define (max-pair n-al)
  (foldr (lambda (x y) (cond
                         [(or (empty? y)
                              (empty? x)
                              (> (first x) (first y))) x]
                         [else y]))
         empty
         n-al))

;; Tests:
(check-expect (max-pair empty) empty)
(check-expect (max-pair (list '(4 1) '(4 5) '(4 1) '(4 2)))
              '(4 2))

;; (euclidean-dist point1 point2) produces the euclidean distance between
;;   point1 and point2 if they are points in n space, n being the length
;;   of the lists
;; euclidean-dist: (listof Num) (listof Num) -> Num
;; requires: point1 and point2 be the same length
;; Examples:
(check-expect (euclidean-dist '(2 2 2 2) '(1 1 1 1))
              2)
(check-expect (euclidean-dist '(1 5 2) '(-1 7 3))
              3)

(define (euclidean-dist point1 point2)
  (sqrt (foldr + 0 (map sqr (map - point2 point1)))))

;; Tests:
(check-expect (euclidean-dist empty empty) 0)
(check-expect (euclidean-dist '(5 5 5 5) '(5 5 5 5)) 
              0)

;; (take lst n) produces the first n elements of lst
;; take: (listof X) Nat -> (listof X)
;; Examples:
(check-expect (take '(1 2 3 4 5 6) 4)
              '(1 2 3 4))
(check-expect (take (list 'a 'b 'c 'd) 1)
              (list 'a))

(define (take lst n)
  (cond
    [(empty? lst) empty]
    [(zero? n) empty]
    [else (cons (first lst) (take (rest lst) (sub1 n)))]))

;; Tests:
(check-expect (take '(1 2 3 4 5 6) 7)
              '(1 2 3 4 5 6))
(check-expect (take empty 3) empty)
(check-expect (take '(3 6 9) 0) empty)

;; (augment-with-distances fv fv-al distance) produces a list of triples where
;;   the first element of the ith triple is (distance) from fv to the ith
;;   element of fv-al, and the second and third are the first and second elements
;;   of that ith pair in fv-al
;; augment-with-distances: FV FV-AL ((listof Num) (listof Num) -> Num)
;;                         -> (listof (list Nat (listof Num) Nat))
;; requires: fv and all of the FV in fv-al be the same length
;; Examples:
(check-expect (augment-with-distances '(1 1 1 1) '(((1 1 1 1) 4)) euclidean-dist)
              '((0 (1 1 1 1) 4)))
(check-expect (augment-with-distances '(1 1 1 1) 
                                      (list (list (list 2 2 2 2) 2)
                                            (list (list 1 1 1 1) 1))
                                      euclidean-dist)
              (list (list 2 (list 2 2 2 2) 2)
                    (list 0 (list 1 1 1 1) 1)))

(define (augment-with-distances fv fv-al distance)
  (map (lambda (x) (list (distance fv (first x)) (first x) (second x)))
       fv-al))

;; Tests:
(check-expect (augment-with-distances '(0 0) 
                                      '(((1 0) 2) ((3 4) 3)) 
                                      euclidean-dist)
              '((1 (1 0) 2) (5 (3 4) 3)))
(check-expect (augment-with-distances '(1 2 3) empty euclidean-dist)
              empty)

;; (find-k-nearest fv fv-al k distance) finds the k pairs in fv-al
;;   that are closest to fv according to distance
;; find-k-nearest: FV FV-AL Nat ((listof Num) (listof Num) -> Num)
;;                 -> FV-AL
;; requires: k not be 0
;;           fv and all of the FV in fv-al be the same length
;; Examples:
(check-expect (find-k-nearest '(1 1)
                              '(((1 0) 5) ((1 1) 3) ((5 5) 2))
                              2
                              euclidean-dist)
              '(((1 1) 3) ((1 0) 5)))
(check-expect (find-k-nearest '(0)
                              '(((4) 2) ((8) 1) ((3) 4))
                              2
                              euclidean-dist)
              '(((3) 4) ((4) 2)))

(define (find-k-nearest fv fv-al k distance)
  (foldr (lambda (x y) (cons (rest x) y))
         empty
         (take (sort (augment-with-distances fv fv-al distance) 
                     (lambda (x y) (< (first x) (first y)))) k)))

;; Tests:
(check-expect (find-k-nearest '(7 5) empty 5 euclidean-dist)
              empty)
(check-expect (find-k-nearest '(2 3)
                              '(((2 3) 2) ((3 4) 2))
                              0
                              euclidean-dist)
              empty)

;; (find-within-d fv fv-al d distance) produces all of the pairs in fv-al
;;   that are less than or equal to d away from fv according to distance
;; find-within-d: FV FV-AL Num ((listof Num) (listof Num) -> Num)
;;                -> FV-AL
;; requires: d be non-negative
;;           fv and all of the FV in fv-al be the same length
;; Examples:
(check-expect (find-within-d '(0 0)
                             '(((1 0) 5) ((0 0) 3) ((5 5) 2))
                             1
                             euclidean-dist)
              '(((1 0) 5) ((0 0) 3)))
(check-expect (find-within-d '(0 0)
                             '(((1 0) 5) ((0 0) 3) ((5 5) 2))
                             0.5
                             euclidean-dist)
              '(((0 0) 3)))

(define (find-within-d fv fv-al d distance)
  (filter (lambda (x) (<= (distance fv (first x)) d)) fv-al))

;; Tests:
(check-expect (find-within-d '(4 3 2)
                             empty
                             1
                             euclidean-dist)
              empty)
(check-expect (find-within-d '(1 1 1)
                             '(((1 1 2) 2) ((2 2 2) 3))
                             1
                             euclidean-dist)
              '(((1 1 2) 2)))

;; (classify-k-nearest fv fv-al k distance) produces the most common class label
;;   among the k closest pairs in fv-al to fv according to distance
;; classify-k-nearest: FV FV-AL Nat ((listof Num) (listof Num) -> Num)
;;                     -> Nat
;; requires: k be non-zero
;; Examples:
(check-expect (classify-k-nearest '(3 4)
                                  '(((3 5) 4) ((2 4) 4) ((3 4) 8) ((5 6) 8) ((10 10) 8))
                                  3
                                  euclidean-dist)
              4)
(check-expect (classify-k-nearest '(0 0)
                                  '(((1 0) 1) ((-1 0) 1) ((0 1) 2))
                                  3
                                  euclidean-dist)
              1)

(define (classify-k-nearest fv fv-al k distance)
  (first (first (sort (occurrences-al 
                       (foldr (lambda (x y) (cons (second x) y))
                              empty
                              (find-k-nearest fv fv-al k distance)))
                      (lambda (x y) (> (second x) (second y)))))))

;; Tests:
(check-expect (classify-k-nearest '(0 0)
                                  '(((0 0) 1) ((0 0) 2) ((0 0) 3))
                                  2
                                  euclidean-dist)
              1)
(check-expect (classify-k-nearest '(0 0)
                                  '(((1 1) 2) ((1 2) 3) ((-1 1) 2) ((2 0) 3) ((10 10) 3))
                                  3
                                  euclidean-dist)
              2)

;; (classify-within-d fv fv-al d distance) produces the most common class label
;;   among the pairs in fv-al that are no more than d away from fv according
;;   to distance
;; classify-within-d: FV FV-AL Num ((listof Num) (listof Num) -> Num)
;;                    -> Nat
;; requires: d be non-negative
;; Examples:
(check-expect (classify-within-d '(3 4)
                                 '(((3 5) 4) ((2 4) 4) ((3 4) 8) ((5 6) 8) ((10 10) 8))
                                 1
                                 euclidean-dist)
              4)
(check-expect (classify-within-d '(0 0)
                                 '(((1 0) 1) ((-1 0) 2) ((0 1) 2) ((0 0) 1))
                                 0.5
                                 euclidean-dist)
              1)

(define (classify-within-d fv fv-al d distance)
  (local [(define result (sort (occurrences-al 
                                (foldr (lambda (x y) (cons (second x) y))
                                       empty
                                       (find-within-d fv fv-al d distance)))
                               (lambda (x y) (> (second x) (second y)))))]
    (cond [(empty? result) result]
          [else (first (first result))])))

;; Tests:
(check-expect (classify-within-d '(3 4)
                                 '(((3 5) 4) ((2 4) 4) ((3 7) 8) ((5 6) 8) ((10 10) 8))
                                 0
                                 euclidean-dist)
              empty)

;; (iris-species fv) produces the most common species name based on 
;;   the 5 closest neighbors to fv in iris-fv-al
;; iris-species: FV -> (anyof "setosa" "versicolor" "virginica")
;; Example:
(check-expect (iris-species '(0 0 0 0)) "setosa")

(define (iris-species fv)
  (local [(define most-common
            (classify-k-nearest fv iris-fv-al 5 euclidean-dist))]
    (cond
      [(= most-common 0) "setosa"]
      [(= most-common 1) "versicolor"]
      [(= most-common 2) "virginica"])))

;; Tests:
(check-expect (iris-species '(10 10 10 10)) "virginica")
(check-expect (iris-species '(6 3 4 2)) "versicolor")