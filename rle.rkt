;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname rle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; October 14th, 2014
;; Assignment 5
;; Problem 2: Run Length Encoding

;; An RlePair is a:
;; (cons Any Nat)

;; An RleList is one of:
;; * empty
;; * (cons RlePair RleList)

;; (rle-decode encoded-list) consumes a list encoded by RLE and produces the
;;   original list
;; rle-decode: RleList -> (listof Any)
;; Examples:
(check-expect (rle-decode (list (list 6 5)
                                (list 'pink 2)))
              (list 6 6 6 6 6 'pink 'pink))
(check-expect (rle-decode (list (list "red" 0)
                                (list 45.6 2)))
              (list 45.6 45.6))

(define (rle-decode encoded-list)
  (cond
    [(empty? encoded-list) empty]
    [(empty?(first encoded-list))
     (rle-decode (rest encoded-list))]
    [(zero? (second (first encoded-list)))
     (rle-decode (rest encoded-list))]
    [else 
     (cons (first (first encoded-list))
           (rle-decode (cons (cons (first (first encoded-list))
                                   (cons (sub1 (second (first encoded-list))) 
                                         empty))
                             (rest encoded-list))))]))

;; Tests:
(check-expect (rle-decode empty)
              empty)
(check-expect (rle-decode (list (list "red" 0)
                                (list 45.6 0)))
              empty)
(check-expect (rle-decode (list empty
                                (list "red" 1)))
              (list "red"))
(check-expect (rle-decode (list (list 'a 5)
                                empty))
              (list 'a 'a 'a 'a 'a))
(check-expect (rle-decode (list empty
                                empty))
              empty)
(check-expect (rle-decode (list (list 'a 2)
                                (list 'b 2)
                                (list 'c 3)))
              (list 'a 'a 'b 'b 'c 'c 'c))