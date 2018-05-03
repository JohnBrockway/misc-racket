;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ironman) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; October 21st, 2014
;; Assignment 6
;; Problem 4: IronMan

(define-struct ironman-finish (name gender time))
;; An Ironman-Finish is a (make-ironman-finish Str (anyof 'M 'F) Num)

;; A Race-Result is one of:
;; * empty
;; (cons Ironman-Finish Race-Result)


;; (ironman-qualifiers race n) produces a list of lists of the top n
;;   males and females
;; ironman-qualifiers: Race-Result Nat -> (listof (listof Str))
;; Examples:
(check-expect (ironman-qualifiers 
               (list (make-ironman-finish "Paula Newby-Fraser" 'F 8.6)
                     (make-ironman-finish "Peter Reid" 'M 8.7)
                     (make-ironman-finish "Lori Bowden" 'F 9.1))
               1)
              (list (list "Peter Reid") (list "Paula Newby-Fraser")))
(check-expect (ironman-qualifiers 
               (list (make-ironman-finish "Paula Newby-Fraser" 'F 8.6)
                     (make-ironman-finish "Peter Reid" 'M 8.7)
                     (make-ironman-finish "Lori Bowden" 'F 9.1))
               2)
              (list (list "Peter Reid") (list "Paula Newby-Fraser" "Lori Bowden")))

(define (ironman-qualifiers race n)
  (list (get-n-gender race n 'M) (get-n-gender race n 'F)))

;; Tests:
(check-expect (ironman-qualifiers empty 2)
              (list empty empty))
(check-expect (ironman-qualifiers 
               (list (make-ironman-finish "Paula Newby-Fraser" 'F 8.6)
                     (make-ironman-finish "Peter Reid" 'M 8.7)
                     (make-ironman-finish "Richard Bowden" 'M 9.1)
                     (make-ironman-finish "Lisa Bently" 'F 9.3))
               2)
              (list (list "Peter Reid" "Richard Bowden") (list "Paula Newby-Fraser" "Lisa Bently")))
(check-expect (ironman-qualifiers 
               (list (make-ironman-finish "Paula Newby-Fraser" 'F 8.6)
                     (make-ironman-finish "Peter Reid" 'M 8.7)
                     (make-ironman-finish "Richard Bowden" 'M 9.1)
                     (make-ironman-finish "Lisa Bently" 'F 9.3))
               0)
              (list empty empty))

;; (get-n-gender race n gender) is a helper to get the first n names from race for
;;   a specific gender, or as many as exist if that is less than n
;; get-n-gender: Race-Result Nat (anyof 'M 'F) -> (listof String)
;; Examples:
(check-expect (get-n-gender (list (make-ironman-finish "Paula Newby-Fraser" 'F 8.6)
                                  (make-ironman-finish "Peter Reid" 'M 8.7)
                                  (make-ironman-finish "Lori Bowden" 'F 9.1)) 
                            2 
                            'M)
              (list "Peter Reid"))
(check-expect (get-n-gender (list (make-ironman-finish "Paula Newby-Fraser" 'F 8.6)
                                  (make-ironman-finish "Peter Reid" 'M 8.7)
                                  (make-ironman-finish "Richard Bowden" 'M 9.1)) 
                            1 
                            'M)
              (list "Peter Reid"))

(define (get-n-gender race n gender)
  (cond
    [(= n 0) empty]
    [(empty? race) empty]
    [(symbol=? gender (ironman-finish-gender (first race)))
     (cons (ironman-finish-name (first race))
           (get-n-gender (rest race) (sub1 n) gender))]
    [else (get-n-gender (rest race) n gender)]))

;; Tests:
(check-expect (get-n-gender empty 1 'F)
              empty)
(check-expect (get-n-gender (list (make-ironman-finish "Paula Newby-Fraser" 'F 8.6)
                                  (make-ironman-finish "Peter Reid" 'M 8.7)
                                  (make-ironman-finish "Richard Bowden" 'M 9.1)
                                  (make-ironman-finish "Lisa Bently" 'F 9.3)) 
                            0 
                            'M)
              empty)
(check-expect (get-n-gender (list (make-ironman-finish "Paula Newby-Fraser" 'F 8.6)
                                  (make-ironman-finish "Peter Reid" 'M 8.7)
                                  (make-ironman-finish "Richard Bowden" 'M 9.1)
                                  (make-ironman-finish "Lisa Bently" 'F 9.3)) 
                            2
                            'F)
              (list "Paula Newby-Fraser" "Lisa Bently"))