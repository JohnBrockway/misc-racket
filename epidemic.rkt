;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname epidemic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; November 11th, 2014
;; Assignment 8
;; Problem 2: Epidemic

(define-struct vector (name date-infected people-infected))
;; A Vector is a (make-vector (String Nat (listof Vector))

;; An ITree (Infection-Tree) is one of:
;; * empty
;; * Vector
;; requires: All elements of people-infected must have a date-infected
;;           that is non-strictly higher than the date-infected of the Vector

(define bob (make-vector "bob" 
                         0 
                         (list (make-vector "jon" 
                                            2 
                                            (list (make-vector "emily" 9 empty)))
                               (make-vector "james" 
                                            3 
                                            (list (make-vector "jane" 9 empty))))))


;; (total-infected root) produces the total number of nodes stemming from
;;   root
;; total-infected: ITree -> Nat
;; Examples:
(check-expect (total-infected (make-vector "bob" 
                                           0 
                                           (list (make-vector "jon" 2 empty))))
              1)
(check-expect (total-infected (make-vector "bob" 
                                           0 
                                           (list (make-vector "jon" 2 empty)
                                                 (make-vector "james" 3 empty))))
              2)

(define (total-infected root)
  (local [;; (apply-to-all list) applies total-infected to every element of a
          ;;   list and every subsequent element of the element's lists
          ;; apply-to-all: (listof Vector) -> Num
          (define (apply-to-all list)
            (cond
              [(empty? list) 0]
              [else (+ (total-infected (first list))
                       (apply-to-all (rest list)))]))]
    (cond
      [(empty? root) 0]
      [else (+ (length (vector-people-infected root))
               (apply-to-all (vector-people-infected root)))])))

;; Tests:
(check-expect (total-infected empty)
              0)
(check-expect (total-infected 
               (make-vector "bob" 
                            0 
                            (list (make-vector "jon" 2 empty)
                                  (make-vector "james" 
                                               3 
                                               (list (make-vector "jane" 9 empty))))))
              3)

