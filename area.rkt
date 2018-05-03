;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname area) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; September 30th, 2014
;; Assignment 3
;; Problem 2: Triangular Area


(define-struct 3dposn (x y z))
;; A 3dPosn is a (make-3dposn Num Num Num)


;; (side-length p q) comsumes 3dPosns p and q and returns the length
;;   of the line connecting them
;; side-length: 3dPosn 3dPosn -> Num
;; Example:
(check-within (sidelength (make-3dposn 1 1 1)
                          (make-3dposn 3 4 5))
              5.385 0.001)
(check-within (sidelength (make-3dposn 1 1 1)
                          (make-3dposn 1 1 1))
              0 0.001)

(define (sidelength p q)
  (sqrt (+ (sqr (- (3dposn-x q) (3dposn-x p))) 
           (sqr (- (3dposn-y q) (3dposn-y p))) 
           (sqr (- (3dposn-z q) (3dposn-z p))))))

;; Tests:
(check-within (sidelength (make-3dposn 1 1 1)
                          (make-3dposn 1 1 1))
              0 0.001)
(check-within (sidelength (make-3dposn 1 -1 1)
                          (make-3dposn -1 1 3))
              3.464 0.001)

;; (collinear? p1 p2 p3) consumes 3 3D coordinates and returns
;;   true if they are collinear and false if not
;; collinear?: 3DPosn 3dPosn 3dPosn -> Bool
;; Examples:
(check-expect (collinear? (make-3dposn 1 1 1)
                          (make-3dposn 2 2 2)
                          (make-3dposn 3 3 3))
              true)
(check-expect (collinear? (make-3dposn 1 1 1)
                          (make-3dposn 2 2 2)
                          (make-3dposn 3 0 3))
              false)

(define (collinear? p1 p2 p3)
  (cond
    [(and (= (/ (- (3dposn-x p2) (3dposn-x p1)) 
                (- (3dposn-x p3) (3dposn-x p1)))
             (/ (- (3dposn-y p2) (3dposn-y p1)) 
                (- (3dposn-y p3) (3dposn-y p1))))
          (= (/ (- (3dposn-x p2) (3dposn-x p1)) 
                (- (3dposn-x p3) (3dposn-x p1)))
             (/ (- (3dposn-y p2) (3dposn-z p1)) 
                (- (3dposn-z p3) (3dposn-z p1)))))
     true]
    [else false]))

;; Tests:
(check-expect (collinear? (make-3dposn 1 -1 1)
                          (make-3dposn 2 2 2)
                          (make-3dposn 3 3 -3))
              false)
(check-expect (collinear? (make-3dposn 0 0 0)
                          (make-3dposn 2 2 2)
                          (make-3dposn 3 3 3))
              true)

;; (area-triangle p1 p2 p3) consumes 3 3D coordinates and returns
;;   the area of the triangle created by them, or 'Undefined if there
;;   is no triangle created
;; area-triangle: 3DPosn 3dPosn 3dPosn -> (anyof Num 'Undefined)
;; Examples:
(check-expect (area-triangle (make-3dposn 2 2 2)
                             (make-3dposn 3 3 3)
                             (make-3dposn 5 5 5))
              'Undefined)

(define (area-triangle p1 p2 p3)
  (cond
    [(collinear? p1 p2 p3) 'Undefined]
    [else (/ (sqrt (+ (* 2 (sqr (sidelength p2 p3)) (sqr (sidelength p3 p1))) 
                      (* 2 (sqr (sidelength p3 p1)) (sqr (sidelength p1 p2))) 
                      (* 2 (sqr (sidelength p1 p2)) (sqr (sidelength p2 p3)))
                      (- 0 (expt (sqr (sidelength p1 p2)) 4))
                      (- 0 (expt (sqr (sidelength p2 p3)) 4))
                      (- 0 (expt (sqr (sidelength p3 p1)) 4)))) 4)]))