#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is a list of functions and constants "provided" by this module
;; they are visible when you "require" this file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide    
 make-pizza
 pizza?
 pizza-size
 pizza-std-tops
 pizza-prm-tops
 
 make-i-node
 i-node?
 i-node-ingredient
 i-node-quantity
 i-node-left
 i-node-right
 
 make-file
 file?
 file-name
 file-size 
 file-timestamp
 
 make-dir
 dir?
 dir-name
 dir-contents
 
 fs-print
 i-print
 
 sample-fs
 sample-i-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data and type definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Don't worry about the #:transparent -- It's a Full Racket detail
;; that doesn't matter in the teaching languages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct pizza (size std-tops prm-tops) #:transparent)
;; A Pizza is a (make-pizza (anyof 'small 'medium 'large 'xl)
;;                          (listof Str) (listof Str))


;; An Ingredient Tree (I-Tree) is one of:
;; * empty
;; * I-Node

(define-struct i-node (ingredient quantity left right) #:transparent)
;; An I-Node is a (make-i-node Str Num I-Tree I-Tree)
;; requires: All ingredient(s) that appear in [left/right] subtree 
;;           must [precede/follow] (alphabetically) ingredient


;; A FileSystem is a:
;; * Dir

;; A FileDir is one of:
;; * File
;; * Dir

(define-struct file (name size timestamp) #:transparent)
;; A File is a (make-file Str Nat Nat)

(define-struct dir (name contents) #:transparent)
;; A Dir is a (make-dir Str (listof FileDir))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for Pretty Printing an I-Tree
;; (minimal design recipe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (i-print-indent lob) prints the indentation / lines of the tree
;;    each Bool in lob corresponds to a "depth level" of the tree and is true
;;    if we are on the "right" side
;; i-print-indent: (listof Bool) -> Str

(define (i-print/indent lob)
  (cond [(empty? lob) ""]
        [else (string-append 
               (cond
                 ;; lowest level and last entry, so put in 'elbows'
                 [(and (empty? (rest lob)) (first lob)) 
                  (string #\u250c #\u2500)]
                 [(empty? (rest lob)) (string #\u2514 #\u2500)]
                 ;; for higher levels, print vertical bar if
                 ;; two sequential left/rights are mismatched
                 [(not (equal? (first lob) (second lob)))
                  (string #\u2502 #\space)]
                 ;; otherwise, just white space
                 [else "  "])
               (i-print/indent (rest lob)))]))


;; (i-print-aux tree lob) prints each node of tree in order.
;;   lob 'accumulates' information about whether we're on the "right" or
;;   "left" side of each subtree
;; i-print-aux: I-Tree (listof Bool) -> Str

(define (i-print/aux tree lob)
  (cond
    [(empty? tree) ""]
    [else (string-append
           (i-print/aux (i-node-right tree) (append lob (list true)))
           (i-print/indent lob) 
           (i-node-ingredient tree) ":" 
           (number->string (i-node-quantity tree)) "\n"
           (i-print/aux (i-node-left tree) (append lob (list false))))]))


;; (i-print tree) pretty-prints an ingredient tree
;; i-print: I-Tree -> Void

(define (i-print tree)
  (display (i-print/aux tree empty)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for Pretty Printing a FileSystem
;; (minimal design recipe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (fd-print/indent lob) produces the indentation / lines of a FileDir
;;   each Boolean corresponds to a "level of indentation"
;;   and is true if we are on the last entry of that level
;; fd-print/indent: (listof Bool) -> Str

(define (fd-print/indent lob)
  (cond [(empty? lob) ""]
        [else 
         (string-append 
          (cond
            ;; lowest level and last entry, so use "L"
            [(and (first lob) (empty? (rest lob))) (string #\u2514 #\u2500)]
            ;; lowest level so use "T"
            [(empty? (rest lob)) (string #\u251c #\u2500)]
            ;; higher level, but last entry, so just blank
            [(first lob) (string #\space #\space)]
            ;; higher level, but not last entry so use "|"
            [else (string #\u2502 #\space)])
          ;; recurse to next (lower) level
          (fd-print/indent (rest lob)))]))


;; (fd-print fd lob) produces a string representation of a FileDir fd, 
;;   recursing if it's a Dir.  The lob ride-along is used for indentation.
;; fd-print: FileDir (listof Bool) -> Str

(define (fd-print fd lob)
  (cond 
    [(file? fd) (string-append (fd-print/indent lob)
                               (string #\u2500 #\space )
                               (file-name fd) "\n")]
    [else (string-append (fd-print/indent lob)
                         (string #\u25bc #\space ) 
                         (dir-name fd) "\n"
                         (lofd-print (dir-contents fd) lob))]))


;; (lofd-print lofd lob) recurses through a directory (lofd), dispatching
;;   each element to fd-print.  lob 'accumulates' information about whether
;;   or not it's the last entry in the dir
;; lofd-print: (listof FileDir) (listof Bool) -> Str

(define (lofd-print lofd lob)
  (cond
    [(empty? lofd) ""]
    [else (string-append 
           (fd-print (first lofd)
                     (append lob (list (empty? (rest lofd)))))
           (lofd-print (rest lofd) lob))]))


;; (fs-print fs) print a pretty FileSystem (a wrapper for fd-print)
;; fs-print: FileSystem -> Void

(define (fs-print fs)
  (display (fd-print fs empty)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sample-fs
  (make-dir "root" (list 
                    (make-file "readme.txt" 187 5)
                    (make-dir "photos" (list 
                                        (make-file "doctor.jpg" 3669000 6)
                                        (make-file "kirk.jpg" 2866000 7)
                                        (make-file "spock.jpg" 2709000 8)
                                        (make-dir "vacation" (list                    
                                                              (make-file "beach1.jpg" 3297000 9)
                                                              (make-file "beach2.jpg" 2173000 10)
                                                              (make-file "beach3.jpg" 2747000 11)))
                                        (make-file "k9.jpg" 3287000 12)
                                        (make-file "dalek.jpg" 2294000 13)))
                    (make-dir "music" (list 
                                       (make-dir "rock" (list 
                                                         (make-file "rhcp-under-the-bridge.mp3" 10184000 14)
                                                         (make-file "u2-one.mp3" 9693000 15)))
                                       (make-dir "dance" (list 
                                                          (make-file "katy-perry-roar.mp3" 9376000 16)
                                                          (make-file "daft-punk-lose-yourself-to-dance.mp3" 17669000 16)))))
                    (make-dir "schoolwork" empty)
                    (make-dir "notes" (list 
                                       (make-file "shopping.txt" 573 17)
                                       (make-file "todo.txt" 301 18))))))


(define sample-i-tree
  (make-i-node "italian sausage" 50
               (make-i-node "chicken" 100
                            (make-i-node "bacon" 2000
                                         (make-i-node "artichoke hearts" 100 empty empty)
                                         (make-i-node "broccoli" 50 empty empty))
                            (make-i-node "feta cheese" 200 empty empty))
               (make-i-node "pepperoni" 5000
                            (make-i-node "onions" 2000 
                                         (make-i-node "mushrooms" 1500 empty empty)
                                         empty)
                            (make-i-node "tomatoes" 1
                                         (make-i-node "sundried tomatoes" 100 empty empty)
                                         empty))))

