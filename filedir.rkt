;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname filedir) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; November 4th, 2014
;; Assignment 7
;; Problem 2: File Directory
(require "a7.rkt")

(define file1 (make-file "oldfile" 1000 5))
(define file2 (make-file "newfile" 1000 55555555))
(define dir0 (make-dir "empty-dir" empty))
(define dir1 (make-dir "dir-onefile" (list file1)))
(define dir2 (make-dir "dir-twofiles" (list file1 file2)))
(define dir3 (make-dir "dir-threesubdirs" (list dir1 dir1 dir1)))
(define dir4 (make-dir "dir-emptysubdir" (list dir0)))
(define fs1 (make-dir "rootdir" (list file1 dir0 dir1 dir2 dir3 file2)))
(define fs2 (make-dir "u" (list file2 dir0 dir1)))


(define (my-file-fn file)
  (... (file-name file) ...
   ... (file-size file) ...
   ... (file-timestamp file) ...))

(define (my-filedir-fn filedir)
  (cond
    [(file? filedir) ...]
    [else (... (my-dir-fn filedir) ...)]))

(define (my-listof-filedir-fn list)
  (cond
    [(empty? list) ...]
    [else (... (my-filedir-fn (first list))
              (my-listof-filedir-fn (rest list)) ...)]))

(define (my-dir-fn dir)
  (... (dir-name dir) ...
   ... (my-listof-filedir-fn (dir-contents dir)) ...))

;; (filedir-timestamp filedir) produces the largest timestamp found in 
;;   all the files of filedir
;; filedir-timestamp: FileDir -> Nat
;; Examples:
(check-expect (filedir-timestamp dir2) 55555555)
(check-expect (filedir-timestamp file1) 5)

(define (filedir-timestamp filedir)
  (local [(define (largest list bigvalue)
            (cond
              [(empty? list) bigvalue]
              [(or (equal? bigvalue false)
                   (> (first list) bigvalue))
               (largest (rest list) (first list))]
              [else (largest (rest list) bigvalue)]))]
    (cond
      [(and (dir? filedir)
            (empty? (dir-contents filedir)))
       false]
      [(file? filedir) (file-timestamp filedir)]
      [else (largest (cons (filedir-timestamp (first (dir-contents filedir)))
                           (filedir-timestamp (rest (dir-contents filedir))))
                     false)])))
                
;; (bigger-files directory cutoff) produces a list of file names in directory
;;   that are bigger than cutoff
;; bigger-files: Dir Nat -> (listof Str)
;; Examples:
(check-expect (bigger-files dir2 1)
              (list "oldfile" "newfile"))
(check-expect (bigger-files dir2 55555556)
              empty)

(define (bigger-files directory cutoff)
  (cond
    [(empty? (dir-contents directory)) empty]
    [(file? (first (dir-contents directory)))
     (cond
       [(> (file-size (first (dir-contents directory))) cutoff)
        (cons (file-name (first (dir-contents directory)))
              (bigger-files (make-dir (dir-name directory)
                                      (rest (dir-contents directory)))    
                            cutoff))]
       [else (bigger-files (make-dir (dir-name directory)
                                     (rest (dir-contents directory)))
                           cutoff)])]
    [else (append (bigger-files (first (dir-contents directory)) cutoff)
                  (bigger-files (make-dir (dir-name directory)
                                          (rest (dir-contents directory))) 
                                cutoff))]))

;; Tests:
(check-expect (bigger-files dir0 100) empty)
(check-expect (bigger-files sample-fs 3297000)
              (list "doctor.jpg" "rhcp-under-the-bridge.mp3" "u2-one.mp3" 
                    "katy-perry-roar.mp3" "daft-punk-lose-yourself-to-dance.mp3"))

;; (filedir-type directory path) compares the elements of path to follow through
;;   directory to see if the FileDir path leads to is a file or dir, or produces error
;; filedir-type: Dir (listof Str) -> (anyof Sym Str)
;; Examples: 
(check-expect (filedir-type sample-fs
                            (list "photos" "bob"))
              "FileDir not found: bob")
(check-expect (filedir-type sample-fs 
                           (list "photos" "vacation" "beach1.jpg"))
              'file)

(define (filedir-type directory path)
  (local [(define (find-fs directory name)
            (cond
              [(and (file? (first (dir-contents directory)))
                    (string=? (file-name (first (dir-contents directory)))
                              name))
               (first (dir-contents directory))]
              [(and (dir? (first (dir-contents directory)))
                    (string=? (dir-name (first (dir-contents directory)))
                              name))
               (first (dir-contents directory))]
              [else (find-fs (make-dir (dir-name directory)
                                       (rest (dir-contents directory)))
                             name)]))]
    (cond
    [(empty? path)
     (cond
       [(file? directory) 'file]
       [else 'dir])]
    [(and (not (empty? path))
          (file? directory))
     (string-append "Found a File but expecting a Dir: "
                    (file-name directory))]
    [(not (member? (first path) (content-names-list directory)))
     (string-append "FileDir not found: "
                    (first path))]
    [else (filedir-type (find-fs directory (first path)) (rest path))])))

;; Tests:
(check-expect (filedir-type fs2 (list "newfile" "notafile"))
              "Found a File but expecting a Dir: newfile")
(check-expect (filedir-type sample-fs empty)
              'dir)

;; (fs-pathable? directory) produces false if anywhere in directory there exists
;;   any set of contents in which a name is repeated, and true if not
;; fs-pathable?: Dir -> Bool
;; Examples:
(check-expect (fs-pathable? dir2) true)
(check-expect (fs-pathable? dir3) false)

(define (fs-pathable? directory)
  (cond
    [(empty? (dir-contents directory)) true]
    [(file? (first (dir-contents directory)))
     (and (not (member? (file-name (first (dir-contents directory)))
                        (content-names-list (make-dir 
                                             (dir-name directory)
                                             (rest (dir-contents directory))))))
          (fs-pathable? (make-dir 
                         (dir-name directory)
                         (rest (dir-contents directory)))))]
    [(dir? (first (dir-contents directory)))
     (and (not (member? (dir-name (first (dir-contents directory)))
                        (content-names-list (make-dir 
                                             (dir-name directory)
                                             (rest (dir-contents directory))))))
          (fs-pathable? (first (dir-contents directory)))
          (fs-pathable? (make-dir 
                         (dir-name directory)
                         (rest (dir-contents directory)))))]))

;; Tests:
(check-expect (fs-pathable? fs2) true)
(check-expect (fs-pathable? dir4) true)
     
;; (content-names-list directory) produces a list of the names of all of
;;   the contents of directory
;; content-names-list: Dir -> (listof Str)
;; Examples:
(check-expect (content-names-list dir3)
              (list "dir-onefile" "dir-onefile" "dir-onefile"))
(check-expect (content-names-list fs2)
              (list "newfile" "empty-dir" "dir-onefile"))

(define (content-names-list directory)
          (cond
            [(empty? (dir-contents directory)) empty]
            [(file? (first (dir-contents directory)))
             (cons (file-name (first (dir-contents directory)))
                   (content-names-list (make-dir (dir-name directory)
                                                 (rest (dir-contents directory)))))]
            [else (cons (dir-name (first (dir-contents directory)))
                   (content-names-list (make-dir (dir-name directory)
                                                 (rest (dir-contents directory)))))]))

;; Tests:
(check-expect (content-names-list (make-dir "x" empty))
              empty)
     