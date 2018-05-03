;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname plants) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; John Brockway
;; 20576999
;; October 14th, 2014
;; Assignment 5
;; Problem 3: Plants

(define-struct cityinfo (name zone subzone))
;; A CityInfo is a (make-cityinfo Str Nat Sym)
;; requires: zone range from 0 to 9 inclusive
;;           subzone be one of 'a or 'b

(define-struct plantinfo (name zone subzone))
;; A PlantInfo is a (make-plantinfo Str Nat Sym)
;; requires: zone range from 0 to 9 inclusive
;;           subzone be one of 'a or 'b

;; A PlantInfoList is a (listof PlantInfo)
;; A CityInfoList is a (listof CityInfo)


;; (find-hardy-plants city plant-list) produces a list of which plants in
;;   plant-list would survive in city
;; find-hardy-plants: CityInfo PlantInfoList -> PlantInfoList
;; Examples:
(check-expect (find-hardy-plants (make-cityinfo "Saint John" 5 'a)
                                 (list (make-plantinfo "blue eyed grass" 5 'b)
                                       (make-plantinfo "hosta" 3 'a)
                                       (make-plantinfo "columbine" 4 'a)))
              (list (make-plantinfo "hosta" 3 'a)
                    (make-plantinfo "columbine" 4 'a)))
(check-expect (find-hardy-plants (make-cityinfo "Saint John" 5 'a)
                                 (list (make-plantinfo "hosta" 3 'a)
                                       (make-plantinfo "columbine" 4 'a)))
              (list (make-plantinfo "hosta" 3 'a)
                    (make-plantinfo "columbine" 4 'a)))

(define (find-hardy-plants city plant-list)
  (cond
    [(empty? plant-list) empty]
    [(< (plantinfo-zone (first plant-list)) (cityinfo-zone city))
     (cons (first plant-list) 
           (find-hardy-plants city (rest plant-list)))]
    [(> (plantinfo-zone (first plant-list)) (cityinfo-zone city))
     (find-hardy-plants city (rest plant-list))]
    [(symbol=? (plantinfo-subzone (first plant-list)) 
               (cityinfo-subzone city))
     (cons (first plant-list) 
           (find-hardy-plants city (rest plant-list)))]
    [(and (symbol=? (plantinfo-subzone (first plant-list)) 'a)
          (symbol=? (cityinfo-subzone city) 'b))
     (cons (first plant-list) 
           (find-hardy-plants city (rest plant-list)))]
    [(and (symbol=? (plantinfo-subzone (first plant-list)) 'b)
          (symbol=? (cityinfo-subzone city) 'a))
     (find-hardy-plants city (rest plant-list))]))

;; Tests:
(check-expect (find-hardy-plants (make-cityinfo "Saint John" 5 'a)
                                 empty)
              empty)
(check-expect (find-hardy-plants (make-cityinfo "Saint John" 5 'a)
                                 (list (make-plantinfo "agapanthus" 8 'a)
                                       (make-plantinfo "hosta" 3 'a)))
              (list (make-plantinfo "hosta" 3 'a)))
(check-expect (find-hardy-plants (make-cityinfo "Edmonton" 3 'a)
                                 (list (make-plantinfo "agapanthus" 8 'a)
                                       (make-plantinfo "hosta" 3 'a)))
              (list (make-plantinfo "hosta" 3 'a)))
(check-expect (find-hardy-plants (make-cityinfo "Saint John" 5 'a)
                                 (list (make-plantinfo "blue eyed grass" 5 'b)
                                       (make-plantinfo "hosta" 3 'a)))
              (list (make-plantinfo "hosta" 3 'a)))
(check-expect (find-hardy-plants (make-cityinfo "Vancouver" 8 'b)
                                 (list (make-plantinfo "blue eyed grass" 5 'b)
                                       (make-plantinfo "hosta" 3 'a)
                                       (make-plantinfo "agapanthus" 8 'a)))
              (list (make-plantinfo "blue eyed grass" 5 'b)
                    (make-plantinfo "hosta" 3 'a)
                    (make-plantinfo "agapanthus" 8 'a)))

;; (find-growing-cities plant city-list) produces a list of which cities in
;;   city-list would support plant
;; find-growing-cities: PlantInfo CityInfoList -> (listof Str)
;; Examples:
(check-expect (find-growing-cities (make-plantinfo "blue eyed grass" 5 'b)
                                   (list (make-cityinfo "Vancouver" 8 'b)
                                         (make-cityinfo "Edmonton" 3 'a) 
                                         (make-cityinfo "Waterloo" 5 'b)))
              (list "Vancouver" "Waterloo"))
(check-expect (find-growing-cities (make-plantinfo "agapanthus" 8 'a)
                                   (list (make-cityinfo "Vancouver" 8 'b)
                                         (make-cityinfo "Edmonton" 3 'a) 
                                         (make-cityinfo "Waterloo" 5 'b)))
              (list "Vancouver"))

(define (find-growing-cities plant city-list)
    (cond
    [(empty? city-list) empty]
    [(< (plantinfo-zone plant) (cityinfo-zone (first city-list)))
     (cons (cityinfo-name (first city-list)) 
           (find-growing-cities plant (rest city-list)))]
    [(> (plantinfo-zone plant) (cityinfo-zone (first city-list)))
     (find-growing-cities plant (rest city-list))]
    [(symbol=? (plantinfo-subzone plant) 
               (cityinfo-subzone (first city-list)))
     (cons (cityinfo-name (first city-list))  
           (find-growing-cities plant (rest city-list)))]
    [(and (symbol=? (plantinfo-subzone plant) 'a)
          (symbol=? (cityinfo-subzone (first city-list)) 'b))
     (cons (cityinfo-name (first city-list))  
           (find-growing-cities plant (rest city-list)))]
    [(and (symbol=? (plantinfo-subzone plant) 'b)
          (symbol=? (cityinfo-subzone(first city-list)) 'a))
     (find-growing-cities plant (rest city-list))]))

;; Tests:
(check-expect (find-growing-cities (make-plantinfo "blue eyed grass" 5 'b)
                                   empty)
              empty)
(check-expect (find-growing-cities (make-plantinfo "blue eyed grass" 5 'b)
                                   (list (make-cityinfo "Vancouver" 8 'b)
                                         (make-cityinfo "Edmonton" 3 'a)))
              (list "Vancouver"))
(check-expect (find-growing-cities (make-plantinfo "agapanthus" 8 'a)
                                   (list (make-cityinfo "Edmonton" 3 'a) 
                                         (make-cityinfo "Waterloo" 5 'b)))
              empty)
(check-expect (find-growing-cities (make-plantinfo "blue eyed grass" 5 'b)
                                   (list (make-cityinfo "Vancouver" 8 'b)
                                         (make-cityinfo "Waterloo" 5 'b)))
              (list "Vancouver" "Waterloo"))
(check-expect (find-growing-cities (make-plantinfo "chrysanthemum" 3 'b)
                                   (list (make-cityinfo "Edmonton" 3 'a)
                                         (make-cityinfo "Waterloo" 5 'b)))
              (list "Waterloo"))

;; (find-plantless-cities plants cities) produces a list of which elements of
;;   cities are unable to sustain any elements of plants
;; find-plantless-cities: PlantInfoList CityInfoList -> CityInfoList
;; Examples:
(check-expect (find-plantless-cities (list (make-plantinfo "blue eyed grass" 5 'b)
                                           (make-plantinfo "columbine" 4 'a))
                                     (list (make-cityinfo "Vancouver" 8 'b)
                                           (make-cityinfo "Edmonton" 3 'a) 
                                           (make-cityinfo "Waterloo" 5 'b)))
              (list (make-cityinfo "Edmonton" 3 'a)))
(check-expect (find-plantless-cities (list (make-plantinfo "blue eyed grass" 5 'b)
                                           (make-plantinfo "hosta" 3 'a)
                                           (make-plantinfo "columbine" 4 'a))
                                     (list (make-cityinfo "Vancouver" 8 'b)
                                           (make-cityinfo "Edmonton" 3 'a) 
                                           (make-cityinfo "Waterloo" 5 'b)))
              empty)

(define (find-plantless-cities plants cities)
  (cond
    [(empty? cities) empty]
    [(empty? (find-hardy-plants (first cities) plants))
     (cons (first cities)
           (find-plantless-cities plants (rest cities)))]
    [else (find-plantless-cities plants (rest cities))]))

;; Tests:
(check-expect (find-plantless-cities (list (make-plantinfo "liriope" 7 'b)
                                           (make-plantinfo "columbine" 4 'a))
                                     (list (make-cityinfo "Happy Valley-Goose Bay" 1 'a) 
                                           (make-cityinfo "Waterloo" 5 'b)))
              (list (make-cityinfo "Happy Valley-Goose Bay" 1 'a)))
(check-expect (find-plantless-cities (list (make-plantinfo "hosta" 3 'a)
                                           (make-plantinfo "liriope" 7 'b))
                                     (list (make-cityinfo "Edmonton" 3 'a) 
                                           (make-cityinfo "Waterloo" 5 'b)))
              empty)
