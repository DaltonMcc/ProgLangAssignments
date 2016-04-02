#lang racket
;; Programming Languages, Assignment 9 tests
;;
;; This file uses a basic testing mechanism similar to what we did with OCAML.
;; To test, simply run this file in DrRacket.

(require "assignment9sub.rkt")

;; add-nums
(equal? (add-nums (list)) 0) ;;empty list
(equal? (add-nums (list 1 2 'a 3)) 6) ;; non-number
(equal? (add-nums (list 'a 'b 'c)) 0) ;; all non-numbers
(equal? (add-nums (list 2 2 3)) 7)
(equal? (add-nums (list 1 'a 'd 'c 's 1)) 2)


;; length
(equal? (length (list)) 0) ;; empty list
(equal? (length (list 1 2 3 4 5)) 5)
(equal? (length (list 1 2 'a 'b 2)) 5)
(equal? (length (list 'c 3 1 'd)) 4)


;; get-nth
(with-handlers ([exn:fail? (lambda (exn) (equal? (exn-message exn)
                                                 "negative index"))])
    (get-nth null -2))   ;;negative index

(with-handlers ([exn:fail? (lambda (exn) (equal? (exn-message exn)
                                                 "list too short"))])
    (get-nth (list 1 2 3) 4))   ;;out of bounds

(equal? (get-nth (list 'a 'b 'c 'd) 4) 'd)
(equal? (get-nth (list 1 2 3) 2) 2)
(equal? (get-nth (list 1 'a 2 'b 'd 3) 4) 'b)

;; every-other
(equal? (every-other (list 1 2 3 4)) (list 1 3)) ;; even length
(equal? (every-other (list 1 2 3)) (list 1 3))   ;; odd length
(equal? (every-other (list 'a 'b 'c 'd)) (list 'a 'c))
(equal? (every-other (list 'a 2)) (list 'a))
(equal? (every-other (list 1 'a 'b 4 5)) (list 1 'b 5))



;; map
(equal? (map (lambda (x) (* x x)) (list 1 2 3))
     (list 1 4 9))       ;; squaring
(equal? (map (lambda (x) (+ x 1)) (list 1 2 3 4 5))
        (list 2 3 4 5 6))
(equal? (map (lambda (x) (/ x 2)) (list 2 4 6 8))
        (list 1 2 3 4))
(equal? (map (lambda (x) (- x 5)) (list 11 13 15))
        (list 6 8 10))

;; map2
(equal? (map2 (lambda (x y) (* x y)) (list 1 2 3) (list 2 3 4))
     (list 2 6 12))      ;; multiply
(equal? (map2 (lambda (x y) (+ x y)) (list 1 2 3) (list 1 2 3))
        (list 2 4 6))
(equal? (map2 (lambda (x y) (- x y)) (list 5 5 5) (list 2 3 5 6))
        (list 3 2 0)) ;;one list longer than other
(equal? (map2 (lambda (x y) (/ x y)) (list 25 5 10) (list 5 1 2))
        (list 5 5 5))



;; filter
(equal? (filter (lambda (x) (= (modulo x 2) 1))
               (list 1 2 3 4))
     (list 1 3))      ;; odd
(equal? (filter (lambda (x) (= (* x x) 4))
                (list 2 3 4 2 2))
        (list 2 2 2))
(equal? (filter (lambda (x) (= (modulo x 2) 0))
                (list 1 2 3 4 5 6))
        (list 2 4 6))
(equal? (filter (lambda (x) (= (+ x x) 10))
                (list 1 2 3 4 5))
        (list 5))
(equal? (filter (lambda (x) (= (- x 5) 0))
                (list 1 2 3 4))
        (list)) ;; null        

;; call-all
(equal? (call-all (list (lambda () 2)))
     (list 2))        ;; one-element
(equal? (call-all (list (lambda () 1) (lambda () 2) (lambda () 3)))
        (list 1 2 3))
(equal? (call-all (list (lambda () 'a) (lambda () 'b) (lambda () 'c)))
        (list 'a 'b 'c))
(equal? (call-all (list (lambda () "First") (lambda () "Second") (lambda () "Last")))
        (list "First" "Second" "Last"))
