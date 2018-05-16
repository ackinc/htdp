;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; [List-of X] -> [List-of [List-of N X]]
; returns a list where each element is an element of the
;   original list combined with its index in the original list
(check-expect (enumerate empty) empty)
(check-expect (enumerate '(a)) '((0 a)))
(check-expect (enumerate '(a b)) '((0 a) (1 b)))
(define (enumerate l)
  (for/list ([i (length l)]
             [j l])
    (list i j)))


; [X -> Boolean] [List-of X] -> Boolean
; andmap with for loops
(check-expect (and-map even? '(2 4 6)) 6)
(check-expect (and-map even? '(2 4 5)) #false)
(check-expect (and-map odd? '(2 4 6)) #false)
(define (and-map f l)
  (for/and ([item l]) (if (f item) item #false)))

; [X -> Boolean] [List-of X] -> Boolean
; ormap with for loops
(check-expect (or-map even? '(2 4 6)) 2)
(check-expect (or-map even? '(3 5 6)) 6)
(check-expect (or-map odd? '(2 4 6)) #false)
(define (or-map f l)
  (for/or ([item l]) (if (f item) item #false)))