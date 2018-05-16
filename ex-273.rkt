;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-273) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X Y] [List-of X] [X -> Y] -> [List-of Y]
; implements map using fold
(check-expect (map1 empty add1) empty)
(check-expect (map1 '(1 2 3) add1) '(2 3 4))
(check-expect (map1 '(1 2 3) sub1) '(0 1 2))
(define (map1 l f)
  (local (; applies f to x, then cons-es the result to y
          (define (g x y) (cons (f x) y)))
    (foldr g empty l)))