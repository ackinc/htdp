;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-263) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [NEList-of Number] -> Number
; returns the smallest number in the given list
(check-expect (inf '(5)) 5)
(check-expect (inf '(5 9 1)) 1)
(check-expect (inf '(5 9 11)) 5)
(define (inf l)
  (cond [(empty? (rest l)) (first l)]
        [else (local ((define smallest-in-rest (inf (rest l))))
                (if (< (first l) smallest-in-rest)
                    (first l)
                    smallest-in-rest))]))