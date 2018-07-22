;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-498) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> Number
; returns the product of all numbers in list l
(check-expect (product '()) 1)
(check-expect (product '(1 2 3)) 6)
(define (product l0)
  (local (; [List-of Number] Number -> Number
          ; computes the product of numbers in l
          ; accumulator acc: the product in l0, but not in l
          (define (product/a l acc)
            (if (empty? l)
                acc
                (product/a (rest l) (* (first l) acc)))))
    (product/a l0 1)))