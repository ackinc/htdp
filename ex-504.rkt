;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-504) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Digit is an N in [0, 9]

; [List-of Digit] -> N
; converts a list of digits to a decimal number
(check-expect (to10 '(1 0 2)) 102)
(define (to10 l0)
  (local (; converts l to decimal
          ; accumulator acc: the decimal representation of
          ;   digits in l0, but not l
          (define (to10/a l acc)
            (if (empty? l)
                acc
                (to10/a (rest l) (+ (* 10 acc) (first l))))))
    (to10/a l0 0)))