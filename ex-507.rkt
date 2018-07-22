;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-507) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Any -> Any
; the identity function
(define (identity x) x)

; N [N -> X] -> [List-of X][of length n]
; accumulator-based implementation of build-list
(check-expect (build-l*st 5 sub1) (build-list 5 sub1))
(check-expect (build-l*st 5 identity) (build-list 5 identity))
(check-expect (build-l*st 5 add1) (build-list 5 add1))
(define (build-l*st n f)
  (local ((define (build-l*st/a m acc)
            (if (negative? m)
                acc
                (build-l*st/a (sub1 m) (cons (f m) acc)))))
    (build-l*st/a (sub1 n) empty)))