;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-505) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N -> Boolean
; tests whether n is prime by successively checking if
;   numbers from until (sqrt n) divide n
(check-expect (is-prime 37) #true)
(check-expect (is-prime 38) #false)
(define (is-prime n)
  (local ((define sqrtn (floor (sqrt n)))
           ; N -> Boolean
           ; tests if n is prime
           ; accumulator acc: current number
           (define (is-prime/a m)
             (cond [(zero? (remainder n m)) #false]
                   [(> m sqrtn) #true]
                   [(= m 2) (is-prime/a (add1 m))]
                   [else (is-prime/a (+ m 2))])))
    (is-prime/a 2)))