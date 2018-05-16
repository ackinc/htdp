;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-270) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N -> [List-of N]
; creates the list (list 0 ... (sub1 n))
;   for any natural number n
(check-expect (ex1 1) (list 0))
(check-expect (ex1 3) (list 0 1 2))
(define (ex1 n) (build-list n identity))

; N -> [List-of N]
; creates the list (list 1 ... n)
;   for any natural number n
(check-expect (ex2 1) (list 1))
(check-expect (ex2 3) (list 1 2 3))
(define (ex2 n) (build-list n add1))

; N -> [List-of Number]
; creates the list (list 1 1/2 1/3 ... 1/n)
;   for any natural number n
(check-expect (ex3 1) (list 1))
(check-expect (ex3 3) (list 1 1/2 1/3))
(define (ex3 n)
  (local ((define (add-then-invert x) (/ 1 (add1 x))))
    (build-list n add-then-invert)))

; N -> [List-of N]
; creates the list of the first n even numbers
(check-expect (ex4 1) (list 0))
(check-expect (ex4 3) (list 0 2 4))
(define (ex4 n)
  (local ((define (double x) (* 2 x)))
    (build-list n double)))

; N -> [List-of [List-of N]]
; creates a diagonal square of 0s and 1s
(check-expect (ex5 1) (list (list 1)))
(check-expect (ex5 2) (list (list 1 0)
                            (list 0 1)))
(check-expect (ex5 3) (list (list 1 0 0)
                            (list 0 1 0)
                            (list 0 0 1)))
(define (ex5 n)
  (local ((define (ret-zero x) 0)
          ; creates a list of length n with a 1 in position i
          ;   and 0s in other positions
          (define (helper i)
            (append (build-list i ret-zero)
                    (list 1)
                    (build-list (- n i 1) ret-zero))))
    (build-list n helper)))

; N [N -> X]-> [List-of X]
; tabulates the results of applying f to [n (- n 1) ... 0]
(check-expect (tabulate 3 sub1) (list 2 1 0 -1))
(check-expect (tabulate 3 add1) (list 4 3 2 1))
(define (tabulate n f)
  (reverse (build-list (add1 n) f)))