;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-309) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; [List-of [List-of String]] -> [List-of N]
; counts the number of words in each "line"
(define line0 '("a" "loser" "is" "you!"))
(define line1 '("a" "big" "winner" "is" "you!"))
(define text `(,line0 ,line1))
(check-expect (words-on-line text) '(4 5))
(define (words-on-line text)
  ;(for/list ([line text]) (length line)))

  ; the solution involving pattern-matching seems very contrived
  (match text
    [(cons line rest) (cons (length line) (words-on-line rest))]
    [x empty]))