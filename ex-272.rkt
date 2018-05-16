;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-272) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Any] [List-of Any] -> [List-of Any]
(check-expect (append-from-fold empty empty) empty)
(check-expect (append-from-fold '(4 5 6) empty) '(4 5 6))
(check-expect (append-from-fold empty '(4 5 6)) '(4 5 6))
(check-expect (append-from-fold '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(define (append-from-fold l1 l2) (foldr cons l2 l1))

; using foldl instead of foldr would append l2 to (reverse l1)