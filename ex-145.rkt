;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-145) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A NEList-of-temperatures is one of
; - (cons c '())
; - (cons c NEList-of-temperatures)
; where c is a Number >= -273

; NEList-of-temperatures -> Boolean
; returns true if supplied list is sorted in ascending order
(check-expect (sorted? (cons 1 '())) #true)
(check-expect (sorted? (cons 1 (cons 2 '()))) #true)
(check-expect (sorted? (cons 2 (cons 1 '()))) #false)
(define (sorted? l)
  (cond [(empty? (rest l)) #true]
        [else (if (> (first l) (first (rest l)))
                  #false
                  (sorted? (rest l)))]))