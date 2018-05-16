;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-295) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; distances in terms of pixels 
(define WIDTH 300)
(define HEIGHT 300)
 
; N -> [List-of Posn]
; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))
(define (random-posns n)
  (build-list
    n
    (lambda (i)
      (make-posn (random WIDTH) (random HEIGHT)))))


; Number -> [[List-of Posn] -> Boolean]
; specification for "random-posns"
; returns a predicate that takes a list of posns and checks
;   1. that there are just as many posns are required
;   2. that all posns are within the boundaries WIDTH and HEIGHT
(define (n-inside-playground? n)
  (lambda (l)
    (and (= (length l) n)
         (andmap (lambda (p) (and (< (posn-x p) WIDTH)
                                  (< (posn-y p) HEIGHT))) l))))


; N -> [List-of Posn]
; generates a list of n Posns at WIDTH/2 and HEIGHT/2
(check-satisfied (random-posns/bad 3)
                 (n-inside-playground? 3))
(define (random-posns/bad n)
  (if (zero? n)
      empty
      (cons (make-posn (/ WIDTH 2) (/ HEIGHT 2))
            (random-posns/bad (sub1 n)))))