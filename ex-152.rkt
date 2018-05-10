;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-152) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; An N (natural number) is one of:
; - 0
; - (add1 N)

; Image N -> Image
; produces a column of n copies of img
(define (col img n)
  (cond [(zero? n) (error "COL -- n must be at least 1" n)]
        [(= n 1) img]
        [else (above img (col img (sub1 n)))]))

; Image N -> Image
; produces a row of n copies of img
(define (row img n)
  (cond [(zero? n) (error "ROW -- n must be at least 1" n)]
        [(= n 1) img]
        [else (beside img (row img (sub1 n)))]))


(define BALLOON (circle 4 "solid" "red"))
(define SEAT (square 10 "outline" "black"))
(define HALL (col (row SEAT 8) 18))
(define SCENE (overlay HALL (empty-scene (image-width HALL) (image-height HALL))))

; A Posn is a structure (make-posn Number Number)
;   representing a point in a cartesian plane.
;   x - distance (pixels) from left margin
;   y - distance (pixels) from top margin
; Definition is commented-out because already defined in BSL
;(define-struct posn [x y])

; A List-of-Posn is
; - '()
; - (cons p List-of-Posn)
; where p is a posn

; List-of-Posn -> Image
; draws balloons onto scene
(define (add-balloons l)
  (cond [(empty? l) SCENE]
        [else (place-image BALLOON
                           (posn-x (first l))
                           (posn-y (first l))
                           (add-balloons (rest l)))]))
(add-balloons (cons (make-posn 10 22) (cons (make-posn 50 80) '())))