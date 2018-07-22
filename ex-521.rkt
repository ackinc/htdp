;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname section-33.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; physical constants
(define WIDTH 50) ; width of river in px
(define HEIGHT 50) ; height of river in px

(define N-MISSIONARIES 3)
(define N-CANNIBALS 3)

; graphical constants
(define RIVER (rectangle WIDTH HEIGHT "solid" "lightblue"))
(define BANK (rectangle (/ WIDTH 2) HEIGHT "solid" "brown"))

(define BOAT (square (/ WIDTH 2.5) "solid" "yellow"))
(define MISSIONARY (circle (/ WIDTH 10) "solid" "black"))
(define CANNIBAL (circle (/ WIDTH 10) "outline" "black"))

; A 0to1 is an N in [0, 1]
; A 0to3 is an N in [0, 3]

(define-struct pstate [m c b])
; A PuzzleState is a structure: (make-pstate [0to3 0to3 0to1])
; interpretation:
; - m: #missionaries on left side of river
; - c: #cannibals on left side of river
; - b: 0 if on left-side of river, 1 otherwise

; data examples
(define start-state (make-pstate 3 3 0))
(define end-state (make-pstate 0 0 1))
(define mid-state (make-pstate 2 2 1))

; PuzzleState -> Boolean
; checks whether pstate is the final PuzzleState
(define (final? ps)
  (and (zero? (pstate-m ps))
       (zero? (pstate-c ps))))

; PuzzleState -> Image
(define (render-mc ps)
  (local ((define c-left (above-n CANNIBAL (pstate-c ps)))
          (define m-left (above-n MISSIONARY (pstate-m ps)))
          (define c-right (above-n CANNIBAL
                                   (- N-CANNIBALS
                                      (pstate-c ps))))
          (define m-right (above-n MISSIONARY
                                   (- N-MISSIONARIES
                                      (pstate-m ps)))))
    (overlay/align "right" "middle"
                   (beside m-right c-right)
                   (overlay/align "left" "middle"
                                  (beside c-left m-left)
                                  (beside BANK
                                          (place-boat (pstate-b ps))
                                          BANK)))))

; Image N -> Image
; stacks n copies of img vertically
(define (above-n img n)
  (if (zero? n)
      empty-image
      (above img (above-n img (sub1 n)))))

; 0or1 Image -> Image
; draws the boat in the appropriate position on the river
(define (place-boat pos)
  (overlay/align (if (zero? pos) "left" "right")
                 "middle"
                 BOAT
                 RIVER))