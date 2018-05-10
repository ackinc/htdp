;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-47) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define MIN-HAPPINESS 1)
(define MAX-HAPPINESS 5)
(define DELTA-ON-CLOCK-TICK -0.1)
(define DELTA-ON-UP-ARROW 1/5)
(define DELTA-ON-DOWN-ARROW 1/3)

(define SCENE-WIDTH 200)
(define SCENE-HEIGHT 100)
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))

(define GAUGE-COLOR "red")
(define GAUGE-THICKNESS 20)
(define GAUGE-XPOS (/ SCENE-WIDTH 2))
(define GAUGE-YPOS (/ SCENE-HEIGHT 2))

; A WorldState is a Number
; interpretation: the current happiness level

; WorldState -> Image
; displays the current happiness level as a horizontal bar stretching across the scene
; at max happiness, bar reaches from left to right of scene
; at min happiness, bar disappears
(define (render cw)
  (place-image (rectangle (* (/ (- cw MIN-HAPPINESS) (- MAX-HAPPINESS MIN-HAPPINESS))
                             SCENE-WIDTH)
                          GAUGE-THICKNESS
                          "solid"
                          GAUGE-COLOR)
               GAUGE-XPOS
               GAUGE-YPOS
               SCENE))

; WorldState -> WorldState
; changes happiness by DELTA-ON-CLOCK-TICK every clock-tick
; happiness cannot go below MIN-HAPPINESS, or above MAX-HAPPINESS
(check-expect (tock 0) 1)
(check-expect (tock 5) 4.9)
(define (tock cw) (bound (+ cw DELTA-ON-CLOCK-TICK) MIN-HAPPINESS MAX-HAPPINESS))

; WorldState -> WorldState
; changes happiness by appropriate constants when Up/Down arrows are pressed
; happiness cannot go below MIN-HAPPINESS, or above MAX-HAPPINESS
(check-expect (ke-h 1 "q") 1)
(check-expect (ke-h 1 "up") 6/5)
(check-expect (ke-h 1 "down") 4/3)
(define (ke-h cw key)
  (cond [(equal? key "up") (bound (+ cw DELTA-ON-UP-ARROW) MIN-HAPPINESS MAX-HAPPINESS)]
        [(equal? key "down") (bound (+ cw DELTA-ON-DOWN-ARROW) MIN-HAPPINESS MAX-HAPPINESS)]
        [else cw]))

; Number -> Number
; 'truncates' number to supplied min/max limits
(check-expect (bound 23 10 25) 23)
(check-expect (bound 23 25 35) 25)
(check-expect (bound 23 10 15) 15)
(define (bound n minimum maximum)
  (cond [(> n maximum) maximum]
        [(< n minimum) minimum]
        [else n]))

(define (main cw)
  (big-bang cw
            [on-tick tock]
            [on-key ke-h]
            [to-draw render]))
(main MAX-HAPPINESS)