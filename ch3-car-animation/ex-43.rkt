;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-43) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;; constants

;; CAR
; physical constants
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))

; graphical constants
(define BODY (above (rectangle (* 1 WHEEL-DISTANCE) (* 1.5 WHEEL-RADIUS) "solid" "red")
                    (rectangle (* 2 WHEEL-DISTANCE) (* 2 WHEEL-RADIUS) "solid" "red")))

(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define WHEEL-SPACE (rectangle WHEEL-DISTANCE 0 "solid" "white"))
(define WHEELS (beside WHEEL WHEEL-SPACE WHEEL))

(define CAR (above BODY WHEELS))

;; TREE
; physical constants
(define TREE-WIDTH 15)
(define TREE-HEIGHT (* 4 TREE-WIDTH))

; graphical constants
(define TREE-TOP (circle TREE-WIDTH "solid" "green"))
(define TRUNK (rectangle TREE-WIDTH TREE-HEIGHT "solid" "brown"))

(define TREE (overlay/align/offset "middle" "top"
                                   TREE-TOP
                                   0 (/ TREE-WIDTH 2)
                                   TRUNK))

;; SCENE
; physical constants
(define SCENE-WIDTH 200)
(define SCENE-HEIGHT 100)

(define VELOCITY 3)
(define CAR-YPOS (- SCENE-HEIGHT (/ (image-height CAR) 2)))

(define TREE-XPOS (/ SCENE-WIDTH 2))
(define TREE-YPOS (- SCENE-HEIGHT (/ (image-height TREE) 2)))

; graphical constants
(define BACKGROUND (place-image TREE TREE-XPOS TREE-YPOS (empty-scene SCENE-WIDTH SCENE-HEIGHT)))



;;; definitions

; An AnimationState is a Number
; interpretation: # of clock ticks since animation started

; AnimationState -> Number
; returns position of front of car from left margin at time
;   represented by given AnimationState
(define (car-xpos cw) (- (* VELOCITY cw) (/ (image-width CAR) 2)))
(define (car-xpos-sin cw) (+ (- (* VELOCITY cw) (/ (image-width CAR) 2)) (* 5 (sin cw))))

; AnimationState -> Image
; draws car with its front 'cw' pixels from left margin
(define (render cw) (place-image CAR (car-xpos-sin cw) CAR-YPOS BACKGROUND))

; AnimationState -> AnimationState
; increases # clock ticks by 1 for every clock tick
(check-expect (tock 0) 1)
(check-expect (tock 3) 4)
(define (tock cw) (add1 cw))

; AnimationState -> Boolean
; ends animation when car hits the right side of scene
(check-expect (end? 0) #false)
(check-expect (end? (/ SCENE-WIDTH VELOCITY)) #true)
(define (end? cw) (>= cw (/ SCENE-WIDTH VELOCITY)))

(define (main cw)
  (big-bang cw
            [on-tick tock]
            [to-draw render]
            [stop-when end?]))
(main 0)