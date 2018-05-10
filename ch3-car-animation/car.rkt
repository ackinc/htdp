;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname car) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; physical constants
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))
(define VELOCITY 3)

; graphical constants
(define BODY (above (rectangle (* 1 WHEEL-DISTANCE) (* 1.5 WHEEL-RADIUS) "solid" "red")
                    (rectangle (* 2 WHEEL-DISTANCE) (* 2 WHEEL-RADIUS) "solid" "red")))

(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define WHEEL-SPACE (rectangle WHEEL-DISTANCE 0 "solid" "white"))
(define WHEELS (beside WHEEL WHEEL-SPACE WHEEL))

(define CAR (above BODY WHEELS))