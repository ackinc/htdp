;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-94) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 300)
(define HEIGHT 200)

(define TANK-VEL 3) ; pixels per clock-tick
(define UFO-VEL 3) ; pixels per clock-tick
(define MISSILE-VEL 6) ; pixels per clock-tick

(define TANK (rectangle 30 10 "solid" "brown"))
(define UFO (above (rectangle 10 5 "solid" "red") (rectangle 40 10 "solid" "green")))
(define MISSILE (triangle 8 "solid" "black"))
(define SUN (underlay (star-polygon 20 7 3 "solid" "yellow") (circle 10 "solid" "yellow")))
(define BACKGROUND (place-image SUN (* WIDTH 4/5) (* HEIGHT 1/5) (empty-scene WIDTH HEIGHT)))
BACKGROUND