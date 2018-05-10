;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-103) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct spider [legs space])
(define-struct elephant [space])
(define-struct boa [length girth])
(define-struct armadillo [weight space])

; A ZooAnimal is one of:
; (make-spider legs space)
; (make-elephant space)
; (make-boa length girth)
; (make-armadillo weight space)

; A Cage is a structure (make-cage Number Number Number)
(define-struct cage [length width height])

; Cage -> Number
; returns the volume of the cage
(check-expect (volume (make-cage 1 1 1)) (* 1 1 1))
(check-expect (volume (make-cage 2 2 2)) (* 2 2 2))
(check-expect (volume (make-cage 3 4 5)) (* 3 4 5))
(define (volume cage) (* (cage-length cage)
                         (cage-width cage)
                         (cage-height cage)))

; Boa -> Number
; returns the space needed to transport a boa
;   calculated by treating the boa as a "foldable" cylinder
(check-within (boa-space (make-boa 1 2)) pi 0.01)
(check-within (boa-space (make-boa 5 2)) (* 5 pi) 0.01)
(define (boa-space boa)
  (* pi (sqr (/ (boa-girth boa) 2)) (boa-length boa)))

; ZooAnimal Cage -> Boolean
; returns true if animal will fit in cage, false otherwise
(check-expect (fits (make-spider 4 10) (make-cage 2 2 2)) #false)
(check-expect (fits (make-spider 4 10) (make-cage 2 3 2)) #true)
(check-expect (fits (make-elephant 10) (make-cage 2 2 2)) #false)
(check-expect (fits (make-elephant 10) (make-cage 3 2 2)) #true)
(check-expect (fits (make-boa 10 4) (make-cage 3 5 3)) #false)
(check-expect (fits (make-boa 10 2) (make-cage 3 5 3)) #true)
(check-expect (fits (make-armadillo 40 10) (make-cage 2 2 2)) #false)
(check-expect (fits (make-armadillo 40 10) (make-cage 2 3 2)) #true)
(define (fits animal cage)
  (cond [(spider? animal) (<= (spider-space animal) (volume cage))]
        [(elephant? animal) (<= (elephant-space animal) (volume cage))]
        [(boa? animal) (<= (boa-space animal) (volume cage))]
        [(armadillo? animal) (<= (armadillo-space animal) (volume cage))]
        [else (error "FITS -- unknown animal" animal)]))