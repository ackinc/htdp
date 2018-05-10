;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-81) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define SECONDS-IN-HOUR 3600)
(define SECONDS-IN-MINUTE 60)

(define-struct time [hours minutes seconds])
; A Time is a structure
; (make-time hours minutes seconds)
; hours -> an integer in [0, 24)
; minutes/seconds -> integers in [0, 60)

; Time -> Number
; returns the number of seconds represented by given time
(check-expect (time->number (make-time 1 0 0)) 3600)
(check-expect (time->number (make-time 1 0 1)) 3601)
(check-expect (time->number (make-time 1 1 1)) 3661)
(define (time->number t)
  (+ (* SECONDS-IN-HOUR (time-hours t)) (* SECONDS-IN-MINUTE (time-minutes t)) (time-seconds t)))