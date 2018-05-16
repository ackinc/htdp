;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname section-12.8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An FSM-State is a Color

; A Transition is a structure:
;   (make-transition FSM-State FSM-State)
(define-struct transition [current next])

; An FSM is a list of Transitions, i.e., one of:
; - empty
; - (cons Transition FSM)

; interpretation: an FSM is the set of transitions that a
;                 finite-state-machine can undergo in response
;                 to keystrokes

; Examples:
(define fsm-traffic (list (make-transition "red" "yellow")
                          (make-transition "yellow" "green")
                          (make-transition "green" "red")))
; ex 227
(define fsm-bw-machine (list (make-transition "black" "white")
                             (make-transition "white" "black")))


; ex 226
; FSM-State FSM-State -> Boolean
; returns true if the two given states are equal
(check-expect (state=? "red" "red") #true)
(check-expect (state=? "red" "blue") #false)
(define (state=? s1 s2) (string=? s1 s2))