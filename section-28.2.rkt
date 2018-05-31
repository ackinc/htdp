;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname section-28.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define E 0.01)

; [Number -> Number] Number Number -> Number
; gives the area under f between a and b
(check-within (integrate (lambda (x) 10) 0 5) 50 E)
(check-within (integrate (lambda (x) (* 2 x)) 0 10) 100 E)
(check-within (integrate (lambda (x) (* 3 x x)) 0 10) 1000 E)
;(define (integrate f a b) #i0.0)

; [Number -> Number] Number Number -> Number
; gives the area of the trapezoid with corners at:
; - (a, 0)
; - (b, 0)
; - (b, (f b))
; - (a, (f a))
(check-expect (area-of-trapezoid (lambda (x) (* 2 x)) 5 10) 75)
(define (area-of-trapezoid f a b)
  (* 1/2 (- b a) (+ (f b) (f a))))

; ex 458
; kepler integration - calculates area under f between a and b
;   by splitting the area into two trapezoids, and summing
;   their areas
(define (kepler-integrate f a b)
  (local ((define m (/ (+ a b) 2)))
    (+ (area-of-trapezoid f a m) (area-of-trapezoid f m b))))

; ex 459
; another integration method - calculates area under f
;   in [a, b] by dividing the area into N rectangles,
;   and summing up the area of each

; N = 50 for tests to pass if E = 0.1
; N = 159 for tests to pass if E = 0.01
(define N 200)

(define (rect-sum-integrate f a b)
  (local ((define width (/ (- b a) N))
          (define half-width (/ width 2))
          ; determines area of n-th rectangle (whose
          ;   lower left corner will be at
          ;   (a + width * (n - 1), 0)
          (define (area n)
            (* width
               (f (+ a (* width n) half-width)))))
    (foldr + 0 (map area (build-list N (lambda (i) i))))))

; ex 460
; [Number -> Number] Number Number -> Number
; integrates f in [a, b] using a divide & conquer approach
;   that falls back to kepler-integration when the interval
;   is small
(define (integrate-dc f a b)
  (if (< (- b a) E)
      (kepler-integrate f a b)
      (local ((define m (/ (+ a b) 2)))
        (+ (integrate-dc f a m)
           (integrate-dc f m b)))))

; ex 461
; [Number -> Number] Number Number -> Number
; integrates f in [a, b] using an adaptive algorithm
;   that uses kepler integration instead of d&c when
;   f is nearly level in [a, b]
(define (integrate-adaptive f a b)
  (local ((define m (/ (+ a b) 2))
          (define area-full (area-of-trapezoid f a b))
          (define area-left (area-of-trapezoid f a m))
          (define area-right (area-of-trapezoid f m b))
          (define area-sum (+ area-left area-right)))
    (if (< (abs (- area-full area-sum))
           (* E (- b a)))
        area-sum
        (+ (integrate-adaptive f a m)
           (integrate-adaptive f m b)))))