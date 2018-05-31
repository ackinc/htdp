;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname section-28.1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define EPSILON 0.001)

(define (line-x x) (+ (* 2 x) 3))
(define (poly-x x) (* (- x 2) (- x 4)))

; ex 455
; [Number -> Number] Number -> Number
; determines the slope of f at r
(check-expect (slope line-x 0) 2)
(check-within (slope poly-x 3) 0 EPSILON)
(define (slope f r)
  (/ (- (f (+ r EPSILON)) (f (- r EPSILON))) (* 2 EPSILON)))

; ex 456
; [Number -> Number] Number -> Number
; determines the root of the tangent to f at r
(check-expect (root-of-tangent poly-x 2) 2)

;f  ==  x^2 - 6x + 8
;f' ==  2x - 6
;at x = 5, slope 4
;passes through (5, 3)
;passes through (0, -17)
;line is y = 4x - 17
;passes through (4.25, 0)
(check-within (root-of-tangent poly-x 5) 4.25 EPSILON)

(check-expect (root-of-tangent poly-x 4) 4)
(define (root-of-tangent f r) (- r (/ (f r) (slope f r))))

(define (check-root-of-poly-x root)
  (or (<= (- 2 EPSILON) root (+ 2 EPSILON))
      (<= (- 4 EPSILON) root (+ 4 EPSILON))))

; [Number -> Number] -> Number
; Finds a root of f using Newton's method of repeated guesses
; Terminates when (f guess) is "close" to 0
; Sometimes fails to terminate:
;   tangent of f at r may be a horizontal line (no root)
;   example: (newton poly-x 3)
(check-within (newton line-x 1) -1.5 EPSILON)
(check-satisfied (newton poly-x 1) check-root-of-poly-x)
(check-satisfied (newton poly-x 5.5) check-root-of-poly-x)
(define (newton f r)
  (if (<= (abs (f r)) EPSILON)
      r
      (newton f (root-of-tangent f r))))


; ex 456
; Number Number -> N
; determines the number of months needed to double amt
;   at a monthly interest rate of r
; termination: will not terminate for r <= 0
(check-expect (double-amt 100 50) 2)
(check-expect (double-amt 100 12) 7)
(check-expect (double-amt 100 6) 12)
(define (double-amt amt r)
  (local ((define multiplier (add1 (/ r 100)))
          (define (helper cur-amt cur-mths)
            (if (>= cur-amt (* 2 amt))
                cur-mths
                (helper (* cur-amt multiplier) (add1 cur-mths)))))
    (helper amt 0)))