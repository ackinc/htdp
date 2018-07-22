;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname section-33.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; ex 525

; side-length at or below which a normal triangle should
;   be drawn, instead of a Sierpinski triangle
(define MINIMUM-SIDE-LENGTH 20)

; Posn Posn -> Number
; computes the euclidean distance between two posns
(check-within (distance (make-posn 0 0) (make-posn 3 4)) 5 0.01)
(check-within (distance (make-posn 3 5) (make-posn 3 4)) 1 0.01)
(check-within (distance (make-posn 1 1) (make-posn 3 4)) (sqrt 13) 0.01)
(define (distance a b)
  (sqrt (+ (sqr (- (posn-x a) (posn-x b)))
           (sqr (- (posn-y a) (posn-y b))))))

; two posns are considered close enough to be the same point
;   if the distance between them is less than:
(define CLOSE-ENOUGH 0.1)

; Posn Posn -> Boolean
; checks if two points are close enough to be the same point
(define (close-enough? a b)
  (<= (distance a b) CLOSE-ENOUGH))

; Image Posn Posn Posn -> Image 
; adds the black triangle a, b, c to scene
(define (add-triangle scene a b c)
  (local ((define s1 (scene+line scene
                                 (posn-x a) (posn-y a)
                                 (posn-x b) (posn-y b)
                                 "black"))
          (define s2 (scene+line s1
                                 (posn-x b) (posn-y b)
                                 (posn-x c) (posn-y c)
                                 "black"))
          (define s3 (scene+line s2
                                 (posn-x c) (posn-y c)
                                 (posn-x a) (posn-y a)
                                 "black")))
    s3))
 
; Posn Posn Posn -> Boolean 
; is the triangle a, b, c too small to be divided
(define (too-small? a b c)
  (<= (distance a b) MINIMUM-SIDE-LENGTH))
 
; Posn Posn -> Posn
; determines the midpoint between a and b
(check-expect (mid-point (make-posn 0 0) (make-posn 2 2))
              (make-posn 1 1))
(define (mid-point a b)
  (make-posn (/ (+ (posn-x a) (posn-x b)) 2)
             (/ (+ (posn-y a) (posn-y b)) 2)))

(define MT (empty-scene 400 400))
(define A (make-posn 200  50))
(define B (make-posn  27 350))
(define C (make-posn 373 350))

; Image Posn Posn Posn -> Image
; draws a Sierpinski triangle with vertices at a, b, and c
(define (add-sierpinski scene a b c)
  (if (too-small? a b c)
      (add-triangle scene a b c)
      (local ((define mid-a-b (mid-point a b))
              (define mid-b-c (mid-point b c))
              (define mid-c-a (mid-point c a))
              (define scene1 (add-sierpinski scene a mid-a-b mid-c-a))
              (define scene2 (add-sierpinski scene1 mid-a-b b mid-b-c))
              (define scene3 (add-sierpinski scene2 mid-c-a mid-b-c c)))
        scene3)))

(add-sierpinski MT A B C)

; ex 526

(define CENTER (make-posn 200 200))
(define RADIUS 200)

; Posn -> [Posn -> Boolean]
(define (check-closeness-to p0)
  (lambda (p) (close-enough? p p0)))

; Number -> Posn
; determines the point on the circle with CENTER and RADIUS
;   whose angle is (* 360 factor)
(check-satisfied (circle-pt 0) (check-closeness-to (make-posn 400 200)))
(check-satisfied (circle-pt 0.25) (check-closeness-to (make-posn 200 0)))
(check-satisfied (circle-pt 0.5) (check-closeness-to (make-posn 0 200)))
(check-satisfied (circle-pt 0.75) (check-closeness-to (make-posn 200 400)))
(check-satisfied (circle-pt 1) (check-closeness-to (make-posn 400 200)))
(define (circle-pt factor)
  (local ((define angle (* 2 pi factor))
          (define x (+ (posn-x CENTER) (* RADIUS (cos angle))))
          (define y (- (posn-y CENTER) (* RADIUS (sin angle)))))
    (make-posn x y)))


; ex 527

(define MINIMUM-TREE-LENGTH 10); in px

; determines how much shorter the base of the recursive
;   savannah growing on the left should be
(define LEFT-LENGTH-FACTOR 0.66)

; determines how much shorter the base of the recursive
;   savannah growing on the right should be
(define RIGHT-LENGTH-FACTOR 0.8)

; determines how much the left-tree is rotated
(define LEFT-ANGLE-FACTOR 0.2)

; determines how much the right-tree is rotated
(define RIGHT-ANGLE-FACTOR -0.3)

; Number -> Posn
(define (number->posn n)
  (if (complex? n)
      (make-posn (real-part n) (imag-part n))
      (make-posn n 0)))

; Posn Posn -> Posn
; "addsubs" two points
(define (posn+- a b)
  (make-posn (+ (posn-x a) (posn-x b))
             (- (posn-y a) (posn-y b))))

; Posn Posn Number -> Posn
; finds the point that is (* factor (distance a b)) along
;   the line from a to b
(define (posn-along a b factor)
  (make-posn (+ (posn-x a)
                (* factor (- (posn-x b) (posn-x a))))
             (+ (posn-y a)
                (* factor (- (posn-y b) (posn-y a))))))

; Image N N N Number -> Image
; adds a fractal savannah tree to the given scene
(define (add-savannah scene x y len angle)
  (local ((define a (make-posn x y))
          (define tmp (number->posn (make-polar len angle)))
          (define b (posn+- a tmp))
          (define scene1 (add-line scene
                                   (posn-x a) (posn-y a)
                                   (posn-x b) (posn-y b)
                                   "red")))
    (if (<= len MINIMUM-TREE-LENGTH)
        scene1
        (local ((define c (posn-along a b 1/3))
                (define d (posn-along a b 2/3))
                (define scene2
                  (add-savannah scene1
                                (posn-x c) (posn-y c)
                                (* LEFT-LENGTH-FACTOR len)
                                (+ LEFT-ANGLE-FACTOR angle)))
                (define scene3
                  (add-savannah scene2
                                (posn-x d) (posn-y d)
                                (* RIGHT-LENGTH-FACTOR len)
                                (+ RIGHT-ANGLE-FACTOR angle))))
          scene3))))

(add-savannah MT 200 400 150 (/ pi 2))


; ex 528 - bezier curves

(define MINIMUM-CURVE-LENGTH 2)

; adds a bezier curve between a and c, using b as the perspective point
(define (add-bezier scene a b c)
  (if (<= (distance a c) MINIMUM-CURVE-LENGTH)
      (add-line scene
                (posn-x a) (posn-y a)
                (posn-x c) (posn-y c)
                "red")
      (local ((define a-b (mid-point a b))
              (define b-c (mid-point b c))
              (define a-b-c (mid-point a-b b-c))
              (define scene1 (add-bezier scene a a-b a-b-c))
              (define scene2 (add-bezier scene1 c b-c a-b-c)))
        scene2)))

(add-bezier MT A (make-posn 0 0) C)