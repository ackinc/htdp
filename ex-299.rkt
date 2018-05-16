;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-299) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Set is a [Number -> Boolean]
(define (mk-set f)
  (lambda (n) (f n)))

; set of even numbers
(define set-even (mk-set even?))

; set of odd numbers
(define set-odd (mk-set odd?))

; set of numbers divisible by ten
(define set-div-10 (mk-set (lambda (n) (zero? (remainder n 10)))))

; Number Set -> Boolean
; checks if an element is a member of a set
(check-expect (member-of? 1 set-even) #false)
(check-expect (member-of? 1 set-odd) #true)
(check-expect (member-of? 80 set-div-10) #true)
(define (member-of? n s) (s n))

; Number Set -> Set
; adds an element to a set
(check-expect (member-of? 1 (add-element 1 set-even)) #true)
(define (add-element n s)
  (mk-set (lambda (p) (or (member-of? p s) (= p n)))))

; Set Set -> Set
; returns the union of two sets
(check-expect (member-of? 100 (union set-odd set-div-10)) #true)
(define (union s1 s2)
  (lambda (n) (or (member-of? n s1) (member-of? n s2))))

; Set Set -> Set
; returns the intersection of two sets
(check-expect (member-of? 2 (intersection set-even set-div-10)) #false)
(define (intersection s1 s2)
  (lambda (n) (and (member-of? n s1) (member-of? n s2))))