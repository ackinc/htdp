;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch2-ticket-profits) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define DEFAULT-PRICE 5)
(define DEFAULT-ATTENDANCE 120)

(define PRICE-DELTA 0.1)
(define ATTENDANCE-DELTA 15)
(define PRICE-SENSITIVITY (/ ATTENDANCE-DELTA PRICE-DELTA))

(define FIXED-COST 180)
(define VARIABLE-COST 0.04)

(define (attendance ticket-price)
  (- DEFAULT-ATTENDANCE
     (* (- ticket-price DEFAULT-PRICE) PRICE-SENSITIVITY)))

(define (revenue ticket-price) (* ticket-price (attendance ticket-price)))

(define (cost ticket-price)
  (+ FIXED-COST (* VARIABLE-COST (attendance ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price) (cost ticket-price)))

(profit 1)
(profit 2)
(profit 2.5)
(profit 2.8)
(profit 2.9) ; max
(profit 3)
(profit 3.1)
(profit 3.2)
(profit 3.5)
(profit 4)
(profit 5)