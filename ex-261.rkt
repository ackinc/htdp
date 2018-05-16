;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-261) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Inventory-Record is a structure: (make-ir String Number)
(define-struct ir [name price])

; An Inventory is a [List-of Inventory-Records]

; Inventory -> Inventory
; creates an Inventory from an-inv for all
;   those items that cost less than a dollar
(check-expect (extract1 (list (make-ir "c" 1.5)
                              (make-ir "d" 2)))
              empty)
(check-expect (extract1 (list (make-ir "a" 0.5)
                              (make-ir "b" 1)
                              (make-ir "c" 1.5)
                              (make-ir "d" 2)))
              (list (make-ir "a" 0.5)
                    (make-ir "b" 1)))
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (local ((define extracted (extract1 (rest an-inv))))
       (cond
         [(<= (ir-price (first an-inv)) 1.0)
          (cons (first an-inv) extracted)]
         [else extracted]))]))