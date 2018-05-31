;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo-4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct inex [mantissa sign exponent])
; An inex is a structure: (make-inex N99 S N99)
; representating numbers

; An N99 is an N in [0, 99]
; An S is either 1 or -1

; examples
(define inex-99e-99 (make-inex 99 -1 99))
(define inex-1 (make-inex 1 1 0))
(define inex-2 (make-inex 2 1 0))
(define inex-2-alt (make-inex 20 -1 1))
(define inex-9 (make-inex 9 1 0))
(define inex-99 (make-inex 99 1 0))
(define inex-1200 (make-inex 12 1 2))
(define inex-1300 (make-inex 13 1 2))
(define inex-1e99 (make-inex 1 1 99))

(define MIN-POSITIVE (make-inex 1 -1 99))
(define MAX-POSITIVE (make-inex 99 1 99))

; Number Number Number -> inex
; given a valid mantissa, sign, and exponent, returns an inex
;   else throws an error
(check-expect (create-inex 1 1 1) (make-inex 1 1 1))
(check-expect (create-inex 99 1 99) (make-inex 99 1 99))
(check-error (create-inex 100 1 1))
(check-error (create-inex 1 2 1))
(check-error (create-inex 1 2 100))
(define (create-inex m s e)
  (if (and (integer? m) (<= 0 m 99)
           (or (= s 1) (= s -1))
           (integer? e) (<= 0 e 99))
      (make-inex m s e)
      (error "Invalid input" (list m s e))))

; inex -> Number
; converts inex to a racket number
(check-expect (inex->number inex-1) 1)
(check-expect (inex->number inex-2) 2)
(check-expect (inex->number inex-2-alt) 2)
(check-expect (inex->number inex-1300) 1300)
(define (inex->number i)
  (* (inex-mantissa i)
     (expt 10 (* (inex-sign i)
                 (inex-exponent i)))))

; An inv-inex is an inex with a mantissa/exponent that is
;   an N instead of an N99

; inv-inex -> inex
; converts an inv-inex into a valid inex if possible
; otherwise, throws an error
(check-expect (inv-inex->inex (make-inex 100 -1 2)) (make-inex 10 -1 1))
(check-expect (inv-inex->inex (make-inex 100 -1 1)) (make-inex 10 1 0))
(check-expect (inv-inex->inex (make-inex 100 1 1)) (make-inex 10 1 2))

(check-expect (inv-inex->inex (make-inex 9 1 100)) (make-inex 90 1 99))

(check-error (inv-inex->inex (make-inex 100 1 99)))
(check-error (inv-inex->inex (make-inex 10 1 199)))

(define (inv-inex->inex i)
  (local ((define m (inex-mantissa i))
          (define s (inex-sign i))
          (define e (inex-exponent i)))
    (cond [(> m 99) (if (< (* s e) 99)
                        (inv-inex->inex
                         (make-inex (round (/ m 10))
                                    (if (and (= s -1) (= e 1)) 1 s)
                                    ((if (= s 1) add1 sub1) e)))
                        (error "inv-inex->inex -- cannot convert this inv-inex" i))]
          [(> e 99) (if (> (* 10 m) 99)
                        (error "inv-inex->inex -- cannot convert this inv-inex" i)
                        (inv-inex->inex
                         (make-inex (round (* m 10))
                                    s
                                    (if (= s -1)
                                        (add1 e)
                                        (sub1 e)))))]
          [else i])))

; ex 412
; inex inex -> inex
; adds two inex that have either
; 1. the same exponent
(check-expect (inex+ inex-1 inex-2) (make-inex 3 1 0))
(check-expect (inex+ inex-1 inex-9) (make-inex 10 1 0))
(check-expect (inex+ inex-2 inex-9) (make-inex 11 1 0))
(check-expect (inex+ inex-1 inex-99) (make-inex 10 1 1))
(check-expect (inex+ inex-99 inex-99) (make-inex 20 1 1))
(check-expect (inex+ MIN-POSITIVE MIN-POSITIVE)
              (make-inex 2 -1 99))
(check-expect (inex+ MIN-POSITIVE inex-99e-99)
              (make-inex 10 -1 98))
; 2. exponenents that differ by 1
(check-expect (inex+ (make-inex 3 1 1) (make-inex 11 1 0))
              (make-inex 41 1 0))
(check-expect (inex+ (make-inex 11 1 0) (make-inex 3 1 1))
              (make-inex 41 1 0))
(check-expect (inex+ (make-inex 11 -1 1) (make-inex 3 1 0))
              (make-inex 41 -1 1))
(check-expect (inex+ (make-inex 13 1 45) (make-inex 32 1 46))
              (make-inex 33 1 46))
(check-expect (inex+ (make-inex 10 1 45) (make-inex 99 1 46))
              (make-inex 10 1 47))
; error tests
(check-error (inex+ inex-1 MAX-POSITIVE)) ; exps too diff.
(check-error (inex+ inex-1e99 MAX-POSITIVE)) ; out of range
(define (inex+ i1 i2)
  (local ((define m (inex-mantissa i1))
          (define s (inex-sign i1))
          (define e (inex-exponent i1))
          (define m2 (inex-mantissa i2))
          (define s2 (inex-sign i2))
          (define e2 (inex-exponent i2)))
    (cond [(= (* s e) (* s2 e2)) ; only when s == s2 and e == e2
           (inv-inex->inex (make-inex (+ m m2) s e))]
          [(< (* s e) (* s2 e2)) (inex+ i2 i1)]
          [(= (- (* s e) (* s2 e2)) 1)
           (inex+ (make-inex (* m 10)
                             (if (= e 0) -1 s)
                             ((if (or (= s -1) (= e 0)) add1 sub1) e))
                  i2)]
          [else (error "inex+ -- exponents too different"
                       (list i1 i2))])))

; ex 413
; multiply two inexs
(check-expect (inex* inex-1 inex-2) inex-2)
(check-expect (inex* inex-99e-99 inex-1) inex-99e-99)
(check-expect (inex* inex-99e-99 inex-1e99) (make-inex 99 1 0))
(check-error (inex* inex-2 MAX-POSITIVE))
(check-error (inex* MIN-POSITIVE MIN-POSITIVE))
(define (inex* i1 i2)
  (local ((define exp1 (* (inex-sign i1) (inex-exponent i1)))
          (define exp2 (* (inex-sign i2) (inex-exponent i2)))
          (define exp-sum (+ exp1 exp2))
          (define mant-prod (* (inex-mantissa i1) (inex-mantissa i2))))
    (inv-inex->inex
     (make-inex mant-prod
                (if (negative? exp-sum) -1 1)
                (abs exp-sum)))))