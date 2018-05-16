;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-262) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number -> [List-of [List-of Number]]
; create an identity matrix of specified size
(check-expect (identityM 1) '((1)))
(check-expect (identityM 2) '((1 0)
                              (0 1)))
(check-expect (identityM 3) '((1 0 0)
                              (0 1 0)
                              (0 0 1)))
(define (identityM n)
  (local (; Number Number -> [List-of 0or1]
          ; creates a list of numbers of length j,
          ;   with a 1 in position i, and 0s in the
          ;   other positions
          (define (create-row i j)
            (cond [(zero? j) empty]
                  [else (cons (if (zero? i) 1 0)
                              (create-row (sub1 i) (sub1 j)))]))
          (define (helper k)
            (if (= k n)
                empty
                (cons (create-row k n) (helper (add1 k))))))
    (helper 0)))