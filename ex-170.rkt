;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-170) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-Posns -> List-of-Posns
; filters given list to only contain "legal" Posns
; i.e x coord in [0, 100] and y coord in [0, 200]
(check-expect (legal empty) empty)
(check-expect (legal (cons (make-posn 200 300) empty)) empty)
(check-expect (legal (cons (make-posn 100 200) empty))
              (cons (make-posn 100 200) empty))
(check-expect (legal (cons (make-posn 0 0)
                           (cons (make-posn 101 200) empty)))
              (cons (make-posn 0 0) empty))
(define (legal lop)
  (cond [(empty? lop) empty]
        [(and (<= 0 (posn-x (first lop)) 100)
              (<= 0 (posn-y (first lop)) 200))
         (cons (first lop) (legal (rest lop)))]
        [else (legal (rest lop))]))