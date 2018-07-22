;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-503) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; N -> [List-of N]
; returns a list of n random numbers
(define (build-random-list n)
  (for/list ([i n]) (random 10000)))

; A Matrix is a [List-of [List-of Number]]
; A 0Matrix is a matrix whose rows (except for the last one)
;   all begin with 0

; N -> Matrix
; returns a random mxn matrix
(define (build-matrix m n)
  (for/list ([i m]) (build-random-list n)))

(define ex-matrix-3x3       (build-matrix 3 3))
;(define ex-matrix-1k (build-matrix 1000 1000))
;(define ex-matrix-2k (build-matrix 2000 2000))
;(define ex-matrix-3k (build-matrix 3000 3000))
;(define ex-matrix-4k (build-matrix 4000 4000))
;(define ex-matrix-5k (build-matrix 5000 5000))

; Matrix -> Matrix
(define (convert-matrix-to-0matrix m)
  (append (map (lambda (row) (cons 0 (rest row)))
               (but-last m))
          (list (last m))))

; [NEList-of Any] -> [List-of Any]
; returns all but the last element of a non-empty list
(check-expect (but-last '(1)) '())
(check-expect (but-last '(1 2)) '(1))
(check-expect (but-last '(1 2 3)) '(1 2))
(define (but-last l)
  (if (empty? (rest l))
      empty
      (cons (first l) (but-last (rest l)))))

; [NEList-of Any] -> Any
; returns the last element of a non-empty list
(check-expect (last '(1)) 1)
(check-expect (last '(1 2)) 2)
(define (last l)
  (if (empty? (rest l))
      (first l)
      (last (rest l))))

(define ex-0matrix-3x3 (convert-matrix-to-0matrix ex-matrix-3x3))
;(define ex-0matrix-1k (convert-matrix-to-0matrix ex-matrix-1k))
;(define ex-0matrix-2k (convert-matrix-to-0matrix ex-matrix-2k))
;(define ex-0matrix-3k (convert-matrix-to-0matrix ex-matrix-3k))
;(define ex-0matrix-4k (convert-matrix-to-0matrix ex-matrix-4k))
;(define ex-0matrix-5k (convert-matrix-to-0matrix ex-matrix-5k))

; Matrix -> Matrix
; rotates m until the first row has a non-zero first value
(check-satisfied (rotate ex-0matrix-3x3)
                 (lambda (m) (not (zero? (first (first m))))))
(define (rotate m0)
  (local ((define (rotate/a m seen)
            (cond [(empty? m) (error "rotate -- given matrix has no row with first value non-zero" m0)]
                  [(not (zero? (first (first m)))) (append m (reverse seen))]
                  [else (rotate/a (rest m) (cons (first m) seen))])))
    (rotate/a m0 empty)))