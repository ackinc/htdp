;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-422) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N [List-of Any] -> [List-of Any]
; extracts the first n elements of l
(check-expect (take 3 '(1 2)) '(1 2))
(check-expect (take 3 '(1 2 3)) '(1 2 3))
(check-expect (take 3 '(1 2 3 4 5)) '(1 2 3))
(define (take n l)
  (if (or (zero? n) (empty? l))
      empty
      (cons (first l) (take (sub1 n) (rest l)))))

; N [List-of Any] -> [List-of Any]
; returns all but the first n elements from l
(check-expect (drop 3 '(1 2 3 4 5)) '(4 5))
(check-expect (drop 3 '(1 2 3)) '())
(check-expect (drop 3 '(1 2)) '())
(define (drop n l)
  (if (or (zero? n) (empty? l))
      l
      (drop (sub1 n) (rest l))))

; ex 422
; [List-of Any] N+ -> [List-of [List-of Any]]
; splits l into chunks of size n
(check-expect (list->chunks '(1 2 3 4) 2)
              '((1 2) (3 4)))
(check-expect (list->chunks '(1 2 3 4) 3)
              '((1 2 3) (4)))
(check-expect (list->chunks '(1 2 3 4) 4)
              '((1 2 3 4)))
(check-expect (list->chunks '(1 2 3 4) 5)
              '((1 2 3 4)))
(define (list->chunks l n)
  (if (<= (length l) n)
      (list l)
      (cons (take n l) (list->chunks (drop n l) n))))

; [List-of 1String] N+ -> [List-of String]
(check-expect (bundle (explode "abcdefg") 3)
              '("abc" "def" "g"))
(define (bundle l n)
  (map implode (list->chunks l n)))


; ex 423
; String N+ -> [List-of String]
; creates string-chunks of size n from s
(check-expect (partition "anirudh" 3) '("ani" "rud" "h"))
(check-expect (partition "" 3) '(""))
(define (partition s n)
  (if (<= (string-length s) n)
      (list s)
      (cons (substring s 0 n) (partition (substring s n) n))))