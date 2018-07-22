;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-502) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [NEList-of 1String] -> [NEList-of 1String]
; "mirrors" l around its last element
(check-expect (mirror (explode "a")) (explode "a"))
(check-expect (mirror (explode "anir")) (explode "anirina"))
(define (mirror l0)
  (local (; [NEList-of 1String] [List-of 1String] -> [List-of 1String]
          ; reverses all but the last element in l
          ; accumulator acc: contains elements in l0, but not l. reversed.
          (define (mirror/a l acc)
            (cond [(empty? (rest l)) acc]
                  [else (mirror/a (rest l)
                                  (cons (first l) acc))])))
    (append l0 (mirror/a l0 empty))))

; NOTE
; Above solution traverses l0 twice
; Book says mirror can be implemented with just one traversal
; How?

; NOTE
; Book seems to miss that append also traverses
;   the entire list