;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-307) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; String [List-of String] -> [Maybe String]
; retrieves the first name in list that is equal to, or
;   an extension of, the given string
(check-expect (find "anirudh" '("arthas" "anirudh")) "anirudh")
(check-expect (find "anirudh" '("arthas" "anirudhn")) "anirudhn")
(check-expect (find "anirudh" '("arthas" "menethil")) #false)
(define (find s los)
  (for/or ([item los]) (if (prefix? s item) item #false)))


; String String -> Boolean
; checks if s1 is a prefix of s2
(check-expect (prefix? "anirudh" "anirudhn") #true)
(check-expect (prefix? "anirudh" "anirud") #false)
(define (prefix? s1 s2)
  (local ((define s1len (string-length s1)))
    (and (>= (string-length s2) s1len)
         (string=? s1 (substring s2 0 s1len)))))

; N [List-of String] -> Boolean
; checks that no string in given list exceeds a certain length
(check-expect (check-smaller 7 empty) #true)
(check-expect (check-smaller 7 '("anirudh" "nimma")) #true)
(check-expect (check-smaller 7 '("anirudh" "nimmagadda")) #false)
(define (check-smaller n los)
  (for/and ([item los]) (<= (string-length item) n)))