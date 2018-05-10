;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-174) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; List-of-Strings -> String
; joins a list of strings into a single string, using supplied separator
(define line (cons "My" (cons "name" (cons "is" (cons "Anirudh" (cons "Nimmagadda." empty))))))
(check-expect (collapse-line empty " ") "")
(check-expect (collapse-line line " ")
              "My name is Anirudh Nimmagadda.")
(define (collapse-line l sep)
  (cond [(empty? l) ""]
        [(empty? (rest l)) (first l)]
        [else (string-append (first l)
                             sep
                             (collapse-line (rest l) sep))]))

; List-of-list-of-strings -> String
; joins an lls into a single string
(define line1 (cons "My" (cons "name" (cons "is" (cons "Anirudh" (cons "Nimmagadda." empty))))))
(define line2 (cons "I" (cons "am" (cons "27" (cons "years" (cons "old." empty))))))
(check-expect (collapse empty) "")
(check-expect (collapse (cons line empty)) "My name is Anirudh Nimmagadda.")
(check-expect (collapse (cons line1 (cons line2 empty)))
              "My name is Anirudh Nimmagadda.\nI am 27 years old.")
(define (collapse lls)
  (cond [(empty? lls) ""]
        [(empty? (rest lls)) (collapse-line (first lls) " ")]
        [else (string-append (collapse-line (first lls) " ")
                             "\n"
                             (collapse (rest lls)))]))

; 1String -> String
; encodes a 1String as a stringified 3-digit number
(check-expect (encode-char "\t") "009")
(check-expect (encode-char "a") "097")
(check-expect (encode-char "z") "122")
(define (encode-char c)
  (cond [(< (string->int c) 10)
         (string-append "00" (number->string (string->int c)))]
        [(< (string->int c) 100)
         (string-append "0" (number->string (string->int c)))]
        [else (number->string (string->int c))]))

; List-of-strings -> List-of-strings
; encodes a list of chars with encode-char
(check-expect (encode-charlist empty) empty)
(check-expect (encode-charlist (explode "\taz"))
              (cons "009" (cons "097" (cons "122" empty))))
(define (encode-charlist l)
  (cond [(empty? l) empty]
        [else (cons (encode-char (first l))
                    (encode-charlist (rest l)))]))

; String -> String
; numerically encodes a word by converting each char
;   to its 3-digit numeric equivalent (see encode-char)
(check-expect (encode-word "") "")
(check-expect (encode-word "\taz") "009097122")
(define (encode-word w)
  (collapse-line (encode-charlist (explode w)) ""))

; List-of-strings -> List-of-strings
; encodes a list of words. analogous to encode-charlist
(check-expect (encode-wordlist empty) empty)
(check-expect (encode-wordlist (cons "\taz" (cons "az\t" empty)))
              (cons "009097122" (cons "097122009" empty)))
(define (encode-wordlist wl)
  (cond [(empty? wl) empty]
        [else (cons (encode-word (first wl))
                    (encode-wordlist (rest wl)))]))

; List-of-list-of-strings -> List-of-list-of-strings
; numerically encodes text parsed by read-words/line
(define (encode lls)
  (cond [(empty? lls) empty]
        [else (cons (encode-wordlist (first lls))
                    (encode (rest lls)))]))

(write-file "ttt.dat" (collapse (encode (read-words/line "ttt.txt"))))