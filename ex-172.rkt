;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-172) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; List-of-Strings -> String
; joins a list of strings into a single string, using " " as separator
(define line (cons "My" (cons "name" (cons "is" (cons "Anirudh" (cons "Nimmagadda." empty))))))
(check-expect (collapse-line empty) "")
(check-expect (collapse-line line)
              "My name is Anirudh Nimmagadda.")
(define (collapse-line l)
  (cond [(empty? l) ""]
        [(empty? (rest l)) (first l)]
        [else (string-append (first l)
                             " "
                             (collapse-line (rest l)))]))

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
        [(empty? (rest lls)) (collapse-line (first lls))]
        [else (string-append (collapse-line (first lls))
                             "\n"
                             (collapse (rest lls)))]))

(write-file "ttt.dat" (collapse (read-words/line "ttt.txt")))