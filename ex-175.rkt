;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-175) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; An N is a non-negative integer

; A WCResult is a structure (make-wcresult N N N)
;   that holds information on the number of lines, words, and characters
;   in a given text
(define-struct wcresult [l w c])
; l -> #lines
; w -> #words
; c -> #chars

; List-of-list-of-strings -> WCResult
; computes the number of lines, words,
;   and characters (incl. spaces and newlines) in
;   the given text
(check-expect (wc empty) (make-wcresult 0 0 0))
(check-expect (wc (cons (cons "Anirudh" empty) empty)) (make-wcresult 1 1 7))
(check-expect (wc (cons (cons "Anirudh" (cons "Nimmagadda" empty))
                        (cons (cons "is" (cons "my" (cons "name." empty)))
                              empty)))
              (make-wcresult 2 5 30))
(define (wc text)
  (make-wcresult (length text)
                 (count-words text)
                 (string-length (collapse text))))

; List-of-list-of-strings -> N
; counts the #word in a given text
(check-expect (count-words empty) 0)
(check-expect (count-words (cons (cons "anirudh" empty) empty)) 1)
(check-expect (count-words (cons (cons "anirudh"
                                       (cons "nimmagadda" empty))
                                 empty)) 2)
(check-expect (count-words (cons (cons "anirudh"
                                       (cons "nimmagadda" empty))
                                 (cons (cons "nimmagadda"
                                             (cons "anirudh" empty))
                                       empty))) 4)
(define (count-words text)
  (if (empty? text)
      0
      (+ (length (first text))
         (count-words (rest text)))))

(wc (read-words/line "ttt.txt"))