;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname section-27.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 1String -> Boolean
; returns true if given 1String is a lowercase letter
(check-expect (string-lowercase? "c") #true)
(check-expect (string-lowercase? "C") #false)
(check-expect (string-lowercase? " ") #false)
(check-expect (string-lowercase? "0") #false)
(define (string-lowercase? c)
  (<= (string->int "a") (string->int c) (string->int "z")))

(define (string-not-lowercase? c) (not (string-lowercase? c)))

; [List-of X] [X -> Boolean] -> [List-of X]
; extracts elements from the beginning of l that satisfy pred
(check-expect (take-until '() string-lowercase?)
              '())
(check-expect (take-until '("A" "B" "c") string-lowercase?)
              '("A" "B"))
(define (take-until l pred)
  (cond [(or (empty? l) (pred (first l))) empty]
        [else (cons (first l) (take-until (rest l) pred))]))

; [List-of X] [X -> Boolean] -> [List-of X]
; drops elements from beginning of l that don't satisfy pred
(check-expect (drop-until '() string-lowercase?) '())
(check-expect (drop-until '("A" "c" "d") string-lowercase?)
              '("c" "d"))
(define (drop-until l pred)
  (cond [(empty? l) empty]
        [(pred (first l)) l]
        [else (drop-until (rest l) pred)]))



; A Line is a [List-of 1String]
; A Token is one of:
; - 1String
; - String[that only contains lowercase letters]

; Line -> [List-of Token]
(check-expect (tokenize empty) empty)
(check-expect (tokenize (list " ")) empty)
(check-expect (tokenize (list "\n")) empty)
(check-expect (tokenize (explode "A")) (list "A"))
(check-expect (tokenize (explode "A9ani")) (list "A" "9" "ani"))
(check-expect (tokenize (explode "anirudh")) (list "anirudh"))
(check-expect (tokenize (explode "anirudh nimmagadda\n"))
              (list "anirudh" "nimmagadda"))
(check-expect (tokenize (explode "Anirudh Nimmagadda"))
              (list "A" "nirudh" "N" "immagadda"))
(define (tokenize line)
  (cond [(empty? line) empty]
        [(string-whitespace? (first line))
         (tokenize (rest line))]
        [(string-not-lowercase? (first line))
         (cons (first line) (tokenize (rest line)))]
        [else (cons (implode
                     (take-until line
                                 string-not-lowercase?))
                    (tokenize
                     (drop-until line
                                 string-not-lowercase?)))]))