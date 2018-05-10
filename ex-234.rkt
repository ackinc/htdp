;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-234) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define one-list '("Asia: Heat of the Moment"
                   "U2: One"
                   "The White Stripes: Seven Nation Army"))

; A RankedString is a (list N String)

; List-of-Strings -> List-of-RankedStrings
; ranks strings by their position in supplied list
; earlier position means better rank
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; List-of-Strings -> List-of-RankedStrings
; ranks strings by their position in supplied list
; later position means better rank
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

; List-of-Strings -> ... deeply nested list ...
; returns a list representation of an HTML table
;   that displays given songs by rank
(define (make-ranking los)
  `(table ((width "200"))
          ,@(make-song-rows (ranking los))))

; List-of-RankedStrings -> ... deeply nested list ...
; returns a list of table rows containing song rank and name
(define (make-song-rows lors)
  (cond [(empty? lors) empty]
        [else (cons (make-song-row (first lors))
                    (make-song-rows (rest lors)))]))

; RankedString -> ... deeply nested list ...
; returns a single table row containing given song's name and rank
(define (make-song-row rs)
  `(tr (td ,(first rs)) (td ,(second rs))))

(make-ranking one-list)