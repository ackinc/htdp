;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-190) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-1Strings -> List-of-list-of-1Strings
; returns a list of all prefixes of supplied list
(check-expect (prefixes empty) (list empty))
(check-expect (prefixes (list "a")) (reverse (list empty (list "a"))))
(check-expect (prefixes (list "a" "b" "c"))
              (reverse (list empty (list "a")
                             (list "a" "b") (list "a" "b" "c"))))
(define (prefixes l)
  (cond [(empty? l) (list empty)]
        [else (cons l (prefixes (remove-last l)))]))

; list -> list
; returns supplied list with last-elem removed
(check-expect (remove-last empty) empty)
(check-expect (remove-last (list 1)) empty)
(check-expect (remove-last (list 1 2)) (list 1))
(define (remove-last l)
  (cond [(empty? l) empty]
        [(empty? (rest l)) empty]
        [else (cons (first l) (remove-last (rest l)))]))

; List-of-1Strings -> List-of-list-of-1Strings
; returns a list of all suffixes of supplied list
(check-expect (suffixes empty) (list empty))
(check-expect (suffixes (list "a")) (reverse (list empty (list "a"))))
(check-expect (suffixes (list "a" "b" "c"))
              (reverse (list empty (list "c")
                             (list "b" "c") (list "a" "b" "c"))))
(define (suffixes l)
  (if (empty? l)
      (list empty)
      (cons l (suffixes (rest l)))))