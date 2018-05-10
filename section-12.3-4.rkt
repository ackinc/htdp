;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname section-12.3-4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
;(define DICT-LOCATION "usr/share/dict")
;(define DICT (read-lines DICT-LOCATION))

(define DICT '("a" "cat" "act" "rat" "art" "tar"))


; List-of-Strings -> Boolean
; returns true if given list *only* contains
;   "rat", "art", and "tar"
(check-expect (only-words-from-rat? empty) #false)
(check-expect (only-words-from-rat? (list "rat")) #false)
(check-expect (only-words-from-rat? (list "rat" "art" "tar")) #true)
(check-expect (only-words-from-rat? (list "rat" "art" "tar" "rats")) #false)
(define (only-words-from-rat? l)
  (and (= (length l) 3)
       (member? "art" l)
       (member? "rat" l)
       (member? "tar" l)))


; A Word is one of:
; - empty
; - (cons 1String Word)


; String -> List-of-Strings
; returns all valid permutations of given word
(check-expect (alternative-words "") empty)
(check-expect (alternative-words "a") (list "a"))
(check-member-of (alternative-words "cat")
                 '("cat" "act")
                 '("act" "cat"))
(check-satisfied (alternative-words "rat")
                 only-words-from-rat?)
(define (alternative-words s)
  (in-dictionary (words->strings (permutations (string->word s)))))


; List-of-Strings -> List-of-Strings
; filters out words that cannot be found in the dictionary
(check-expect (in-dictionary '()) '())
(check-expect (in-dictionary '("aniru" "cat")) '("cat"))
(define (in-dictionary low)
  (cond [(empty? low) empty]
        [(member? (first low) DICT)
         (cons (first low) (in-dictionary (rest low)))]
        [else (in-dictionary (rest low))]))


; String -> Word
; returns word representation of given string
(check-expect (string->word "") empty)
(check-expect (string->word "a") '("a"))
(check-expect (string->word "ani") '("a" "n" "i"))
(define (string->word s) (explode s))


; Word -> String
; converts a Word back into a String
(check-expect (word->string empty) "")
(check-expect (word->string '("a")) "a")
(check-expect (word->string '("a" "b" "c")) "abc")
(define (word->string w) (implode w))


; List-of-Words -> List-of-Strings
; converts a list of Words into a list of Strings;
;   using word->string
(check-expect (words->strings empty) empty)
(check-expect (words->strings (list (list "a" "b" "c")))
              (list "abc"))
(check-expect (words->strings (list (list "a" "b" "c")
                                    (list "d" "e" "f")))
              (list "abc" "def"))
(define (words->strings lw)
  (cond [(empty? lw) empty]
        [else (cons (word->string (first lw))
                    (words->strings (rest lw)))]))


; Word -> List-of-Words
; returns all permutations of a word
(check-expect (permutations '()) (list '()))
(check-expect (permutations '("a")) '(("a")))
(check-member-of (permutations '("a" "b"))
                 '(("a" "b") ("b" "a"))
                 '(("b" "a") ("a" "b")))
(define (permutations w)
  (cond [(empty? w) (list '())]
        [else (insert-into-words (first w)
                                 (permutations (rest w)))]))


; List-of-words -> Boolean
; checks that the list only contains words that can be formed
;   by inserting "a" into "bc" and "d"
(check-expect (formed-by-inserting-a-into-bc-d? empty) #false)
(check-expect (formed-by-inserting-a-into-bc-d? (list (list "a"))) #false)
(check-expect (formed-by-inserting-a-into-bc-d? '(("a" "b" "c")
                                                  ("b" "a" "c")
                                                  ("b" "c" "a")
                                                  ("a" "d")
                                                  ("d" "a")))
              #true)
(define (formed-by-inserting-a-into-bc-d? l)
  (and (= (length l) 5)
       (member? '("a" "b" "c") l)
       (member? '("b" "a" "c") l)
       (member? '("b" "c" "a") l)
       (member? '("a" "d") l)
       (member? '("d" "a") l)))


; 1String List-of-Words -> List-of-Words
; returns the words formed by inserting the given character
;   into the "slots" between letters (including before/after all letters)
;   of the words in give list-of-words
(check-expect (insert-into-words "a" empty) empty)
(check-expect (insert-into-words "a" (list empty)) '(("a")))
(check-member-of (insert-into-words "a" '(("b")))
                 '(("a" "b") ("b" "a"))
                 '(("b" "a") ("a" "b")))
(check-satisfied (insert-into-words "a" '(("b" "c") ("d")))
                 formed-by-inserting-a-into-bc-d?)
(define (insert-into-words c low)
  (cond [(empty? low) empty]
        [else (append (insert-into-word c (first low))
                      (insert-into-words c (rest low)))]))


; 1String Word -> List-of-Words
; returns all words formed by inserting c into given word
(check-expect (insert-into-word "a" empty) (list (list "a")))
(check-member-of (insert-into-word "a" (list "b"))
                 '(("b" "a") ("a" "b"))
                 '(("a" "b") ("b" "a")))
(check-member-of (insert-into-word "a" (list "b" "c"))
                 '(("a" "b" "c") ("b" "a" "c") ("b" "c" "a"))
                 '(("a" "b" "c") ("b" "c" "a") ("b" "a" "c"))
                 '(("b" "a" "c") ("a" "b" "c") ("b" "c" "a"))
                 '(("b" "a" "c") ("b" "c" "a") ("a" "b" "c"))
                 '(("b" "c" "a") ("a" "b" "c") ("b" "a" "c"))
                 '(("b" "c" "a") ("b" "a" "c") ("a" "b" "c")))
(define (insert-into-word c w)
  (cond [(empty? w) (list (list c))]
        [else (cons (cons c w)
                    (prefix-words (first w)
                                  (insert-into-word c (rest w))))]))


; 1String List-of-Words -> List-of-Words
; prefixes all words in list with given char
(check-expect (prefix-words "a" empty) empty)
(check-expect (prefix-words "a" (list (list "b" "c")
                                      (list "d")))
              (list (list "a" "b" "c")
                    (list "a" "d")))
(define (prefix-words c low)
  (cond [(empty? low) empty]
        [else (cons (cons c (first low))
                    (prefix-words c (rest low)))]))