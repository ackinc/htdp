;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname section-12.1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; location of words file
(define LOCATION "/usr/share/dict/words")

; list of all words in dict
(define AS-LIST (read-lines LOCATION))

; A Letter is a 1String in ["a", "z"]
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; 1String -> Boolean
; returns true if supplied 1String is a letter
(define (letter? c) (member? c LETTERS))

; String -> 1String
; returns the first letter of supplied string
(check-expect (string-first "") "")
(check-expect (string-first "A") "A")
(check-expect (string-first "An") "A")
(define (string-first s)
  (if (zero? (string-length s))
      ""
      (substring s 0 1)))


; ex 195
; Letter Dictionary -> N
; A Dictionary is a list of strings
; A N is a non-negative integer

; counts the number of words in supplied dict that
;   start with the specified 1String
(define (starts-with# letter dict)
  (cond [(empty? dict) 0]
        [else (+ (starts-with# letter (rest dict))
                 (if (string=? letter
                               (string-first (first dict)))
                     1
                     0))]))
;(starts-with# "a" AS-LIST)
;(starts-with# "e" AS-LIST)
;(starts-with# "z" AS-LIST)


; ex 196
; A LetterCount is a Structure (make-lc Letter N)
(define-struct lc [letter count])

; Dictionary -> List-of-letter-counts
; counts the number of words in dict starting with each letter
(define (count-by-letter dict)
  (cond [(empty? dict) empty]
        [(not (letter? (string-first (first dict))))
         (count-by-letter (rest dict))]
        [else (incr-count (string-first (first dict))
                          (count-by-letter (rest dict)))]))

; List-of-letter-counts -> List-of-letter-counts
; adds 1 to the count of the specified letter
(check-expect (incr-count "a" empty) (list (make-lc "a" 1)))
(check-expect (incr-count "a" (list (make-lc "a" 1))) (list (make-lc "a" 2)))
(check-expect (incr-count "a" (list (make-lc "b" 1))) (list (make-lc "b" 1) (make-lc "a" 1)))
(define (incr-count letter llc)
  (cond [(empty? llc) (list (make-lc letter 1))]
        [(string=? (lc-letter (first llc)) letter)
         (cons (make-lc letter (add1 (lc-count (first llc))))
               (rest llc))]
        [else (cons (first llc) (incr-count letter (rest llc)))]))

;(define COUNTS (count-by-letter AS-LIST))
;COUNTS

; List-of-letter-counts -> N
; sums counts for each letter in a list of letter-counts
(define (sum-counts llc)
  (cond [(empty? llc) 0]
        [else (+ (lc-count (first llc))
                 (sum-counts (rest llc)))]))
;(sum-counts COUNTS)


; ex 197
; Dictionary -> LetterCount
; returns the most-frequently appearing starting
;   letter in supplied dictionary
(define (most-frequent dict)
  (most-frequent-letter (count-by-letter dict)))

; NEList-of-letter-counts -> LetterCount
; returns the LetterCount with the highest count in supplied llc
(check-expect (most-frequent-letter (list (make-lc "a" 2))) (make-lc "a" 2))
(check-expect (most-frequent-letter (list (make-lc "a" 2) (make-lc "z" 3))) (make-lc "z" 3))
(define (most-frequent-letter llc)
  ;(most-frequent-letter-helper (first llc) (rest llc)))
  (first (sort-llc llc)))

; LetterCount List-of-letter-counts -> LetterCount
; returns the LetterCount with the highest count among supplied LetterCount and llc
(define (most-frequent-letter-helper default-lc llc)
  (cond [(empty? llc) default-lc]
        [(> (lc-count (first llc)) (lc-count default-lc))
         (most-frequent-letter-helper (first llc) (rest llc))]
        [else (most-frequent-letter-helper default-lc (rest llc))]))

; List-of-letter-counts -> List-of-letter-counts
; sorts list using supplied compare function
(check-expect (sort-llc empty) empty)
(check-expect (sort-llc (list (make-lc "a" 1))) (list (make-lc "a" 1)))
(check-expect (sort-llc (list (make-lc "b" 1) (make-lc "a" 2))) (list (make-lc "a" 2) (make-lc "b" 1)))
(define (sort-llc llc)
  (if (empty? llc)
      empty
      (insert-llc (first llc)
                  (sort-llc (rest llc)))))

; LetterCount List-of-letter-counts -> List-of-letter-counts
; inserts LetterCount at the appropriate position
(check-expect (insert-llc (make-lc "a" 2) empty) (list (make-lc "a" 2)))
(check-expect (insert-llc (make-lc "a" 2) (list (make-lc "b" 4))) (list (make-lc "b" 4) (make-lc "a" 2)))
(check-expect (insert-llc (make-lc "a" 2) (list (make-lc "b" 1))) (list (make-lc "a" 2) (make-lc "b" 1)))
(define (insert-llc item l)
  (cond [(empty? l) (list item)]
        [(lettercount-gte item (first l)) (cons item l)]
        [else (cons (first l) (insert-llc item (rest l)))]))


; ex 198
(define SIMPLEDICT (list "nimmagadda" "anirudh" "surendra" "anuradha" "sneha"))

; Dictionary -> List-of-dictionaries
; splits supplied dictionary into multiple dictionaries based on starting letter
(check-expect (words-by-first-letter empty) empty)
(check-expect (words-by-first-letter SIMPLEDICT)
              (list (list "surendra" "sneha")
                    (list "anirudh" "anuradha")
                    (list "nimmagadda")))
(define (words-by-first-letter dict)
  (cond [(empty? dict) empty]
        [(not (letter? (string-first (first dict))))
         (words-by-first-letter (rest dict))]
        [else (update-dictlist (first dict)
                               (words-by-first-letter (rest dict)))]))

; String List-of-dictionaries -> List-of-dictionaries
; adds specified word to supplied list of dictionaries
(check-expect (update-dictlist "anirudh" empty)
              (list (list "anirudh")))
(check-expect (update-dictlist "balaji"
                               (list (list "surendra" "sneha")
                                     (list "anirudh" "anuradha")
                                     (list "nimmagadda")))
              (list (list "surendra" "sneha")
                    (list "anirudh" "anuradha")
                    (list "nimmagadda")
                    (list "balaji")))
(check-expect (update-dictlist "narendra"
                               (list (list "surendra" "sneha")
                                     (list "anirudh" "anuradha")
                                     (list "nimmagadda")))
              (list (list "surendra" "sneha")
                    (list "anirudh" "anuradha")
                    (list "narendra" "nimmagadda")))
(define (update-dictlist word ld)
  (cond [(empty? ld) (list (list word))]
        [(string=? (string-first word)
                   (string-first (first (first ld))))
         (cons (cons word (first ld)) (rest ld))]
        [else (cons (first ld)
                    (update-dictlist word (rest ld)))]))

; Dictionary -> LetterCount
; returns LetterCount for most-frequently appearing
;   starting letter in supplied dict
(define (most-frequent.v2 dict)
  (make-lc (string-first (first (first (sort-dictionaries (words-by-first-letter dict)))))
           (length (first (sort-dictionaries (words-by-first-letter dict))))))

; List-of-dictionaries -> List-of-dictionaries
; sorts dictionaries in descending order of length
(check-expect (sort-dictionaries (list (list "anirudh") (list "balaji" "baka")))
              (list (list "balaji" "baka") (list "anirudh")))
(define (sort-dictionaries ld)
  (if (empty? ld)
      empty
      (insert-dict (first ld)
                   (sort-dictionaries (rest ld)))))

; Dictionary List-of-dictionaries -> List-of-dictionaries
; inserts dict into appropriate place in list
(define (insert-dict d ld)
  (cond [(empty? ld) (list d)]
        [(length>= d (first ld)) (cons d ld)]
        [else (cons (first ld) (insert-dict d (rest ld)))]))

; List List -> Boolean
; returns true if first list is at least as long as second list
(check-expect (length>= empty (list 1)) #false)
(check-expect (length>= empty empty) #true)
(check-expect (length>= (list 1) empty) #true)
(define (length>= l1 l2) (>= (length l1) (length l2)))

; LetterCount LetterCount -> Boolean
; returns true if first LC has higher count
(check-expect (lettercount-gte (make-lc "a" 5) (make-lc "b" 1)) #true)
(check-expect (lettercount-gte (make-lc "a" 0) (make-lc "b" 1)) #false)
(define (lettercount-gte lc1 lc2) (>= (lc-count lc1) (lc-count lc2)))


(most-frequent AS-LIST)
(most-frequent.v2 AS-LIST)