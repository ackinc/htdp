;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-274) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of 1Strings] -> [List-of [List-of 1Strings]]
; returns all prefixes of the supplied list of 1Strings
(check-expect (prefixes empty) (list empty))
(check-expect (prefixes (list "a")) (reverse (list empty (list "a"))))
(check-expect (prefixes (list "a" "b" "c"))
              (reverse (list empty (list "a")
                             (list "a" "b") (list "a" "b" "c"))))
(define (prefixes lo1s)
  (local ((define (get-prefix-of-length n)
            (explode (substring (implode lo1s) 0 n))))
    (reverse (build-list (add1 (length lo1s))
                         get-prefix-of-length))))

; [List-of 1Strings] -> [List-of [List-of 1Strings]]
; returns all suffixes of the supplied list of 1Strings
(check-expect (suffixes empty) (list empty))
(check-expect (suffixes (list "a")) (reverse (list empty (list "a"))))
(check-expect (suffixes (list "a" "b" "c"))
              (reverse (list empty (list "c")
                             (list "b" "c") (list "a" "b" "c"))))
(define (suffixes lo1s)
  (local ((define len (length lo1s))
          (define (get-suffix-of-length n)
            (explode (substring (implode lo1s) (- len n)))))
    (reverse (build-list (add1 len)
                         get-suffix-of-length))))