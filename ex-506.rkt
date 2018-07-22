;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-506) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X -> Y] [List-of X] -> [List-of Y]
; maps f onto l0
(check-expect (my-map even? '(1 2 3 4))
              '(#false #true #false #true))
(define (my-map f l0)
  (local (; [List-of X] [List-of Y] -> [List-of Y]
          ; maps f onto l
          ; accumulator acc: contains values obtained by
          ;   mapping f onto elements in l0, but not l
          (define (my-map/a l acc)
            (if (empty? l)
                acc
                (my-map/a (rest l)
                          (cons (f (first l)) acc)))))
    (reverse (my-map/a l0 empty))))