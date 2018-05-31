;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname section-25.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of X] [X X -> Boolean] -> [List-of X]
; uses the quicksort algorithm to sort a list of items
(check-expect (quick-sort '(3 2 2 1) <) '(1 2 2 3))
(check-expect (quick-sort '(3 2 1 2) >=) '(3 2 2 1))
(define (quick-sort items cmp)
  (if (<= (length items) 1)
      items
      (local ((define pivot (first items))
              (define rest-items (rest items))
              (define left-items (filter (lambda (n)
                                           (cmp n pivot))
                                         rest-items))
              (define right-items (filter (lambda (n)
                                            (not (cmp n pivot)))
                                          rest-items)))
        (append (quick-sort left-items cmp)
                (list pivot)
                (quick-sort right-items cmp)))))