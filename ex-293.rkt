;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-293) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; builds a list of n random numbers < x
(define (build-random-list n x)
  (if (= 0 n)
      empty
      (cons (random x)
            (build-random-list (sub1 n) x))))


; X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(check-expect (find 1 '(2 3)) #false)
(check-expect (find 1 '(1 2 3)) '(1 2 3))
(check-expect (find 1 '(2 1 3)) '(1 3))

(define a-list (build-random-list 500 10000))
(define a-number (random 10000))
(check-satisfied (find a-number a-list) (found? a-number a-list))

(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))


; [List-of X] X -> [NEList-of X -> Boolean]
; creates a function that checks if a given list is the
;   correct output of calling find with x and l

(check-expect ((found? 1 '(1 2 3)) '(2 3)) #false)
(check-expect ((found? 1 '(1 2 3)) '(1 2)) #false)
(check-expect ((found? 1 '(1 2 3)) '(1 2 3)) #true)

(check-expect ((found? 4 '(1 2 3)) '(1 2 3)) #false)
(check-expect ((found? 4 '(1 2 3)) #false) #true)

(define (found? x l)
  (lambda (result)
    (or (and (member? x l)
             (equal? (first result) x)
             (suffix? result l))
        (false? result))))

; [List-of X] [List-of X] -> Boolean
; returns true if l1 is a suffix of l2
(check-expect (suffix? empty empty) #true)
(check-expect (suffix? empty '(1 2 3)) #true)

(check-expect (suffix? '(1) empty) #false)
(check-expect (suffix? '(1) '(1 2 3)) #false)
(check-expect (suffix? '(2 3) '(1 2 3)) #true)

(define (suffix? l1 l2)
  (cond [(empty? l1) #true]
        [(empty? l2) #false]
        [(equal? l1 l2) #true]
        [else (suffix? l1 (rest l2))]))