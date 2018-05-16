;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-294) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; builds a list of n random numbers < x
(define (build-random-list n x)
  (if (= 0 n)
      empty
      (cons (random x)
            (build-random-list (sub1 n) x))))


; X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(check-expect (index 4 '(1 2 3)) #false)
(check-expect (index 4 '(4 1 2 3)) 0)
(check-expect (index 4 '(1 2 3 4)) 3)

(define a-list (build-random-list 500 10000))
(define a-number (random 10000))
(check-satisfied (index a-number a-list) (is-index? a-number a-list))

(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))


; X [List-of X] -> [Number -> Boolean]
; A specification for "index"
(check-expect ((is-index? 4 '(1 2 3)) 0) #false)
(check-expect ((is-index? 4 '(1 2 3)) #false) #true)
(check-expect ((is-index? 4 '(1 2 3 4)) 3) #true)
(define (is-index? x l)
  (lambda (result)
    (or (and (member? x l)
             (equal? (list-ref l result) x)
             (not (member? x (prefix l result))))
        (false? result))))


; [List-of X] Number -> [List-of X]
; returns the first n items of a list containing at least n items
(check-expect (prefix '(1 2 3) 0) empty)
(check-expect (prefix '(1 2 3) 1) '(1))
(check-expect (prefix '(1 2 3) 3) '(1 2 3))
(define (prefix l n)
  (if (zero? n) empty (cons (first l) (prefix (rest l) (sub1 n)))))