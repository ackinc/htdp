;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname section-19.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Atom is one of:
; - Number
; - String
; - Symbol

; An S-expr is one of:
; - Atom
; - SL

; An SL is a [List-of S-expr]

; A better data def for S-expr:
; An S-expr is one of:
; - Number
; - String
; - Symbol
; - [List-of S-expr]

; X -> Boolean
; checks if argument is an Atom
(define (atom? x) (or (number? x) (string? x) (symbol? x)))

; ex 317
; S-expr Symbol -> N
; counts the occurrences of sy in sexp
(check-expect (count 20 'hello) 0)
(check-expect (count 'hello 'hello) 1)
(check-expect (count '(hello world) 'hello) 1)
(check-expect (count '(hello world (hello 10 hello)) 'hello)
              3)
(define (count sexp sy)
  (local ((define (count-atom at)
            (if (and (symbol? at) (symbol=? at sy)) 1 0))
          (define (count-sl sl)
            (cond [(empty? sl) 0]
                  [else (+ (count (first sl) sy)
                           (count-sl (rest sl)))])))
    (cond [(atom? sexp) (count-atom sexp)]
          [else (count-sl sexp)])))

; [NEList-of Number] -> Number
; returns the maximum number in l
(define (max-list l)
  (cond [(empty? (rest l)) (first l)]
        [else (max (first l) (max-list (rest l)))]))


; ex 318
; S-expr -> N
; returns the depth of sexp
(check-expect (depth 20) 1)
(check-expect (depth 'hello) 1)
(check-expect (depth '(hello)) 2)
(check-expect (depth '(hello
                       world
                       (hello
                        (hello world)
                        world)
                       (hello hello))) 4)
(define (depth sexp)
  (cond [(atom? sexp) 1]
        [else (add1 (max-list (map depth sexp)))]))


; ex 319
; S-expr Symbol Symbol -> S-expr
; replaces occurrences of old in sexp with new
(check-expect (substitute 'hello 'world 'WORLD) 'hello)
(check-expect (substitute 'hello 'hello 'world) 'world)
(check-expect (substitute '(hello world) 'hello 'world)
              '(world world))
(define (substitute sexp old new)
  (local ((define (substitute-atom at)
            (if (and (symbol? at) (symbol=? at old))
                new
                at)))
    (cond [(atom? sexp) (substitute-atom sexp)]
          [else (map (lambda (s)
                       (substitute s old new))
                     sexp)])))


; [List-of Number] -> Number
; sums the numbers in n
(check-expect (sum-list empty) 0)
(check-expect (sum-list '(1 2 3)) 6)
(define (sum-list l) (foldr + 0 l))


; ex 320
; S-expr Symbol -> N
; counts the occurrences of sy in sexp
(check-expect (count-new 20 'hello) 0)
(check-expect (count-new 'hello 'hello) 1)
(check-expect (count-new '(hello world) 'hello) 1)
(check-expect (count-new '(hello world (hello 10 hello)) 'hello)
              3)
(define (count-new sexp sy)
  (cond [(and (symbol? sexp) (symbol=? sexp sy)) 1]
        [(atom? sexp) 0]
        [else (sum-list (map (lambda (s) (count-new s sy)) sexp))]))