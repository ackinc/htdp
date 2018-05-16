;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-310-311) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An NP is a structure: (make-no-parent)
(define-struct no-parent [])
(define NP (make-no-parent))

; A Child is a structure: (make-child FT FT String String N)
(define-struct child [father mother name birthyear eyes])

; An FT is one of:
; - NP
; - Child

; An FF is a [List-of FT]

; Data Examples:
; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT -> N
; counts the number of people in a family tree
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Adam) 3)
(check-expect (count-persons Gustav) 5)
(define (count-persons ft)
  (cond [(no-parent? ft) 0]
        [else (+ 1
                 (count-persons (child-father ft))
                 (count-persons (child-mother ft)))]))

; FT N -> N
; sums the ages of people in a family tree
(check-expect (sum-ages Carl 2000) 74)
(check-expect (sum-ages Adam 2000) 198)
(check-expect (sum-ages Gustav 2000) 229)
(define (sum-ages ft current-year)
  (cond [(no-parent? ft) 0]
        [else (+ (- current-year (child-birthyear ft))
                 (sum-ages (child-father ft) current-year)
                 (sum-ages (child-mother ft) current-year))]))

; FT N -> Number
; calculates the average age of the people in a family tree
(check-expect (avg-age Carl 2000) 74)
(check-expect (avg-age Adam 2000) 198/3)
(check-expect (avg-age Gustav 2000) 229/5)
(define (avg-age ft current-year)
  (local ((define count (count-persons ft)))
    (if (zero? count)
        (error "Cannot calculate average age of an empty family tree!")
        (/ (sum-ages ft current-year) count))))

; [List-of Number] -> Number
; sums numbers in list
(define (sum-list l) (foldr + 0 l))

; FF N -> Number
; calculates the avg age of all the people in the family forest
(check-expect (avg-age-forest (list Adam) 2000) 198/3)
(check-expect (avg-age-forest (list Gustav) 2000) 229/5)
(check-within (avg-age-forest (list Carl Adam Gustav) 2000) 55.66 0.01)
(define (avg-age-forest ff cy)
  (if (empty? ff)
      (error "Cannot calculate avg age for empty forest")
      (/ (sum-list (map (lambda (ft) (sum-ages ft cy)) ff))
         (sum-list (map count-persons ff)))))