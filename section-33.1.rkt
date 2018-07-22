;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname section-33.1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))

; ex 512

; Lam -> Boolean
; tests whether lam is a variable
(check-expect (is-var? 'a) #true)
(check-expect (is-var? ex1) #false)
(define (is-var? lam) (symbol? lam))

; Lam -> Boolean
; tests whether lam is a lambda-expression
(check-expect (is-λ? ex1) #true)
(check-expect (is-λ? ex4) #false)
(define (is-λ? lam) (equal? (first lam) 'λ))

; Lam -> Boolean
; tests whether lam is a lambda-application
(check-expect (is-app? ex1) #false)
(check-expect (is-app? ex4) #true)
(define (is-app? lam) (not (or (is-var? lam) (is-λ? lam))))

; Lam -> Symbol
; extracts the parameter from a lambda-expression
(check-expect (λ-para ex1) 'x)
(check-expect (λ-para ex3) 'y)
(define (λ-para lam) (first (second lam)))

; Lam -> Lam
; extracts the body from a lambda-expression
(check-expect (λ-body ex1) 'x)
(check-expect (λ-body ex3) '(λ (x) y))
(define (λ-body lam) (third lam))

; Lam -> Lam
; extracts the function from an application
(check-expect (app-fun ex4) '(λ (x) (x x)))
(define (app-fun lam) (first lam))

; Lam -> Lam
; extracts the argument from an application
(check-expect (app-arg ex4) '(λ (x) (x x)))
(define (app-arg lam) (second lam))

; Lam -> [List-of Symbol]
; extracts all the symbols used as lambda-parameters in lam
(check-expect (declareds ex1) '(x))
(check-expect (declareds ex3) '(y x))
(check-expect (declareds ex4) '(x x))
(define (declareds lam)
  (cond [(is-var? lam) empty]
        [(is-λ? lam) (append (list (λ-para lam))
                             (declareds (λ-body lam)))]
        [else (append (declareds (app-fun lam))
                      (declareds (app-arg lam)))]))



; ex 513 - commented out because these definitions
;            conflict with those above

;(define-struct λ [para body])
; A λ is a structure: (make-λ Symbol Lam)

;(define-struct app [fun arg])
; An Application is a structure: (make-app Lam Lam)

; A Lam is one of:
; - Symbol
; - (make-lambda para body)
; - (make-app fun arg)

; data examples
;(define ex1-alt (make-λ 'x 'x))
;(define ex2-alt (make-λ 'x 'y))
;(define ex3-alt (make-λ 'y (make-λ 'x 'y)))
;(define ex4-alt (make-app (make-λ 'x (make-app 'x 'x))
;                          (make-λ 'x (make-app 'x 'x))))


; Lam -> Lam
; replaces undeclared variables in lam0 with '*undeclared
(check-expect (undeclared ex1) ex1)
(check-expect (undeclared ex2) '(λ (x) *undeclared))
(check-expect (undeclared ex3) ex3)
(check-expect (undeclared ex4) ex4)
(define (undeclared lam0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds: list of all vars declared
          ;   between lam0 and lam
          (define (undeclared/a lam declareds)
            (cond [(is-var? lam) (if (member? lam declareds)
                                     lam
                                     '*undeclared)]
                  [(is-λ? lam) (local ((define para (λ-para lam))
                                       (define body (λ-body lam)))
                                 (list 'λ
                                       (list para)
                                       (undeclared/a body (cons para declareds))))]
                  [else (list (undeclared/a (app-fun lam) declareds)
                              (undeclared/a (app-arg lam) declareds))])))
    (undeclared/a lam0 empty)))


; ex 514
; an expression in which 'x occurs as both a
;   free and bound variabke
(define ex5 '((λ (y) x) (λ (x) x)))
(check-expect (undeclared ex5) '((λ (y) *undeclared)
                                 (λ (x) x)))


; ex 515
(define ex6 '(λ (x) (x *undeclared)))
(define ex7 `(λ (*undeclared) (,ex6 y)))
(check-expect (undeclared ex7)
              `(λ (*undeclared) (,ex6 *undeclared)))


; ex 515
; Lam -> Lam
; like undeclared, except for two changes:
; - replaces free occurrence of 'x with (list '*undeclared 'x)
; - replaces bound occurrence of 'x with (list '*declared 'x)
(check-expect (undeclared.v2 ex5)
              '((λ (y) (*undeclared x)) (λ (x) (*declared x))))
(define (undeclared.v2 lam0)
  (local ((define (undeclared/a lam declareds)
            (cond [(is-var? lam) (if (member? lam declareds)
                                     (list '*declared lam)
                                     (list '*undeclared lam))]
                  [(is-λ? lam) (local ((define para (λ-para lam))
                                       (define body (λ-body lam)))
                                 (list 'λ
                                       (list para)
                                       (undeclared/a body (cons para declareds))))]
                  [else (list (undeclared/a (app-fun lam) declareds)
                              (undeclared/a (app-arg lam) declareds))])))
    (undeclared/a lam0 empty)))


; ex 517

; Any [List-of Any] -> [N or -1]
; returns the index of item in l, or #false if not found
(check-expect (list-index-of 'a '(a b c)) 0)
(check-expect (list-index-of 'c '(a b c)) 2)
(check-expect (list-index-of 'd '(a b c)) -1)
(define (list-index-of item l0)
  (local ((define (list-index-of/a l n)
            (cond [(empty? l) -1]
                  [(equal? (first l) item) n]
                  [else (list-index-of/a (rest l) (add1 n))])))
    (list-index-of/a l0 0)))

; A Lam-Ext is one of:
; - Lam
; - -1
; - N

; Lam -> Lam-Ext
; replaces variables with a number representing
;   how far away they are from their declaration
(define ex8 '((λ (x) ((λ (y) (y x)) x)) (λ (z) z)))
(check-expect (static-distance ex8)
              '((λ (x) ((λ (y) (0 1)) 0)) (λ (z) 0)))
(define (static-distance lam0)
  (local ((define (static-distance/a lam declareds)
            (cond [(is-var? lam) (list-index-of lam declareds)]
                  [(is-λ? lam) (local ((define para (λ-para lam))
                                       (define body (λ-body lam)))
                                 (list 'λ
                                       (list para)
                                       (static-distance/a body (cons para declareds))))]
                  [else (list (static-distance/a (app-fun lam) declareds)
                              (static-distance/a (app-arg lam) declareds))])))
    (static-distance/a lam0 empty)))