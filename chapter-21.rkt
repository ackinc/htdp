;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chapter-21) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; ex 345
(define-struct add [left right])
(define-struct mul [left right])

; A BSL-exp (short for "BSL expression") is one of:
; - Number
; - (make-add BSL-exp BSL-exp)
; - (make-mul BSL-exp BSL-exp)

; ex 346
; A BSL-Value is a Number
; interpretation: the class of values that can be the result
;   of evaluating a BSL expr

; ex 347
; BSL-exp -> BSL-Value
; evaluates the given numeric expression
(check-expect (eval-exp 9) 9)
(check-expect (eval-exp (make-add 9 9)) 18)
(check-expect (eval-exp (make-mul 9 9)) 81)
(check-expect (eval-exp (make-add (make-add 1 1)
                                  (make-mul 9 9)))
              83)
(define (eval-exp bsl)
  (cond [(number? bsl) bsl]
        [(add? bsl) (+ (eval-exp (add-left bsl))
                       (eval-exp (add-right bsl)))]
        [(mul? bsl) (* (eval-exp (mul-left bsl))
                       (eval-exp (mul-right bsl)))]
        [else (error "EVAL-EXP -- expression is invalid" bsl)]))



; ex 348
(define-struct AND [left right])
(define-struct OR [left right])
(define-struct NOT [expr])

; A BSL-Bool-exp can be one of:
; - #true
; - #false
; - (make-NOT BSL-Bool-exp)
; - (make-AND BSL-Bool-exp BSL-Bool-exp)
; - (make-OR BSL-Bool-exp BSL-Bool-exp)

; A BSL-Bool-Value is one of:
; - #true
; - #false

; BSL-Bool-exp -> BSL-Bool-Value
; evaluates the given boolean expression
(check-expect (eval-bool-exp #true) #true)
(check-expect (eval-bool-exp #false) #false)
(check-expect (eval-bool-exp (make-NOT #false)) #true)
(check-expect (eval-bool-exp
               (make-NOT (make-AND #true #true)))
              #false)
(check-expect (eval-bool-exp (make-AND #true #true)) #true)
(check-expect (eval-bool-exp (make-OR #false #true)) #true)
(define (eval-bool-exp bsl)
  (cond [(boolean? bsl) bsl]
        [(NOT? bsl) (not (eval-bool-exp (NOT-expr bsl)))]
        [(AND? bsl) (and (eval-bool-exp (AND-left bsl))
                         (eval-bool-exp (AND-right bsl)))]
        [(OR? bsl) (or (eval-bool-exp (OR-left bsl))
                       (eval-bool-exp (OR-right bsl)))]
        [else (error "Invalid BSL-Bool expression" bsl)]))



; Parsing S-exprs

(define WRONG
  "Found an S-expr that cannot be parsed into a BSL-expr")
(define (atom? x) (or (number? x) (string? x) (symbol? x)))

; ex 349
; S-expr -> BSL-expr
(check-expect (parse 9) 9)
(check-expect (parse '(+ 3 5)) (make-add 3 5))
(check-expect (parse '(* 3 5)) (make-mul 3 5))
; (check-error (parse 'a)) ; before introducing variables
(check-expect (parse 'a) 'a)
(check-expect (parse '(f 3)) (make-call 'f 3))
(check-error (parse "a"))
(check-error (parse '(/ 5 3)))
(check-error (parse '(+ 5)))
(check-error (parse '(+ 5 3 1)))
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
 
; SL -> BSL-expr 
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(symbol? (first s))
       (cond
         [(symbol=? (first s) '+)
          (if (= L 3)
              (make-add (parse (second s)) (parse (third s)))
              (error WRONG))]
         [(symbol=? (first s) '*)
          (if (= L 3)
              (make-mul (parse (second s)) (parse (third s)))
              (error WRONG))]
         [(symbol=? (first s) 'define) ; function def
          (if (and (= L 3)
                   (list? (second s))
                   (symbol? (first (second s)))
                   (symbol? (second (second s))))
              (local ((define fname (first (second s)))
                      (define fparam (second (second s)))
                      (define fbody (parse (third s))))
                (make-def fname fparam fbody))
              (error WRONG))]
         [(= L 2) (make-call (first s)
                             (parse (second s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))

; SL -> BSL-da-all
; parses sl into a list of data definitions
(check-expect (parse-da empty) empty)
(check-expect (parse-da '((define x 5))) '((x 5)))
(check-expect (parse-da '((define x 5)
                          (define y 2)))
              '((x 5) (y 2)))
(check-expect (parse-da '((define x 5)
                          (define (f r) (+ r r))))
              (list (list 'x 5)
                    (list 'f (make-def 'f 'r (make-add 'r 'r)))))
(define (parse-da sl)
  (cond [(empty? sl) empty]
        [else (match (first sl)
                [(list 'define x y)
                 (cond [(symbol? x) ; constant definition
                        (cons (list x y) (parse-da (rest sl)))]
                       [(and (list? x) (= (length x) 2)) ; function definition
                        (cons (list (first x) (parse (first sl))) (parse-da (rest sl)))]
                       [else (error WRONG)])]
                [x (error WRONG)])]))
         
     
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) s]))

; S-expr -> BSL-Value
; parses the given S-expr;
; if it is a valid representation of a BSL-expr,
;   produces its value
(check-expect (interpreter-expr '(+ 3 5)) 8)
(check-expect (interpreter-expr '(+ (* 5 6) (* 9 9))) 111)
(check-error (interpreter-expr '(+ 3 5 8)))
(define (interpreter-expr s) (eval-exp (parse s)))

; Adding Variables

; A BSL-var-exp ("BSL expression containing variables")
;   is one of:
; - Number
; - Symbol
; - (make-add BSL-var-exp BSL-var-exp)
; - (make-mul BSL-var-exp BSL-var-exp)

; Data Examples
(define BSL-VAR-EXP-1 0)
(define BSL-VAR-EXP-2 'x)
(define BSL-VAR-EXP-3 (make-add 'x (make-mul 'y 9)))
(define BSL-VAR-EXP-4 (make-add 'x (make-mul 'y 'x)))

; ex 352
; BSL-var-exp Symbol Number -> BSL-var-exp
; replaces occurrences of s in bsl with v
(check-expect (subst 9 'x 3) 9)
(check-expect (subst 'x 'x 3) 3)
(check-expect (subst 'y 'x 3) 'y)
(check-expect (subst (make-add 'x (make-mul 'x 'y)) 'x 3)
              (make-add 3 (make-mul 3 'y)))
(define (subst bsl s v)
  (cond [(number? bsl) bsl]
        [(symbol? bsl) (if (symbol=? bsl s) v bsl)]
        [(add? bsl) (make-add (subst (add-left bsl) s v)
                              (subst (add-right bsl) s v))]
        [(mul? bsl) (make-mul (subst (mul-left bsl) s v)
                              (subst (mul-right bsl) s v))]
        [(call? bsl) (make-call (call-name bsl)
                                (subst (call-arg bsl) s v))]
        [else (error "SUBST -- expression is invalid" bsl)]))

; ex 353
; BSL-var-exp -> Boolean
; checks if bsl is a BSL-exp
(check-expect (numeric? 9) #true)
(check-expect (numeric? 'x) #false)
(check-expect (numeric? (make-add 4 5)) #true)
(check-expect (numeric? (make-add 4 (make-mul 3 'x))) #false)
(define (numeric? bsl)
  (cond [(number? bsl) #true]
        [(symbol? bsl) #false]
        [(add? bsl) (and (numeric? (add-left bsl))
                         (numeric? (add-right bsl)))]
        [(mul? bsl) (and (numeric? (mul-left bsl))
                         (numeric? (mul-right bsl)))]
        [else (error "NUMERIC? - expression is invalid" bsl)]))

; ex 354
; BSL-var-exp -> BSL-Value
; if bsl is a BSL-exp, determine its value
(check-expect (eval-variable 9) 9)
(check-error (eval-variable 'x))
(check-expect (eval-variable (make-mul (make-add 5 4) 9)) 81)
(check-error (eval-variable (make-add 8 (make-mul 'x 9))))
(define (eval-variable bsl)
  (if (numeric? bsl)
      (eval-exp bsl)
      (error "EVAL-VARIABLE -- expression cannot contain a variable" bsl)))

; An AL ("Association List") is a [List-of Association]
; An Association is a list of two items: (list Symbol Number)
; interpretation: mapping of variables to values

; Data Examples:
(define AL1 '((x 3) (y 5)))
(define AL2 '((x 10)))

; ex 354
; BSL-var-exp AL -> BSL-Value
; evaluates bsl using the substitution model
(check-expect (eval-variable* 'x AL1) 3)
(check-expect (eval-variable* 'x AL2) 10)
(check-expect (eval-variable* BSL-VAR-EXP-3 AL1) 48)
(check-error (eval-variable* BSL-VAR-EXP-4 AL2))
(define (eval-variable* ex da)
  ; solution with simultaneous processing
  ;(if (empty? da)
  ;    (eval-variable ex)
  ;    (eval-variable* (subst ex (first (first da))
  ;                               (second (first da)))
  ;                    (rest da))))
  ; solution without simultaneous processing
  (eval-variable (foldr (lambda (assoc exp)
                          (subst exp
                                 (first assoc)
                                 (second assoc)))
                        ex
                        da)))

; ex 355
; BSL-var-exp AL -> BSL-Value
; evaluates ex using the environment model, instead of
;   the substitution model used by eval-variable*
(check-expect (eval-var-lookup* 'x AL1) 3)
(check-expect (eval-var-lookup* 'x AL2) 10)
(check-expect (eval-var-lookup* BSL-VAR-EXP-3 AL1) 48)
(check-error (eval-var-lookup* BSL-VAR-EXP-4 AL2))
(define (eval-var-lookup* ex da)
  (cond [(number? ex) ex]
        [(symbol? ex) (local ((define lookup-result (assq ex da)))
                        (if (false? lookup-result)
                            (error "EVAL-VAR-LOOKUP* -- variable is undefined" ex)
                            (second lookup-result)))]
        [(add? ex) (+ (eval-var-lookup* (add-left ex) da)
                      (eval-var-lookup* (add-right ex) da))]
        [(mul? ex) (* (eval-var-lookup* (mul-left ex) da)
                      (eval-var-lookup* (mul-right ex) da))]
        [else (error "EVAL-VAR-LOOKUP* -- expression is invalid" ex)]))


; ex 356
(define-struct call [name arg])
; A BSL-fun-exp is one of:
; - Number
; - Symbol
; - (make-add BSL-fun-exp BSL-fun-exp)
; - (make-mul BSL-fun-exp BSL-fun-exp)
; - (make-call Symbol BSL-fun-exp)

; Data Examples
(define bsl-fe-1 (make-call 'k (make-add 1 1)))
(define bsl-fe-2 (make-mul 5 bsl-fe-1))
(define bsl-fe-3 (make-mul (make-call 'i 5) bsl-fe-1))

; ex 357
; BSL-fun-exp Symbol Symbol BSL-fun-exp
; determines the value of ex
(define (eval-definition1 ex f x b)
  (cond [(number? ex) ex]
        [(symbol? ex) (error "EVAL-DEFINITION1 -- expression cannot contain a variable" ex)]
        [(add? ex) (+ (eval-definition1 (add-left ex) f x b)
                      (eval-definition1 (add-right ex) f x b))]
        [(mul? ex) (* (eval-definition1 (mul-left ex) f x b)
                      (eval-definition1 (mul-right ex) f x b))]
        [(call? ex) (if (not (symbol=? (call-name ex) f))
                        (error "EVAL-DEFINITION1 -- expression cannot be a call to some other function" (list ex f))
                        (local ((define arg-val (eval-definition1 (call-arg ex) f x b))
                                (define f-body (subst b x arg-val)))
                          (eval-definition1 f-body f x b)))]
        [else (error "EVAL-DEFINITION1 -- expression is invalid" ex)]))

; the following call will never terminate (a generative recursion gotcha)
;(eval-definition1 (make-call 'f 1) 'f 'x (make-call 'f 'x))

; ex 358
; A data-definition for function definitions
; A BSL-fun-def is a struct: (make-def Symbol Symbol BSL-fun-exp)
(define-struct def [name param body])

; Data Examples:
(define f (make-def 'f 'x (make-add 3 'x)))
(define g (make-def 'g 'y (make-call 'f (make-mul 2 'y))))
(define h (make-def 'h 'v (make-add (make-call 'f 'v)
                                    (make-call 'g 'v))))

; A BSL-fun-def* is a [List-of (list Symbol BSL-fun-def)]
(define da-fgh `((f ,f) (g ,g) (h ,h)))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the function specified from the definitions area
(check-error (lookup-def da-fgh 'i))
(check-expect (lookup-def da-fgh 'f) f)
(define (lookup-def da fname)
  (local ((define result (assq fname da)))
    (if (false? result)
        (error "LOOKUP-DEF -- cannot find function" fname)
        (second result))))

; ex 359
; BSL-fun-exp BSL-fun-def* -> BSL-Value
; evaluates ex using the definitions in da
;   throws an error if the evaluation of ex involves
;   calling an undefined function
(check-error (eval-function* (make-call 'i 10) da-fgh))
(check-expect (eval-function* (make-call 'f 10) da-fgh) 13)
(check-expect (eval-function* (make-call 'h 3) da-fgh) 15)
(define (eval-function* ex da)
  (cond [(number? ex) ex]
        [(symbol? ex) (error "EVAL-FUNCTION* -- expression cannot contain a variable" ex)]
        [(add? ex) (+ (eval-function* (add-left ex) da)
                      (eval-function* (add-right ex) da))]
        [(mul? ex) (* (eval-function* (mul-left ex) da)
                      (eval-function* (mul-right ex) da))]
        [(call? ex) (local ((define fn (lookup-def da (call-name ex)))
                            (define arg-val (eval-function* (call-arg ex) da))
                            (define fn-body (subst (def-body fn) (def-param fn) arg-val)))
                      (eval-function* fn-body da))]))


; Evaluating everything

; A data definition for the definitions area
; A BSL-da-all is a [List-of BSL-def]
; A BSL-def is a (list Symbol Value)
; A Value is one of:
; - Number
; - BSL-fun-def
(define area-of-circle
  (make-def 'area-of-circle
            'r
            (make-mul 'close-to-pi (make-mul 'r 'r))))
(define volume-of-10-cylinder
  (make-def 'volume-of-10-cylinder
            'r
            (make-mul 10 (make-call 'area-of-circle 'r))))
(define example-da
  (list (list 'close-to-pi 3.14)
        (list 'area-of-circle area-of-circle)
        (list 'volume-of-10-cylinder volume-of-10-cylinder)))

; BSL-da-all Symbol -> Number
; retrieves a constant from list of data definitions
(check-error (lookup-con-def example-da 'x))
(check-within (lookup-con-def example-da 'close-to-pi) 3.14 0.01)
(define (lookup-con-def da x)
  (local ((define result (assq x da)))
    (cond [(false? result)
           (error "LOOKUP-CON-DEF -- cannot find constant" x)]
          [(not (number? (second result)))
           (error "LOOKUP-CON-DEF -- expected a number, found something else" result)]
          [else (second result)])))

; BSL-da-all Symbol -> BSL-fun-def
; retrieves a function from list of data definitions
(check-error (lookup-fun-def example-da 'x))
(check-expect (lookup-fun-def example-da 'area-of-circle) area-of-circle)
(define (lookup-fun-def da f)
  (local ((define result (assq f da)))
    (cond [(false? result)
           (error "LOOKUP-FUN-DEF -- cannot find function" f)]
          [(not (def? (second result)))
           (error "LOOKUP-FUN-DEF -- expected a function, found something else" result)]
          [else (second result)])))

; BSL-fun-exp BSL-da-all -> BSL-Value
; evaluates ex using the definitions in da
(check-within (eval-all (make-add 'close-to-pi
                                  (make-call 'volume-of-10-cylinder 1))
                        example-da)
              34.54 0.01)
(define (eval-all ex da)
  (cond [(number? ex) ex]
        [(symbol? ex) (lookup-con-def da ex)]
        [(add? ex) (+ (eval-all (add-left ex) da)
                      (eval-all (add-right ex) da))]
        [(mul? ex) (* (eval-all (mul-left ex) da)
                      (eval-all (mul-right ex) da))]
        [(call? ex) (local ((define arg-val (eval-all (call-arg ex) da))
                            (define fn (lookup-fun-def da (call-name ex)))
                            (define fn-body (subst (def-body fn)
                                                   (def-param fn)
                                                   arg-val)))
                      (eval-all fn-body da))]))


; ex 362
; S-expr SL -> BSL-Value
; An interpreter for S-expressions
(check-within (interpreter '(+ close-to-pi
                               (volume-of-10-cylinder x))
                           '((define close-to-pi 3.14)
                             (define x 1)
                             (define (area-of-circle r)
                               (* close-to-pi (* r r)))
                             (define (volume-of-10-cylinder r)
                               (* 10 (area-of-circle r)))))
              34.54 0.01)
(define (interpreter sexp sl) (eval-all (parse sexp)
                                        (parse-da sl)))