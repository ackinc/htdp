;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname section-28.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]
 
(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))
 
(define S '(1 1 2)) ; a Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e) (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e) (first (reverse e)))

; ex 462
; [List-of Number] Solution -> Number
; plugs-in sol into eqn-lhs
(define (plug-in eqn-lhs sol)
  (foldr + 0 (map * eqn-lhs sol)))

; SOE Solution -> Boolean
; checks if s is a solution of soe
(check-expect (check-solution M S) #true)
(check-expect (check-solution M '(1 2 3)) #false)
(define (check-solution soe s)
  (equal? (map (lambda (eqn-lhs) (plug-in eqn-lhs s))
               (map lhs soe))
          (map rhs soe)))

; ex 465
; [List-of Number] Number -> [List-of Number]
; scales a list of numbers by m
(define (scale m l) (map (lambda (x) (* m x)) l))

; [List-of Number] [List-of Number] -> [List-of Number]
; subtracts the appropriate multiple of the second
;   equation from the first such that first term of result
;   is 0 (first term is then dropped from the result)
(check-expect (subtract '(3 2 1) '(1 1 1)) '(-1 -2))
(check-expect (subtract '(1 1 1) '(3 2 1)) '(1/3 2/3))
(define (subtract e1 e2)
  (local ((define m (/ (first e1) (first e2))))
    (rest (map - e1 (scale m e2)))))


; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix

; ex 466, 467, 468
; SOE -> TM
; triangulates the given SOE

; ex 467
; termination: will not terminate if no eqns in
;   intermediate-soe have leading non-zero terms
;   example: '((1 2 3)
;              (1 2 5)
;              (1 2 7))

; ex 468
; modified definition to signal error for soe where
;   all eqns have leading coeffs = 0

(check-expect (triangulate M) '((2 2 3 10)
                                (3 9 21)
                                (1 2)))
(check-expect (triangulate '((2 3 3 8)
                             (2 3 -2 3)
                             (4 -2 2 4)))
              '((2 3 3 8)
                (-8 -4 -12)
                (-5 -5)))
(check-error (triangulate '((1 2 3)
                            (1 2 4)
                            (1 2 5))))
(define (triangulate soe)
  (cond [(andmap (lambda (eqn) (zero? (first eqn))) soe)
         (error "triangulate -- encountered soe where leading coeffs are 0 for all eqns" soe)]
        [(empty? (rest soe)) soe]
        [(zero? (first (first soe))) (triangulate (rotate soe))]
        [else (local ((define first-eqn (first soe))
                      (define intermediate-soe
                        (map (lambda (eqn)
                               (subtract eqn first-eqn))
                             (rest soe))))
                (cons first-eqn
                      (triangulate intermediate-soe)))]))

; [NEList-of Any] -> [NEList-of Any]
; rotates the given list by sending the first element
;   to the end
(check-expect (rotate '(1)) '(1))
(check-expect (rotate '(1 2 3)) '(2 3 1))
(define (rotate l) (append (rest l) (list (first l))))

; ex 469
; TM -> [List-of Number]
; solves the given triangular system of equations
(check-expect (solve (triangulate M)) S)
(define (solve tsoe)
  (local ((define eqn (first tsoe))
          (define eqn-lhs (lhs eqn))
          (define eqn-rhs (rhs eqn))
          (define coeff (first eqn-lhs))
          (define rest-tsoe (rest tsoe))
          (define rest-soln (if (empty? rest-tsoe)
                                empty
                                (solve rest-tsoe)))
          (define rest-eqn-val (if (empty? rest-tsoe)
                                   0
                                   (plug-in (rest eqn-lhs)
                                            rest-soln))))
    (cons (/ (- eqn-rhs rest-eqn-val) coeff) rest-soln)))

(check-expect (solve-alt (triangulate M)) S)
(check-expect (solve-alt (triangulate '((2 3 3 8)
                                        (2 3 -2 3)
                                        (4 -2 2 4))))
              '(1 1 1))
(define (solve-alt tsoe)
  (foldr (lambda (eqn rest-soln)
           (cons (/ (- (rhs eqn)
                       (plug-in (rest (lhs eqn)) rest-soln))
                    (first eqn))
                 rest-soln))
         empty
         tsoe))

; ex 470
(check-expect (gauss M) S)
(define (gauss soe) (solve (triangulate soe)))