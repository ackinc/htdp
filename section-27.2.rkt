;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname section-27.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define epsilon 0.001)

(define (poly x) (* (- x 2) (- x 4)))

; ex 445
; Number [Number -> Number] -> Boolean
; checks that there is a root of f in [x, x + epsilon]
(check-expect (verify-root 2 poly) #true)
(check-expect (verify-root 4 poly) #true)
(check-expect (verify-root 3 poly) #false)
(define (verify-root x f)
  (or (<= (f x) 0 (f (+ x epsilon)))
      (<= (f (+ x epsilon)) 0 (f x))))

; [Number -> Number] Number Number -> Number
; finds R such that (<= (f R) 0 (f (+ R epsilon)))
;   using the Intermediate Value Theorem
; assumptions:
; 1. f is continuous between l and r
; 2. (or (< (f l) 0 (f r)) (< (f r) 0 (f l)))

; ex 446
(check-satisfied (find-root poly 1 3)
                 (lambda (x) (verify-root x poly)))
(check-satisfied (find-root poly 3 5)
                 (lambda (x) (verify-root x poly)))
; ex 447
; this next test works because, luckily, the first
;   average of l and r has (< (f m) 0 (f l))
(check-satisfied (find-root poly 1 6)
                 (lambda (x) (verify-root x poly)))
(check-error (find-root poly 1 15))

(define (find-root f l r)
  (if (<= (- r l) epsilon)
      l
      (local ((define f@l (f l))
              (define f@r (f r))
              (define m (/ (+ l r) 2))
              (define f@m (f m)))
        (cond [(or (<= f@l 0 f@m) (<= f@m 0 f@l))
               (find-root f l m)]
              [(or (<= f@m 0 f@r) (<= f@r 0 f@m))
               (find-root f m r)]))))

; ex 449
(check-satisfied (find-root-alt poly 1 3)
                 (lambda (x) (verify-root x poly)))
(check-satisfied (find-root-alt poly 3 5)
                 (lambda (x) (verify-root x poly)))
(define (find-root-alt f l r)
  (local ((define (helper l r f@l f@r)
            (if (<= (- r l) epsilon)
                l
                (local ((define m (/ (+ l r) 2))
                        (define f@m (f m)))
                  (cond [(or (<= f@l 0 f@m) (<= f@m 0 f@l))
                         (helper l m f@l f@m)]
                        [(or (<= f@m 0 f@r) (<= f@r 0 f@m))
                         (helper m r f@m f@r)])))))
    (helper l r (f l) (f r))))


; ex 450, 451
(define-struct table [VL array])
; A Table is a structure: (make-table N [N -> Number])

; data examples
(define ex-table-1 (make-table 5 (lambda (i) i)))
(define ex-table-2 (make-table 5 (lambda (i) (add1 i))))
(define ex-table-3 (make-table 5 (lambda (i) (sub1 i))))

; Table N -> Number
; returns t's nth element
(check-expect (table-ref ex-table-1 4) 4)
(check-expect (table-ref ex-table-2 4) 5)
(check-expect (table-ref ex-table-3 4) 3)
(define (table-ref t idx)
  (if (>= idx (table-VL t))
      (error "table-ref -- table doesn't have index" idx)
      ((table-array t) idx)))

; Number -> Boolean
; returns true if n is <= epsilon of 0
(define (nearly-zero? n) (<= (abs n) epsilon))

; Table -> [Maybe N]
; returns the index of the first 'root' of table t (which
;   represents a monotonically increasing function)
;   with a linear scan
(check-expect (find-linear ex-table-1) 0)
(check-expect (find-linear ex-table-2) #false)
(check-expect (find-linear ex-table-3) 1)
(define (find-linear t)
  (local ((define (helper idx)
            (cond [(>= idx (table-VL t)) #false]
                  [else (local ((define n (table-ref t idx)))
                          (cond [(nearly-zero? n) idx]
                                [(positive? n) #false]
                                [else (helper (add1 idx))]))])))
    (helper 0)))


; Table -> [Maybe N]
; returns the index of the first 'root' of table t (which
;   represents a monotonically increasing function)
;   with a binary search
(check-expect (find-binary ex-table-1) 0)
(check-expect (find-binary ex-table-2) #false)
(check-expect (find-binary ex-table-3) 1)
(define (find-binary t)
  (local ((; finds a root of t between l and r (inclusive)
           ; terminates when l > r
           define (helper l r)
            (if (> l r)
                #false
                (local ((define m (floor (/ (+ l r) 2)))
                        (define n (table-ref t m)))
                  (cond [(nearly-zero? n) m]
                        [(positive? n) (helper l (sub1 m))]
                        [else (helper (add1 m) r)])))))
    (helper 0 (sub1 (table-VL t)))))