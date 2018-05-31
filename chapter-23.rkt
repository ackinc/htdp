;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chapter-23) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct db [schema content])
; A DB is a struct: (make-db Schema Content)

; A Schema is a [List-of Spec]
; A Spec is a (list Label Predicate)
; A Label is a String
; A Predicate is a [Any -> Boolean]

; A Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any

; integrity constraints:
; in any (make-db sch con),
; every row in content has length equal to (length sch)
; ith cell in each row satisfies ith predicate in sch

; data examples:
(define school-schema
  `(("Name"    ,string?)
    ("Age"     ,integer?)
    ("Present" ,boolean?)))
(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))
(define school-db
  (make-db school-schema
           school-content))

(define school-content-2
  `(("Giskard" 45 #true)
    ("Robert"   35 #false)
    ("Carol" 30 #true)
    ("David"  62 #true)))
(define school-db-2
  (make-db school-schema
           school-content-2))
	
(define presence-schema
  `(("Present"     ,boolean?)
    ("Description" ,string?)))
(define presence-content
  `((#true  "presence")
    (#false "absence")))
(define presence-db
  (make-db presence-schema
           presence-content))

(define presence-schema-2
  `(("Present"     ,boolean?)
    ("Description" ,string?)
    ("Code" ,integer?)))
(define presence-content-2
  `((#true  "presence" 1)
    (#false "absence" 0)))
(define presence-db-2
  (make-db presence-schema-2
           presence-content-2))

(define presence-schema-3
  `(("Present"     ,boolean?)
    ("Description" ,string?)
    ("Code" ,integer?)))
(define presence-content-3
  `((#true  "presence" 1)
    (#true  "in" 1)
    (#false "absence" 0)
    (#false "out" 0)))
(define presence-db-3
  (make-db presence-schema-3
           presence-content-3))

(define bad-db (make-db school-schema presence-content))

; DB -> Boolean
; checks the integrity of the given DB
(check-expect (integrity-check school-db) #true)
(check-expect (integrity-check presence-db) #true)
(check-expect (integrity-check bad-db) #false)
(define (integrity-check db)
  (local ((define schema (db-schema db))
          (define width (length schema))
          (define (row-integrity-check row)
            (and (= (length row) width)
                 (andmap (lambda (spec cell)
                           ((second spec) cell))
                         schema
                         row))))
  (andmap row-integrity-check (db-content db))))

; ex 405, 406, 407
; Projections
; DB [List-of Label] -> DB
; returns only those "columns" in db whose labels are in ll
(check-expect (db-content (project school-db
                                   '("Name" "Present")))
              `(("Alice" #true)
                ("Bob"   #false)
                ("Carol" #true)
                ("Dave"  #false)))
(define (project db ll)
  (local ((define schema (db-schema db))
          (define content (db-content db))

          ; Spec -> Boolean
          ; returns true if column should be included
          ;   in projection
          (define (keep? spec) (member? (first spec) ll))

          (define col-select-flags (map keep? schema))

          ; Row -> Row
          ; retains only those cells of row that should be
          ;   included in projection
          (define (row-project row)
            (foldr (lambda (cell col-select-flag proj-row)
                     (if col-select-flag
                         (cons cell proj-row)
                         proj-row))
                   empty
                   row
                   col-select-flags)))

    (make-db (filter keep? schema)
             (map row-project content))))


; ex 408
; Selection
; DB [List-of Label] [Row -> Boolean] -> [List-of Row]
; returns rows that satify the given predicate,
;   projected down to cols specified in ll
(check-expect (select school-db
                      '("Name" "Age")
                      (lambda (row) (< (second row) 32)))
              '(("Bob" 25)
                ("Carol" 30)))
(check-expect (select school-db
                      '("Name" "Age" "Present")
                      (lambda (row)
                        (string=? (first row) "Carol")))
              '(("Carol" 30 #true)))
(define (select db ll p)
  (db-content (project (make-db (db-schema db)
                                (filter p (db-content db)))
                       ll)))

; ex 409
; [List-of Any] Any -> [List-of Any]
; returns the sublist starting with x, if x is in l
(check-expect (list-search 'a '(a b c d)) '(a b c d))
(check-expect (list-search 'c '(a b c d)) '(c d))
(check-expect (list-search 'e '(a b c d)) '())
(define (list-search x l)
  (cond [(empty? l) empty]
        [(equal? x (first l)) l]
        [else (list-search x (rest l))]))

; [List-of Any] Any -> [Maybe N]
; returns the index of x in l, or #false if not found
(check-expect (list-index-of 'a '(a b c d)) 0)
(check-expect (list-index-of 'd '(a b c d)) 3)
(check-expect (list-index-of 'e '(a b c d)) #false)
(define (list-index-of x l)
  (local ((define tmp (list-search x l)))
    (if (empty? tmp)
        #false
        (- (length l) (length tmp)))))

; [List-of Any] [List-of N] -> [List-of Any]
; selects elements of items that are at specified indices
; invalid indices are ignored
(check-expect (select-indices '(1 2 3 4) '(3 0 3 8 1 2))
              '(4 1 4 2 3))
(define (select-indices items indices)
  (local ((define max-index (sub1 (length items))))
    (foldr (lambda (index result)
             (if (<= 0 index max-index)
                 (cons (list-ref items index) result)
                 result))
           empty
           indices)))

; DB [List-of Label] -> DB
; reorders columns of db according to lol
;   ignores labels in lol that are not in db's schema
;   drops columns of db whose labels are not in lol
(check-expect (db-content (reorder
                           school-db
                           '("Name" "Present" "Age")))
              '(("Alice" #true  35)
                ("Bob"   #false 25)
                ("Carol" #true  30)
                ("Dave"  #false 32)))
(check-expect (db-content (reorder
                           school-db
                           '("Age" "Sex" "Name")))
              '((35 "Alice")
                (25 "Bob")
                (30 "Carol")
                (32 "Dave")))
(define (reorder db lol)
  (local ((define schema (db-schema db))
          (define content (db-content db))

          (define colnames (map first schema))
          (define col-indices
            (filter number?
                    (map (lambda (label)
                           (list-index-of label colnames))
                         lol))))
    (make-db (select-indices schema col-indices)
             (map (lambda (row)
                    (select-indices row col-indices))
                  content))))

; ex 410
; returns the union of two databases that have the same schema
; drops duplicate rows
(check-expect (db-union school-db
                        school-db-2)
              (make-db school-schema
                       '(("Alice" 35 #true)
                         ("Bob"   25 #false)
                         ("Dave"  32 #false)
                         ("Giskard" 45 #true)
                         ("Robert"   35 #false)
                         ("Carol" 30 #true)
                         ("David"  62 #true))))
(check-error (db-union school-db presence-db))
(define (db-union db1 db2)
  (local ((define schema (db-schema db1)))
    (if (not (equal? (map first schema)
                     (map first (db-schema db2))))
        (error "db-union -- schemas must be identical"
               (list db1 db2))
        (make-db schema
                 (foldr (lambda (row result)
                          (if (member? row result)
                              result
                              (cons row result)))
                        (db-content db2)
                        (db-content db1))))))

; ex 411
; joins two databases by "translating" the last cell of
;   each row in the first into its corresponding rows in
;   the second database
(check-expect (db-content (join school-db presence-db))
              '(("Alice" 35 "presence")
                ("Bob" 25 "absence")
                ("Carol" 30 "presence")
                ("Dave" 32 "absence")))
(check-expect (db-content (join school-db presence-db-2))
              '(("Alice" 35 "presence" 1)
                ("Bob" 25 "absence" 0)
                ("Carol" 30 "presence" 1)
                ("Dave" 32 "absence" 0)))
(check-expect (db-content (join school-db presence-db-3))
              '(("Alice" 35 "presence" 1)
                ("Alice" 35 "in" 1)
                ("Bob" 25 "absence" 0)
                ("Bob" 25 "out" 0)
                ("Carol" 30 "presence" 1)
                ("Carol" 30 "in" 1)
                ("Dave" 32 "absence" 0)
                ("Dave" 32 "out" 0)))
(define (join db1 db2)
  (local ((define schema1 (db-schema db1))
          (define schema2 (db-schema db2)))
    (if (not (equal? (first (last schema1))
                     (first (first schema2))))
      (error "join -- last column of db1 and first column of db2 must have the same spec" (list db1 db2))
      (local ((define colnames-right
                (map first (rest schema2)))

              (define joint-schema
                (append (but-last schema1) (rest schema2)))

              ; Row -> [List-of Row]
              ; "translates" last cell in row to its
              ;   counterpart(s) in db2
              (define (join-row row)
                (local ((define row-left (but-last row))
                        (define rows-right
                          (select db2
                                  colnames-right
                                  (lambda (r)
                                    (equal? (first r) (last row))))))
                  (map (lambda (r) (append row-left r))
                       rows-right))))
        (make-db joint-schema
         (foldr append
                empty
                (map join-row (db-content db1))))))))

; [NEList-of Any] -> [List-of Any]
; removes last element from non-empty list nel
(check-expect (but-last '(1 2 3)) '(1 2))
(check-expect (but-last '(1)) '())
(define (but-last nel)
  (cond [(empty? (rest nel)) empty]
        [else (cons (first nel) (but-last (rest nel)))]))

; [NEList-of Any] -> Any
; returns the last element of nel
(check-expect (last '(1 2 3)) 3)
(check-expect (last '(1)) 1)
(define (last nel)
  (if (empty? (rest nel))
      (first nel)
      (last (rest nel))))