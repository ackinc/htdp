;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chapter-22) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require 2htdp/universe)
(require 2htdp/image)

; An XExpr (short for X-expression) is one of:
; - (list Symbol)
; - (list Symbol [List-of Attr])
; - (cons Symbol [List-of XExpr])
; - (cons Symbol (cons [List-of Attr] [List-of XExpr]))

; An Attr is a (list Symbol String)

; ex 363
; A data definition for XExpr that doesn't use "List-of ...":

; An XExpr is a (cons Symbol OptionalAttrsAndBody)
; An OptionalAttrsAndBody is one of:
; - '()
; - (list Attrs)
; - XExprs
; - (cons Attrs XExprs)
; An Attrs is one of:
; - '()
; - (cons Attr Attrs)
; An XExprs is one of:
; - '()
; - (cons XExpr XExprs)

; ex 364
(define example-1 '(transition ((from "seen-e")
                                (to "seen-f"))))
(define example-2 '(ul (li (word))
                       (li (word))))

; ex 365
; <server name="example.org"></server>

; <carcas><board><grass></grass></board>
;         <player name="sam"></player>
; </carcas>

; <start></start>


; ex 366
; To make working with X-expressions easy, we define parsers:
; - xexpr-name
; - xexpr-attr (extracts the list of attrs of an X-expr)
; - xexpr-content

; Some data examples to help with future tests:
(define a0 '((name "sam")))
(define e0 '(machine))
(define e1 '(machine (action)))
(define e2 '(machine () (action)))
(define e3 `(machine ,a0 (action)))
(define e4 `(machine ,a0 (action) (action)))

; XExpr -> Symbol
; extracts the name from an XExpr
(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e4) 'machine)
(define (xexpr-name xexp) (first xexp))

; XExpr -> Attrs
; extracts the attrs of an XExpr
(check-expect (xexpr-attr example-1) '((from "seen-e")
                                       (to "seen-f")))
(check-expect (xexpr-attr example-2) empty)
(check-expect (xexpr-attr e0) empty)
(check-expect (xexpr-attr e1) empty)
(check-expect (xexpr-attr e2) empty)
(check-expect (xexpr-attr e3) a0)
(check-expect (xexpr-attr e4) a0)
(define (xexpr-attr xexp)

  ; "garage programming" solution
  ;(if (and (>= (length xexp) 2)
  ;         (list? (second xexp))
  ;         (not (empty? (second xexp)))
  ;         (list? (first (second xexp))))
  ;    (second xexp)
  ;    empty))

  ; properly-designed solution
  (local ((define optional-loa-or-content (rest xexp)))
    (cond [(empty? optional-loa-or-content) empty]
          [(list-of-attributes? (first optional-loa-or-content))
           (first optional-loa-or-content)]
          [else empty])))

; ex 368
; An AttrsOrXExpr is one of:
; - Attrs
; - XExpr

; ex 366 continued
; AttrsOrXExpr -> Boolean
; returns true if ex is a list of attributes
(check-expect (list-of-attributes? a0) #true)
(check-expect (list-of-attributes? e0) #false)
(define (list-of-attributes? ex)
  (if (empty? ex)
      #true
      (match (first ex)
        [(? symbol?) #false]
        [(? cons?) #true])))

; XExpr -> XExprs
; extracts the content of xexp
(check-expect (xexpr-content e0) empty)
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))
(define (xexpr-content xexp)
  (local ((define optional-loa-or-content (rest xexp)))
    (cond [(empty? optional-loa-or-content) empty]
          [(list-of-attributes? (first optional-loa-or-content))
           (rest optional-loa-or-content)]
          [else optional-loa-or-content])))

; ex 369
; Attrs Symbol -> [Maybe String]
; returns the value of the given attr in a list of attrs
(check-expect (find-attr a0 'name) "sam")
(check-expect (find-attr a0 'age) #false)
(define (find-attr attrs name)
  (local ((define result (assq name attrs)))
    (if (false? result)
        #false
        (second result))))

; We need a data definition for "plain" strings in XML
; An XWord is a '(word ((text String)))

; ex 370
; Data Examples for XWord
(define xw-1 '(word ((text "27"))))
(define xw-2 '(word ((text "Elias"))))
(define xw-3 '(word ((text "Samson"))))

; Any -> Boolean
; checks if given arg is an XWord
(check-expect (word? e0) #false)
(check-expect (word? 1) #false)
(check-expect (word? xw-1) #true)
(define (word? ex)
  (match ex
    [(list 'word (list (list 'text x))) #true]
    [x #false]))

; XWord -> String
; extracts the text from an XWord
(check-expect (word-text xw-1) "27")
(check-expect (word-text xw-2) "Elias")
(check-error (word-text 0))
(define (word-text word)
  (match word
    [(list 'word (list (list 'text x))) x]
    [else (error "Not an XWord" word)]))

; ex 371
; Refine the data definition for XExpr
;   so you can represent XML elements, including items in
;   enumerations, that are plain strings
; Nothing needs to be done here. The current definition
;   already handles such items, since every XWord is a XExpr

; Version 1 data definitions for XHTML enumerations (<ul>)
; An XEnum is one of:
; - (cons 'ul [List-of XItem])
; - (cons 'ul (cons Attrs [List-of XItem]))
; An XItem is one of:
; - (list 'li XWord)
; - (cons 'li (cons Attrs (list XWord)))

; Ignoring possibility of empty XItem (<li />) since it
;   makes some of the following function definitions more
;   tedious

; ex 372
; Not sure how to formulate tests for render-item1 that
;   don't depend on BT
; Even if I created at test function that took an image
;   as the "bullet" arg, and returned another function
;   that can be used with check-satisfied, I'm still using BT

; Version 2 data definitions for XHTML enumerations (<ul>)
; An XEnum is one of:
; - (cons 'ul [List-of XItem])
; - (cons 'ul (cons Attrs [List-of XItem]))
; An XItem is one of:
; - (list 'li XWord)
; - (cons 'li (cons Attrs (list XWord)))
; - (list 'li XEnum)
; - (cons 'li (cons Attrs (list XEnum)))

; Again, ignoring empty list elements (<li />)

; Data Example
(define ex-item-1 '(li (word ((text "potatoes")))))
(define ex-item-2 '(li (word ((text "onions")))))
(define ex-item-3 '(li (word ((text "garlic")))))
(define ex-enum-1 `(ul ((name "shopping list"))
                       ,ex-item-1
                       ,ex-item-2
                       ,ex-item-3))

(define ex-item-4 `(li ,ex-enum-1))
(define ex-enum-2 `(ul ((name "shopping lists"))
                       ,ex-item-1
                       ,ex-item-3
                       ,ex-item-4))

; useful for ex 377
(define ex-enum-3 `(ul ((name "shopping list"))
                       ,ex-item-2
                       ,ex-item-2
                       ,ex-item-3))
(define ex-item-5 `(li ,ex-enum-3))
(define ex-item-6 '(li ((size "12") (weight "1"))
                       (word ((text "shoes")))))
(define ex-item-7 '(li ((size "12") (weight "1"))
                       (word ((text "shoe")))))

; ex 376
; XEnum String -> N
; counts the number of times the string s occurs in xexp
(check-expect (count-in-enum ex-enum-1 "potato") 0)
(check-expect (count-in-enum ex-enum-1 "potatoes") 1)
(check-expect (count-in-enum ex-enum-2 "potatoes") 2)
(define (count-in-enum xexp s)
  (foldr + 0 (map (lambda (item) (count-in-item item s))
                  (xexpr-content xexp))))

; XItem String -> N
; counts the number of times the string s occurs in xexp
(check-expect (count-in-item ex-item-1 "potato") 0)
(check-expect (count-in-item ex-item-1 "potatoes") 1)
(check-expect (count-in-item ex-item-4 "potatoes") 1)
(define (count-in-item xexp s)
  (local ((define contents (xexpr-content xexp))
          (define content (first contents)))
    (if (word? content)
        (if (string=? (word-text content) s)
            1
            0)
        (count-in-enum content s))))

; ex 377

; String -> XWord
; helper function to create an XWord containing given string
(check-expect (makeword "27") xw-1)
(check-expect (makeword "Elias") xw-2)
(check-expect (makeword "Samson") xw-3)
(define (makeword s) `(word ((text ,s))))

; Attrs [XEnum or XItem] -> XItem
; helper function to create an XItem
(check-expect (makeitem empty (makeword "potatoes")) ex-item-1)
(check-expect (makeitem '((size "12") (weight "1"))
                        (makeword "shoe"))
              ex-item-7)
(define (makeitem attrs content)
  (if (empty? attrs)
      (cons 'li (list content))
      (cons 'li (cons attrs (list content)))))

; Attrs [List-of XItem] -> XEnum
; helper function to create an XEnum
(check-expect (makeenum empty
                        (list ex-item-1
                              ex-item-2
                              ex-item-3))
              `(ul ,ex-item-1 ,ex-item-2 ,ex-item-3))
(check-expect (makeenum '((name "shopping list"))
                        (list ex-item-1
                              ex-item-2
                              ex-item-3))
              ex-enum-1)
(define (makeenum attrs items)
  (if (empty? attrs)
      (cons 'ul items)
      (cons 'ul (cons attrs items))))

; XEnum String String -> XEnum
; replaces instances of s1 with s2 in xexp
(check-expect (replace-in-enum ex-enum-1 "potato" "onions")
              ex-enum-1)
(check-expect (replace-in-enum ex-enum-1 "potatoes" "onions")
              ex-enum-3)
(define (replace-in-enum xexp s1 s2)
  (makeenum (xexpr-attr xexp)
            (map (lambda (item) (replace-in-item item s1 s2))
                 (xexpr-content xexp))))

; XItem String String -> XItem
; replaces instances of s1 with s2 in xexp
(check-expect (replace-in-item ex-item-1 "potato" "garlic")
              ex-item-1)
(check-expect (replace-in-item ex-item-1 "potatoes" "garlic")
              ex-item-3)
(check-expect (replace-in-item ex-item-4 "potatoes" "onions")
              ex-item-5)
; test to check that attrs remain untouched
(check-expect (replace-in-item ex-item-6 "shoes" "shoe")
              ex-item-7)
(define (replace-in-item xexp s1 s2)
  (local ((define attrs (xexpr-attr xexp))
          (define content (first (xexpr-content xexp))))
    (cond [(word? content)
           (if (string=? (word-text content) s1)
               (makeitem attrs (makeword s2))
               xexp)]
          [else (makeitem
                 attrs
                 (replace-in-enum content s1 s2))])))



;;; Section 22.3 - Domain-Specific Languages ;;;
; Design, implement, and program a DSL for configuring
;   a system that simulates arbitrary FSMs

; Data definitions for FSMs:
; An FSM is a [List-of 1Transition]
; A 1Transition is a (list FSM-State FSM-State)
; An FSM-State is a String representing a color

; FSM data examples:
(define fsm-traffic '(("red" "yellow")
                      ("yellow" "green")
                      ("green" "red")))
(define fsm-bw '(("black" "white")
                 ("white" "black")))

; FSM FSM-State -> FSM-State 
; matches the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
      (lambda (current)
        (square 100 "solid" current))]
    [on-key
      (lambda (current key-event)
        (find transitions current))]))
 
; [X Y] [List-of [list X Y]] X -> Y
; finds the matching Y for the given X in alist
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

; The FSM simulator "program" takes two args:
; - initial state (FSM-State)
; - list of possible transitions (FSM)

; We want to allow a user of the simulate program to specify
;   this "configuration" in an XML file

; Example:
; <machine initial="red">
;   <action from="red" to="yellow" />
;   <action from="yellow" to="green" />
;   <action from="green" to="red" />
; </machine>

; The above XML configuration can be read into an XExpr:
; '(machine ((initial "red"))
;           (action ((from "red") (to "yellow")))
;           (action ((from "yellow") (to "green")))
;           (action ((from "green") (to "red"))))

; Data definition for FSM configurations
; An XMachine is:
; `(machine ((initial ,FSM-State)) ,@[List-of XTransition])
; An XTransition is:
; `(action ((from ,FSM-State) (to ,FSM-State)))

; ex 381
; rewriting the above data defs using list and cons
;   instead of quote
; An XMachine is:
; - (cons 'machine (cons (list (list 'initial FSM-State))
;                        [List-of XTransition]))
; An XTransition is:
; - (list 'action (list (list 'from FSM-State)
;                       (list 'to FSM-State)))

; ex 382
(define bw-conf
  '(machine ((initial "black"))
            (action ((from "black") (to "white")))
            (action ((from "white") (to "black")))))

(define tl-conf
  '(machine ((initial "red"))
            (action ((from "red") (to "yellow")))
            (action ((from "yellow") (to "green")))
            (action ((from "green") (to "red")))))

; XMachine -> FSM-State
; extracts the initial state from the given conf
(check-expect (xm-state0 tl-conf) "red")
(check-expect (xm-state0 bw-conf) "black")
(define (xm-state0 conf) (find-attr (xexpr-attr conf) 'initial))

; XMachine -> FSM
; extracts the list of transitions from given conf
(check-expect (xm->transitions tl-conf) fsm-traffic)
(check-expect (xm->transitions bw-conf) fsm-bw)
(define (xm->transitions conf)
  (local ((define actions (xexpr-content conf))
          (define (action->1transition a)
            (local ((define attrs (xexpr-attr a)))
              (list (find-attr attrs 'from)
                    (find-attr attrs 'to)))))
    (map action->1transition actions)))


; XMachine -> FSM-State
; interprets the given configuration and simulates an FSM
(define (simulate-xmachine conf)
  (simulate (xm-state0 conf) (xm->transitions conf)))

; ex 383
;(simulate-xmachine bw-conf)
;(simulate-xmachine tl-conf)



;;; Section 22.4 - Reading XML ;;;

; ex 386
; XExpr String -> [Maybe String]
; searches x for a 'meta x-expression whose "itemprop" attr
;   has value = s
; if found, returns the value of its "content" attribute
; if not found, returns #false
(check-expect (get-xexpr '(machine) "name") #false)
(check-expect (get-xexpr '(meta) "name") #false)
(check-expect (get-xexpr '(meta ((itemprop "age"))) "name")
              #false)
(check-expect (get-xexpr '(meta ((itemprop "name"))) "name")
              #false)
(check-expect (get-xexpr '(meta ((itemprop "name")
                                 (content "anirudh"))) "name")
              "anirudh")
(check-expect (get-xexpr '(html
                           (head
                            (meta ((itemprop "name")
                                   (content "anirudh")))
                            (meta ((itemprop "age")
                                   (content "27"))))
                           (body)) "name")
              "anirudh")
(define (get-xexpr x s)
  (local ((define attrs (xexpr-attr x)))
    (if (and (symbol=? (xexpr-name x) 'meta)
             (equal? (find-attr attrs 'itemprop) s))
        (find-attr attrs 'content)
        (first-non-false-value (map (lambda (ex) (get-xexpr ex s))
                                    (xexpr-content x))))))

; [List-of Any] -> Any
; returns the first value in l that is not #false
(check-expect (first-non-false-value empty) #false)
(check-expect (first-non-false-value (list #false #false)) #false)
(check-expect (first-non-false-value (list #false #false 1)) 1)
(define (first-non-false-value l)
  (foldl (lambda (x acc) (if (false? acc)
                             x
                             acc))
         #false
         l))