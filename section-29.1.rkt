;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname section-29.1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; For the FSM problem (ex 476)
(require 2htdp/image)
(require 2htdp/universe)

; A Node is a Symbol
; A Graph is a [List-of Node-Representation]

; A Node-Representation is a (list Node [List-of Node])
; interpretation: the inner list is a list of all the nodes
;   that have incoming edges from the first node

; an acyclic graph
(define ex-graph-acyclic '((A (B E))
                           (B (E F))
                           (C (D))
                           (D ())
                           (E (C F))
                           (F (D G))
                           (G ())))

; this graph has a cycle
(define ex-graph-cyclic '((A (B E))
                          (B (E F))
                          (C (B D))
                          (D ())
                          (E (C F))
                          (F (D G))
                          (G ())))

; Node Graph -> [List-of Node]
; returns the neighbours of the given node n in graph g
;   or #false if n is not in g
(check-expect (neighbours 'A ex-graph-acyclic) '(B E))
(check-expect (neighbours 'D ex-graph-acyclic) '())
(check-expect (neighbours 'H ex-graph-acyclic) #false)
(define (neighbours n g)
  (local ((define result (assq n g)))
    (if (false? result)
        #false
        (second result))))

; A Path is a [List-of Node]

; Graph Node Node -> [Maybe Path]
; returns a path from node m to n in an acyclic
;   graph g, if one exists
; termination: fails to terminate if graph is cyclic
(check-member-of (find-path ex-graph-acyclic 'A 'G)
                 '(A B E F G)
                 '(A E F G)
                 '(A B F G))
(check-expect (find-path ex-graph-acyclic 'C 'G) #false)

; test below will not terminate, since ex-graph-2 has a cycle
;(check-expect (find-path ex-graph-cyclic 'C 'G) #false)

(define (find-path g orig dest)
  (if (symbol=? orig dest)
      (list orig)
      (local ((define partial-result
                (first-non-false-value
                 (map (lambda (nbr)
                        (find-path g nbr dest))
                      (neighbours orig g)))))
        (if (false? partial-result)
            #false
            (cons orig partial-result)))))

; [List-of [Maybe X]] -> [Maybe X]
; returns the first element of l that is not #false
(define (first-non-false-value l)
  (cond [(empty? l) #false]
        [(false? (first l)) (first-non-false-value (rest l))]
        [else (first l)]))

; Graph -> Boolean
; tests whether all nodes of g are connected
(check-expect (test-on-all-nodes ex-graph-acyclic) #false)

; test below will not terminate, since
;   ex-graph-cyclic has a cycle
;(check-expect (test-on-all-nodes ex-graph-cyclic) #true)

(define (test-on-all-nodes g)
  (local ((define nodes (map first g))
          ; Node -> Boolean
          ; returns true if given node is connected
          ;   to all nodes of g
          (define (connected-to-all-nodes n)
            (andmap (lambda (m)
                      (not (false? (find-path g n m))))
                    nodes)))
    (andmap connected-to-all-nodes nodes)))


; ex 476
(define-struct transition [current key next])
(define-struct fsm [initial transitions final])

; An FSM is a structure:
;   (make-fsm FSM-State [List-of Transition] FSM-State)

; An FSM-State is a String

; A Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)

; data example
(define fsm-a-bc*-d
  (make-fsm "AA" (list (make-transition "AA" "a" "BC")
                       (make-transition "BC" "b" "BC")
                       (make-transition "BC" "c" "BC")
                       (make-transition "BC" "d" "DD"))
            "DD"))

; FSM String -> Boolean
; returns true if the key-presses represented by s
;   cause fsm to reach its finish state
(check-expect (fsm-match fsm-a-bc*-d "abd") #true)
(check-expect (fsm-match fsm-a-bc*-d "acbcd") #true)
(check-expect (fsm-match fsm-a-bc*-d "acbc") #false)
(define (fsm-match fsm s)
  (local ((define transitions (fsm-transitions fsm))
          (define (apply-successive-state-changes cur-state
                                                  key-presses)
            (if (zero? (string-length key-presses))
                cur-state
                (apply-successive-state-changes
                 (fsm-next-state transitions
                                 cur-state
                                 (string-first key-presses))
                 (string-rest key-presses)))))
    (string=? (apply-successive-state-changes
               (fsm-initial fsm) s)
              (fsm-final fsm))))

; [List-of Transition] FSM-State 1String
; returns the state to which keypress k transitions fsm
; throws an error if keypress is invalid
(define tx-a-bc*-d (fsm-transitions fsm-a-bc*-d))
(check-expect (fsm-next-state tx-a-bc*-d "AA" "a") "BC")
(check-error (fsm-next-state tx-a-bc*-d "AA" "b"))
(check-expect (fsm-next-state tx-a-bc*-d "BC" "b") "BC")
(check-expect (fsm-next-state tx-a-bc*-d "BC" "c") "BC")
(check-expect (fsm-next-state tx-a-bc*-d "BC" "d") "DD")
(define (fsm-next-state transitions cur-state key)
  (cond [(empty? transitions)
         (error "fsm-next-state -- invalid keypress"
                (list transitions cur-state key))]
        [else (local ((define tx (first transitions)))
                (if (and (string=? (transition-current tx)
                                   cur-state)
                         (string=? (transition-key tx)
                                   key))
                    (transition-next tx)
                    (fsm-next-state (rest transitions)
                                    cur-state
                                    key)))]))

; String -> 1String
; returns the first character of a non-empty string
(check-error (string-first ""))
(check-expect (string-first "a") "a")
(check-expect (string-first "ani") "a")
(define (string-first s)
  (if (zero? (string-length s))
      (error "string-first -- cannot get first char "
             "of empty string" s)
      (substring s 0 1)))

; String -> String
; returns all but the first character of a non-empty string
(check-error (string-rest ""))
(check-expect (string-rest "a") "")
(check-expect (string-rest "ani") "ni")
(define (string-rest s)
  (if (zero? (string-length s))
      (error "string-rest -- cannot get rest of chars "
             "of empty string" s)
      (substring s 1)))

; an alternative implementation of fsm-match, using big-bang
; FSM String -> Boolean
; returns true if the key-presses represented by s
;   cause fsm to reach its finish state
(check-expect (fsm-match-alt fsm-a-bc*-d "abd") #true)
(check-expect (fsm-match-alt fsm-a-bc*-d "acbcd") #true)
(check-expect (fsm-match-alt fsm-a-bc*-d "acbc") #false)
(define EMPTY-SCENE (empty-scene 100 100))
(define (fsm-match-alt fsm s)
  ; A WorldState is (list FSM-State String)
  ; interpretation: the current state of the fsm,
  ;   and the keypresses representing the
  ;   remaining transitions
  (local ((define initial (fsm-initial fsm))
          (define transitions (fsm-transitions fsm))
          (define final (fsm-final fsm))

          ; [List-of Transition] String 1String -> String
          ; returns the state resulting from pressing k
          ;   when current state is cs
          (define (next-state rem-trans cs k)
            (if (empty? rem-trans)
                (error "next-state -- invalid keypress"
                       (list transitions cs k))
                (local ((define t (first rem-trans)))
                  (if (and (equal? (transition-current t) cs)
                           (equal? (transition-key t) k))
                      (transition-next t)
                      (next-state (rest rem-trans) cs k)))))

          (define (tock cw)
            (local ((define rem-keys (second cw)))
              (list (next-state transitions
                                (first cw)
                                (string-first rem-keys))
                    (string-rest rem-keys))))

          (define (end? cw)
            (zero? (string-length (second cw)))))

    (string=? (first (big-bang (list initial s)
                               [on-tick tock]
                               [stop-when end?]
                               [to-draw (lambda (cw)
                                          EMPTY-SCENE)]))
              final)))


; [List-of Any] -> [List-of [List-of Any]]
; returns all possible arrangements of l using
;   generative recursion
; how: for each elem in l, first generates all arrangements
;        of elems other then elem, then prepends elem to those
;        arrangements
(define (arrangements l)
  (if (empty? l)
      (list empty)
      (foldr (lambda (elem result)
               (append (map (lambda (arrangement)
                              (cons elem arrangement))
                            (arrangements (remove elem l)))
                       result))
             empty
             l)))