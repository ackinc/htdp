;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-522) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; physical constants
(define WIDTH 50) ; width of river in px
(define HEIGHT 50) ; height of river in px

(define N-MISSIONARIES 3)
(define N-CANNIBALS 3)

; graphical constants
(define RIVER (rectangle WIDTH HEIGHT "solid" "lightblue"))
(define BANK (rectangle (/ WIDTH 2) HEIGHT "solid" "brown"))

(define BOAT (square (/ WIDTH 2.5) "solid" "yellow"))
(define MISSIONARY (circle (/ WIDTH 10) "solid" "black"))
(define CANNIBAL (circle (/ WIDTH 10) "outline" "black"))

; An N[x, y] is an N in [x, y]

(define-struct pstate [m c b h])
; A PuzzleState is a structure: (make-pstate [N[0, 3] N[0, 3] N[0, 1] [List-of PuzzleStateWithoutHistory]])
; interpretation:
; - m: #missionaries on left side of river
; - c: #cannibals on left side of river
; - b: 0 if on left-side of river, 1 otherwise
; - h (accumulator): list of states we went through to reach
;                      the current one, in
;                      reverse-chronological order


(define-struct pstatewh [m c b])
; A PuzzleStateWithoutHistory is a structure: (make-pswh [N[0, 3] N[0, 3] N[0, 1]])

; PuzzleState -> PuzzleStateWithoutHistory
; O(1)
(define (pstate->pswh ps)
  (make-pstatewh (pstate-m ps) (pstate-c ps) (pstate-b ps)))

; PuzzleState data examples
(define start-state (make-pstate 3 3 0 empty))
(define mid-state (make-pstate 2 2 1 (list (pstate->pswh start-state))))
(define end-state (make-pstate 0 0 1 (map pstate->pswh
                                          (list mid-state start-state))))
(define bad-state (make-pstate 2 3 0 (map pstate->pswh
                                          (list mid-state start-state))))


; PuzzleState -> Boolean
; checks whether current state is the final PuzzleState
; O(1)
(check-expect (final? start-state) #false)
(check-expect (final? mid-state) #false)
(check-expect (final? end-state) #true)
(define (final? ps)
  (and (zero? (pstate-m ps))
       (zero? (pstate-c ps))))

; PuzzleState -> [List-of Image]
(define (render-mc ps)
  (local (; PuzzleStateWithoutHistory -> Image
          (define (render-one pswh)
            (local ((define c-left (above-n CANNIBAL (pstatewh-c pswh)))
                    (define m-left (above-n MISSIONARY (pstatewh-m pswh)))
                    (define c-right (above-n CANNIBAL
                                             (- N-CANNIBALS
                                                (pstatewh-c pswh))))
                    (define m-right (above-n MISSIONARY
                                             (- N-MISSIONARIES
                                                (pstatewh-m pswh)))))
              (overlay/align "right" "middle"
                             (beside m-right c-right)
                             (overlay/align "left" "middle"
                                            (beside c-left m-left)
                                            (beside BANK
                                                    (place-boat (pstatewh-b pswh))
                                                    BANK))))))
    (reverse (cons (render-one (pstate->pswh ps))
                   (map render-one (pstate-h ps))))))

; Image N -> Image
; stacks n copies of img vertically
; O(n)
(define (above-n img n)
  (if (zero? n)
      empty-image
      (above img (above-n img (sub1 n)))))

; N[0, 1] -> Image
; draws the boat in the appropriate position on the river
; O(1)
(define (place-boat pos)
  (overlay/align (if (zero? pos) "left" "right")
                 "middle"
                 BOAT
                 RIVER))


; ex 523
; [List-of PuzzleState] -> [List-of PuzzleState]
; generates list of pstates that can be reached from the
;   pstates in los
; O(mn), where m is (length los), and n is length of history
;         of the states in los
(define (create-next-states los)
  (foldr append empty (map create-next-states-for-one los)))

; PuzzleState -> [List-of PuzzleState]
; generates list of pstates that can be reached from ps
; O(n), where n is length of ps' history
(define (create-next-states-for-one ps)
  (filter (lambda (ps)
            (and (not (false? ps))
                 (acceptable-state? ps)
                 (not (already-reached-state? ps))))
          (list (move-one-cannibal ps)
                (move-two-cannibals ps)
                (move-one-missionary ps)
                (move-two-missionaries ps)
                (move-one-of-each ps))))

; PuzzleState -> Boolean
; returns #true if missionaries don't get eaten!
; O(1)
(check-expect (acceptable-state? start-state) #true)
(check-expect (acceptable-state? bad-state) #false)
(define (acceptable-state? ps)
  (local ((define c-left (pstate-c ps))
          (define m-left (pstate-m ps))
          (define c-right (- N-CANNIBALS c-left))
          (define m-right (- N-MISSIONARIES m-left)))
    (and (or (zero? m-left) (>= m-left c-left))
         (or (zero? m-right) (>= m-right c-right)))))

; PuzzleStateWithoutHistory PuzzleStateWithoutHistory -> Boolean
; returns #true if the two states are identical
; O(1)
(check-expect (same-state? (pstate->pswh start-state)
                           (pstate->pswh start-state)) #true)
(check-expect (same-state? (pstate->pswh start-state)
                           (pstate->pswh end-state)) #false)
(check-expect (same-state? (pstate->pswh mid-state)
                           (make-pstatewh 2 2 1)) #true)
(define (same-state? pswh1 pswh2)
  (and (= (pstatewh-m pswh1) (pstatewh-m pswh2))
       (= (pstatewh-c pswh1) (pstatewh-c pswh2))
       (= (pstatewh-b pswh1) (pstatewh-b pswh2))))

; PuzzleState -> Boolean
; returns #true if current state was already reached previously
; O(n), where n is length of ps' history
(check-expect (already-reached-state? mid-state) #false)
(check-expect (already-reached-state? (make-pstate 3 3 0 (map pstate->pswh (list mid-state start-state)))) #true)
(define (already-reached-state? ps)
  (local ((define pswh (pstate->pswh ps)))
    (ormap (lambda (s) (same-state? pswh s)) (pstate-h ps))))


; helper functions representing moves in the game

; N[1, 2] N[1, 2] PuzzleState -> [Maybe PuzzleState]
; simulates nm missionaries and nc cannibals taking the
;   boat to the opposite bank
; returns #false if there are not enough
;   missionaries/cannibals on the side of boat
; O(1)
(check-expect (move-people 1 1 start-state) mid-state)
(check-expect (move-people 0 1 mid-state) bad-state)
(check-expect (move-people 0 2 mid-state) #false)
(define (move-people nm nc ps)
  (local ((define boat-on-left? (zero? (pstate-b ps)))
          (define c-left (pstate-c ps))
          (define c-right (- N-CANNIBALS c-left))
          (define c (if boat-on-left? c-left c-right))

          (define m-left (pstate-m ps))
          (define m-right (- N-MISSIONARIES m-left))
          (define m (if boat-on-left? m-left m-right)))

    (if (or (< m nm) (< c nc))
        #false
        (make-pstate (- m-left (* nm (if boat-on-left? 1 -1)))
                     (- c-left (* nc (if boat-on-left? 1 -1)))
                     (if boat-on-left? 1 0)
                     (cons (pstate->pswh ps) (pstate-h ps))))))

(define (move-one-cannibal ps) (move-people 0 1 ps))
(define (move-two-cannibals ps) (move-people 0 2 ps))
(define (move-one-missionary ps) (move-people 1 0 ps))
(define (move-two-missionaries ps) (move-people 2 0 ps))
(define (move-one-of-each ps) (move-people 1 1 ps))

; PuzzleState -> [Maybe PuzzleState]
; generates the final puzzle state (if it can be reached)
;   starting from ps
(define (solve ps)
  (local ((define (solve* los)
            (cond [(empty? los) #false]
                  [(ormap final? los) (first (filter final? los))]
                  [else (solve* (create-next-states los))])))
    (solve* (list ps))))

; 1 step to finish
(render-mc (solve (make-pstate 0 2 0 empty)))

; 2 steps to finish
(render-mc (solve (make-pstate 0 1 1 empty)))

; 3 steps to finish
(render-mc (solve (make-pstate 0 3 0 empty)))

; 5 steps to finish
(render-mc (solve (make-pstate 2 2 0 empty)))

; actual puzzle - 11 steps to finish
(render-mc (solve start-state))