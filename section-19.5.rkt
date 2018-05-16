;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname section-19.5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))

; A Node is a struct: (make-node Number Symbol BT BT)
(define-struct node [ssn name left right])

; A BT is one of:
; - NONE
; - Node

; Data Examples:
(define node1 (make-node 1 'a NONE NONE))
(define node3 (make-node 3 'c NONE NONE))
(define node2 (make-node 2 'b node1 node3))

(define node5 (make-node 5 'e NONE NONE))
(define node7 (make-node 7 'g NONE NONE))
(define node6 (make-node 6 'f node5 node7))

(define node4 (make-node 4 'd node2 node6))

; ex 322
; Number BT -> Boolean
; checks if n occurs in bt
(check-expect (contains-bt? 3 node6) #false)
(check-expect (contains-bt? 3 node4) #true)
(define (contains-bt? n bt)
  (cond [(no-info? bt) #false]
        [(equal? (node-ssn bt) n) #true]
        [else (or (contains-bt? n (node-left bt))
                  (contains-bt? n (node-right bt)))]))

; ex 323
; Number BT -> [Maybe Symbol]
; searches BT for node with ssn = n
;   returns node's name if found
;   returns false if not found
(check-expect (search-bt 2 node6) #false)
(check-expect (search-bt 2 node4) 'b)
(define (search-bt n bt)
  (cond [(no-info? bt) #false]
        [(= (node-ssn bt) n) (node-name bt)]
        [else (local ((define left-result (search-bt n (node-left bt))))
                (if (false? left-result)
                    (search-bt n (node-right bt))
                    left-result))]))

; ex 324
; BT -> [List-of Number]
; performs an inorder traversal of bt
(check-expect (inorder node1) (list 1))
(check-expect (inorder node2) (list 1 2 3))
(define (inorder bt)
  (cond [(no-info? bt) empty]
        [else (append (inorder (node-left bt))
                      (list (node-ssn bt))
                      (inorder (node-right bt)))]))

; ex 325
; Number BST -> BST
; searches bst for node with ssn = "n"
(check-expect (search-bst 10 node4) NONE)
(check-expect (search-bst 4 node4) 'd)
(check-expect (search-bst 1 node4) 'a)
(define (search-bst n bst)
  (cond [(no-info? bst) NONE]
        [(= n (node-ssn bst)) (node-name bst)]
        [(> n (node-ssn bst)) (search-bst n (node-right bst))]
        [else (search-bst n (node-left bst))]))


; ex 326
; BST Number Symbol -> BST
; creates a new BST from bst with new node inserted
(define treeA (make-node 63 'a
                         (make-node 29 'b
                                    (make-node 15 'c
                                               (make-node 10 'd NONE NONE)
                                               (make-node 24 'e NONE NONE))
                                    NONE)
                         (make-node 89 'f
                                    (make-node 77 'g NONE NONE)
                                    (make-node 95 'h
                                               NONE
                                               (make-node 99 'i NONE NONE)))))
(check-expect (inorder (create-bst (create-bst treeA 88 'j) 36 'k))
              (list 10 15 24 29 36 63 77 88 89 95 99))
(define (create-bst bst n s)
  (cond [(no-info? bst) (make-node n s NONE NONE)]
        [(<= n (node-ssn bst)) (make-node (node-ssn bst)
                                          (node-name bst)
                                          (create-bst (node-left bst) n s)
                                          (node-right bst))]
        [else (make-node (node-ssn bst)
                         (node-name bst)
                         (node-left bst)
                         (create-bst (node-right bst) n s))]))


; ex 327
; [List-of [List-of Number Symbol]] -> BST
; creates BST from list
(check-expect (inorder (create-bst-from-list '((99 o)
                                               (77 l)
                                               (24 i)
                                               (10 h)
                                               (95 g)
                                               (15 d)
                                               (89 c)
                                               (29 b)
                                               (63 a))))
              (inorder treeA))
(define (create-bst-from-list l)
  (cond [(empty? l) NONE]
        [else (create-bst (create-bst-from-list (rest l))
                          (first (first l))
                          (second (first l)))]))