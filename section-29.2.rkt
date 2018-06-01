;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname section-29.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/abstraction)

; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column


; ex 479
; QP QP -> Boolean
; checks whether a queen at position qp2 is threatened by
;   one at position qp1
(check-expect (threatening? (make-posn 0 0)
                            (make-posn 5 0))
              #true)
(check-expect (threatening? (make-posn 0 0)
                            (make-posn 0 5))
              #true)
(check-expect (threatening? (make-posn 0 0)
                            (make-posn 5 5))
              #true)
(check-expect (threatening? (make-posn 1 2)
                            (make-posn 0 1))
              #true)
(check-expect (threatening? (make-posn 0 0)
                            (make-posn 1 2))
              #false)
(define (threatening? qp1 qp2)
  (local ((define x1 (posn-x qp1))
          (define y1 (posn-y qp1))
          (define x2 (posn-x qp2))
          (define y2 (posn-y qp2)))
    (or (= x1 x2) ; same row
        (= y1 y2) ; same column
        (= (abs (- x1 x2)) (abs (- y1 y2))))))

; QP [List-of QP] -> Boolean
; returns true if queen at qp is threatened by any
;   of the queens in placed
(define (threatened? qp placed)
  (ormap (lambda (oqp) (threatening? qp oqp)) placed))


; ex 480
(define QUEEN
  (above (square 8 "solid" "black")
         (overlay/align "middle" "bottom"
                        (triangle 8 "solid" "black")
                        (rectangle 5 20 "outline" "black"))))

; side of one chessboard square, in px
(define SIDE (* 1.5 (image-height QUEEN)))
(define SQUARE (square SIDE "outline" "black"))

; Image N [Image Image -> Image] -> Image
; places n copies of img beside/above each other
;   as dictated by placefn
(check-expect (place-n SQUARE 2 beside)
              (beside SQUARE SQUARE))
(check-expect (place-n SQUARE 3 above)
              (above SQUARE SQUARE SQUARE))
(define (place-n img n placefn)
  (if (zero? n)
      empty-image
      (placefn img (place-n img (sub1 n) placefn))))

; N [List-of QP] Image -> Image
; draws an nxn chess board with the queens positioned
;   according to qps
(define (render-queens n qps queen-img)
  (local ((define ROW (place-n SQUARE n beside))
          (define CHESSBOARD (place-n ROW n above))
          (define (qp-coord->abs-coord c)
            (+ (* c SIDE) (/ SIDE 2)))
          (define (place-queen qp board)
            (place-image QUEEN
                         (qp-coord->abs-coord (posn-x qp))
                         (qp-coord->abs-coord (posn-y qp))
                         board)))
    (foldr place-queen CHESSBOARD qps)))


; ex 481
; N [List-of QP] -> Boolean
; determines if the given placement of queens is a solution
;   to the n-queens problem
(define (n-queens-solution? n qps)
  (cond [(not (= n (length qps))) #false]
        [(zero? n) #true]
        [(threatened? (first qps) (rest qps)) #false]
        [else (n-queens-solution? (sub1 n) (rest qps))]))

; [List-of Any] [List-of Any] -> Boolean
; tests if set a is a subset of set b
(define (subset? a b)
  (andmap (lambda (a-elem) (member? a-elem b)) a))

; [List-of Any] [List-of Any] -> Boolean
; tests whether two sets are equivalent (contain the
(define (set=? a b) (and (subset? a b) (subset? b a)))


; ex 482

; [X -> [Maybe Y]] [List-of X] -> [Maybe Y]
; returns the first non-false result obtained on
;   mapping f on l
(check-expect (ormap-alt even? '(1 2 3 4)) #true)
(check-expect (ormap-alt (lambda (n)
                           (if (odd? n) n #false))
                         '(6 2 3 4)) 3)
(check-expect (ormap-alt odd? '(6 2 2 4)) #false)
(define (ormap-alt f l)
  (foldl (lambda (elem result)
           (cond [(not (false? result)) result]
                 [else (f elem)]))
         #false
         l))

; Board N -> [Maybe [List-of QP]]
; places n queens on board (that may already
;   contain some queens)
(define (place-queens a-board n)
  (cond [(zero? n) empty]
        [else (local ((define open-spots
                        (find-open-spots a-board)))
                (if (empty? open-spots)
                    #false
                    (ormap-alt
                     (lambda (spot)
                       (local ((define result
                                 (place-queens (add-queen a-board spot)
                                               (sub1 n))))
                         (if (false? result)
                             #false
                             (cons spot result))))
                     open-spots)))]))


; ex 483

; Part 1
; A Board contains those positions where a queen can
;   still be (no guarantees of safety) placed
;(define-struct board [size positions])
; A Board is a structure: (make-board N [List-of Posn])

; N -> Board
; creates an empty nxn board
;(define (board0 n)
;  (make-board n
;              (for*/list ([i n] [j n])
;                (make-posn i j))))

; Board QP -> Board
; places a queen on a-board at position qp
;(define (add-queen a-board qp)
;  (make-board (board-size a-board)
;              (remove qp (board-positions a-board))))

; Board -> [List-of QP]
; given a board, with/out some queens
;   returns the positions of the queens
;(define (extract-qps a-board)
;  (local ((define all-spots
;            (board-positions (board0 (board-size a-board)))))
;    (filter (lambda (spot)
;              (not (member? spot (board-positions a-board))))
;            all-spots)))

; Board -> [List-of Posn]
; returns all unthreatened positions on a-board
;(define (find-open-spots a-board)
;  (local ((define qps (extract-qps a-board)))
;    (filter (lambda (spot)
;              (not (threatened? spot qps)))
;            (board-positions a-board))))

; Part 2
; A Board contains those positions where a queen has been
;   placed
;(define-struct board [size qps])
; A Board is a structure (make-board N [List-of QP])

; N -> Board
; creates an empty nxn board
;(define (board0 n) (make-board n empty))

; Board QP -> Board
; places a queen on a-board at position qp
;(define (add-queen a-board spot)
;  (make-board (board-size a-board)
;              (cons spot (board-qps a-board))))

; Board -> [List-of Posn]
; returns all unthreatened positions on a-board
;(define (find-open-spots a-board)
;  (local ((define n (board-size a-board)))
;    (filter (lambda (posn)
;              (not (threatened? posn (board-qps a-board))))
;            (for*/list ([i n] [j n])
;              (make-posn i j)))))


; Part 3
; A Board is a grid of nxn Boxes, each possibly occupied
;   by a queen
(define-struct board [size boxes])
; A Board is a structure: (make-board N [List-of Box])

(define-struct box [x y threatened?])
; A Box is a structure: (make-box N N Boolean)

; N -> Board
; creates an empty nxn board
(define (board0 n)
  (make-board n
              (for*/list ([i n] [j n])
                (make-box i j #false))))

; Box -> Posn
(define (box->posn sq) (make-posn (box-x sq) (box-y sq)))

; Board QP -> Board
; places a queen on a-board at position qp
(define (add-queen a-board qp)
  (make-board (board-size a-board)
              (map (lambda (p)
                     (make-box (box-x p)
                               (box-y p)
                               (or (box-threatened? p)
                                   (threatening? (box->posn p)
                                                 qp))))
                   (board-boxes a-board))))

; Board -> [List-of Posn]
; returns all unthreatened positions on a-board
(define (find-open-spots a-board)
  (map box->posn (filter (lambda (spot)
                           (not (box-threatened? spot)))
                         (board-boxes a-board))))




; N -> [Maybe [List-of QP]]
; finds a solution to the n-queens problem
(check-expect (n-queens 2) #false)
(check-expect (n-queens 3) #false)
(check-satisfied (n-queens 4)
                 (lambda (soln) (n-queens-solution? 4 soln)))
(define (n-queens n) (place-queens (board0 n) n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An independent solution to the n-queens problem

; N -> [Maybe [List-of QP]]
; finds a solution to the n-queens problem
(check-expect (n-queens-alt 2) #false)
(check-expect (n-queens-alt 3) #false)
(check-satisfied (n-queens-alt 4)
                 (lambda (soln) (n-queens-solution? 4 soln)))
(define (n-queens-alt n)
  (local (; possible column positions for a queen
          (define l (build-list n (lambda (i) i)))

          ; [List-of QP] -> [Maybe [List-of QP]]
          ; places the remaining queens on the board;
          ;   taking into account the threat posed by
          ;   queens in <placed>
          (define (place-remaining-queens placed)
            (local ((define r (length placed))) ; cur-row
              (if (= r n)
                  empty ; all queens already placed
                  (ormap-alt
                   (lambda (col)
                     (local ((define p (make-posn r col)))
                       (cond [(threatened? p placed) #false]
                             [else
                              (local ((define tmp
                                        (place-remaining-queens
                                         (cons p placed))))
                                (if (false? tmp)
                                    #false
                                    (cons p tmp)))])))
                   l)))))
    (place-remaining-queens empty)))