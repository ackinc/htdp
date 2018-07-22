;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname section-32.4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/image)

(define FONT-SIZE 11)
(define FONT-COLOR "black")
 
; [List-of 1String] -> Image
; renders a string as an image for the editor 
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))
 
(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor [List-of 1String] [List-of 1String])
; interpretation if (make-editor p s) is the state of 
; an interactive editor, (reverse p) corresponds to
; the text to the left of the cursor and s to the
; text on the right


(define lla '("l" "l" "a"))
(define good '("g" "o" "o" "d"))
(define allgood (append (reverse lla) good))


; ex 508
; [List-of 1String] -> Number
; returns the width of the given string, rendered as text
(define (text-width s) (image-width (editor-text s)))


; [NEList-of Any] -> [List-of Any]
; returns all but the last element of a non-empty list
(check-expect (but-last '(1)) '())
(check-expect (but-last '(1 2)) '(1))
(check-expect (but-last '(1 2 3)) '(1 2))
(define (but-last l)
  (if (empty? (rest l))
      empty
      (cons (first l) (but-last (rest l)))))

; [NEList-of Any] -> Any
; returns the last element of a non-empty list
(check-expect (last '(1)) 1)
(check-expect (last '(1 2)) 2)
(define (last l)
  (if (empty? (rest l))
      (first l)
      (last (rest l))))


; Editor N -> Boolean
; checks whether the cursor in editor ed is in the correct
;   position after a mouse click at position x

(check-expect (check-editor (make-editor lla good)
                            allgood
                            (text-width (reverse lla)))
              #true)
(check-expect (check-editor (make-editor lla good)
                            allgood
                            (add1 (text-width
                                   (reverse lla))))
              #true)
(check-expect (check-editor (make-editor lla good)
                            allgood
                            (sub1 (text-width
                                   (reverse lla))))
              #false)

; click at extreme right
(check-expect (check-editor (make-editor lla empty)
                            '("a" "l" "l")
                            (+ (text-width (reverse lla))
                               (text-width good)))
              #true)

(define (check-editor ed text x)
  (local ((define p (reverse (editor-pre ed)))
          (define s (editor-post ed))
          (define p-plus-char-text
            (if (empty? s) p (append p (list (first s)))))
          (define p-width (text-width p))
          (define p-plus-char-width
            (text-width p-plus-char-text)))
    (and (equal? (append p s) text)
         (<= p-width x)
         (or (< x p-plus-char-width)
             (empty? s)))))


; [List-of 1String] N -> Editor
; splits ed based on a simulated mouse-click

; click at beginning of text
(check-satisfied (split-structural allgood 0)
                 (lambda (e) (check-editor e allgood 0)))

; click in middle of text, right-edge of character
(check-satisfied (split-structural allgood
                                   (text-width (reverse lla)))
                 (lambda (e) (check-editor e allgood (text-width (reverse lla)))))

; click in middle of text, on character
(check-satisfied (split-structural allgood
                                   (sub1 (text-width (reverse lla))))
                 (lambda (e) (check-editor e allgood (sub1 (text-width (reverse lla))))))

; click at end of text
(check-satisfied (split-structural allgood
                                   (text-width allgood))
                 (lambda (e) (check-editor e allgood (text-width allgood))))
(check-satisfied (split-structural allgood
                                   (* 2 (text-width allgood)))
                 (lambda (e) (check-editor e allgood (* 2 (text-width allgood)))))

(define (split-structural ed x)
  (if (<= (text-width ed) x)
      (make-editor (reverse ed) empty)
      (add-as-last-char (split-structural (but-last ed) x)
                        (last ed))))

; Editor 1String -> Editor
; adds s as the last char in editor ed, leaving
;   cursor position unchanged
(check-expect (add-as-last-char (make-editor empty empty) "s")
              (make-editor empty '("s")))
(check-expect (add-as-last-char (make-editor empty '("s")) "g")
              (make-editor empty '("s" "g")))
(check-expect (add-as-last-char (make-editor '("a") '("s")) "g")
              (make-editor '("a") '("s" "g")))
(define (add-as-last-char ed s)
  (make-editor (editor-pre ed)
               (append (editor-post ed) (list s))))


; ex 509
; [List-of 1String] N -> Editor
; re-implementation of split-structural using an accumulator

; click at beginning of text
(check-satisfied (split allgood 0)
                 (lambda (e) (check-editor e allgood 0)))

; click in middle of text, right-edge of character
(check-satisfied (split allgood (text-width (reverse lla)))
                 (lambda (e) (check-editor e allgood (text-width (reverse lla)))))

; click in middle of text, on character
(check-satisfied (split allgood
                        (sub1 (text-width (reverse lla))))
                 (lambda (e) (check-editor e allgood (sub1 (text-width (reverse lla))))))

; click at end of text
(check-satisfied (split allgood (text-width allgood))
                 (lambda (e) (check-editor e allgood (text-width allgood))))
(check-satisfied (split allgood (* 2 (text-width allgood)))
                 (lambda (e) (check-editor e allgood (* 2 (text-width allgood)))))

(define (split ed0 x)
  (local (; [List-of 1String] [List-of 1String] -> Editor
          ; accumulator pre: 1Strings that should be rendered
          ;   before the cursor in the final editor
          (define (split/a ed pre)
            (if (or (empty? ed)
                    (> (text-width (cons (first ed) pre)) x))
                (make-editor pre ed)
                (split/a (rest ed) (cons (first ed) pre)))))
    (split/a ed0 empty)))


; ex 510

; A Word is a String
; A Line is a [List-of Word]

; Line 1String -> String
; turns the words in a line into a single string, using the
;   given separator
(check-expect (line->string empty " ") "")
(check-expect (line->string '("ani") " ") "ani")
(check-expect (line->string '("ani" "is") " ") "ani is")
(check-expect (line->string '("ani" "is" "alive") " ") "ani is alive")
(define (line->string line sep)
  (if (empty? line)
      ""
      (foldl (lambda (s acc) (string-append acc sep s))
             (first line)
             (rest line))))

; N String String -> String
; reads and parses the contents of in-f into lines of width
;   <= w (not including \n), then writes the result into out-f
(define (fmt w in-f out-f)
  (local (; Line Line N -> Line
          ; build a line of maximal width <= w from the first
          ;   words of l
          ; accumulator result: the line containing taken words
          ; accumulator len: length of result after conversion
          ;   to string
          (define (take/a l result len)
            (if (or (empty? l)
                    (> (+ len (string-length (first l)) 1) w)) ; + 1 for eventual space
                (reverse result)
                (take/a (rest l)
                        (cons (first l) result)
                        (+ len
                           (string-length (first l))
                           1))))

          ; Line N -> Line
          ; drops words from l until text of width just <= w
          ;   has been dropped
          ; accumulator acc: width of text that has been
          ;   dropped
          (define (drop/a l acc)
            (if (or (empty? l)
                    (> (+ acc (string-length (first l)) 1) w))
                l
                (drop/a (rest l) (+ acc
                                    (string-length (first l))
                                    1))))

          ; [List-of Line] [List-of Line] -> [List-of Line]
          ; modifies the lines in contents to have max width w
          ; accumulator acc: lines that were read from in-f,
          ;   but are not in contents, modified to have
          ;   width <= w, and in the opposite order
          ;   that they were read
          (define (fmt/a contents acc)
            (if (empty? contents)
                acc
                (local ((define line (first contents)))
                  (if (<= (string-length (line->string line " ")) w)
                      (fmt/a (rest contents) (cons line acc))
                      (fmt/a (cons (drop/a (rest line)
                                           (string-length (first line)))
                                   (rest contents))
                             ; dropping/taking at least one word ensures
                             ;   we don't loop indefinitely when a
                             ;   huge word is encountered
                             (cons (take/a (rest line)
                                           (list (first line))
                                           (string-length (first line)))
                                   acc)))))))
    (write-file
     out-f
     (line->string (reverse (map (lambda (line)
                                   (line->string line " "))
                                 (fmt/a (read-words/line in-f)
                                        empty)))
                   "\n"))))

(fmt 80 "test-in.txt" "test-out.txt")