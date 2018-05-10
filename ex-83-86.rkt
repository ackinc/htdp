;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-83-86) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 200)
(define HEIGHT 20)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 20 "solid" "red"))
(define TEXT-SIZE 16)

; An Editor is a structure:
; (make-editor pre post)
; pre - STRING - characters that should appear before the cursor
; post - STRING - characters that should appear after the cursor
(define-struct editor [pre post])

; Editor -> Image
; draws the contents of given editor, with cursor in appropriate place
(define (draw-text e)
  (beside (text (editor-pre e) TEXT-SIZE "black")
          CURSOR
          (text (editor-post e) TEXT-SIZE "black")))

; Editor -> Image
; draws the contents of given editor, with cursor in appropriate place, on a background
(define (render e) (overlay/align "left" "middle" (draw-text e) BACKGROUND))

; String -> String
; returns first char of supplied string
; or empty string if empty string supplied
(check-expect (string-first "") "")
(check-expect (string-first "B") "B")
(check-expect (string-first "Ani") "A")
(define (string-first str)
  (if (= (string-length str) 0) "" (substring str 0 1)))

; String -> String
; returns last char of supplied string
; or empty string if empty string supplied
(check-expect (string-last "") "")
(check-expect (string-last "B") "B")
(check-expect (string-last "Ani") "i")
(define (string-last str)
  (if (= (string-length str) 0) "" (substring str (- (string-length str) 1))))

; String -> String
; returns all but first char of supplied string
; or empty string if empty string supplied
(check-expect (string-remove-first "") "")
(check-expect (string-remove-first "B") "")
(check-expect (string-remove-first "Ani") "ni")
(define (string-remove-first str)
  (if (= (string-length str) 0) "" (substring str 1)))

; String -> String
; returns all but last char of supplied string
; or empty string if empty string supplied
(check-expect (string-remove-last "") "")
(check-expect (string-remove-last "B") "")
(check-expect (string-remove-last "Ani") "An")
(define (string-remove-last str)
  (if (= (string-length str) 0) "" (substring str 0 (- (string-length str) 1))))

; Editor Editor -> Editor
; returns first editor if its contents will fit in scene
; else returns second editor
(define (choose e1 e2)
  (if (< (image-width (draw-text e1)) WIDTH) e1 e2))

; Editor -> Editor
; reacts to keypresses; ignores TAB and RETURN
; tests - irrelevant keypresses
(check-expect (edit (make-editor "Ani" "udh") "up") (make-editor "Ani" "udh"))
(check-expect (edit (make-editor "Ani" "udh") "down") (make-editor "Ani" "udh"))
(check-expect (edit (make-editor "Anir" "udh") "\t") (make-editor "Anir" "udh"))
(check-expect (edit (make-editor "Anir" "udh") "\n") (make-editor "Anir" "udh"))
; tests - character insertion
(check-expect (edit (make-editor "" "") "a") (make-editor "a" ""))
(check-expect (edit (make-editor "" "b") "a") (make-editor "a" "b"))
(check-expect (edit (make-editor "Ani" "udh") "r") (make-editor "Anir" "udh"))
(check-expect (edit (make-editor "Anir" "udh") " ") (make-editor "Anir " "udh"))
; tests - backspace
(check-expect (edit (make-editor "" "") "\b") (make-editor "" ""))
(check-expect (edit (make-editor "" "a") "\b") (make-editor "" "a"))
(check-expect (edit (make-editor "Anir" "udh") "\b") (make-editor "Ani" "udh"))
; tests - cursor moving left
(check-expect (edit (make-editor "" "") "left") (make-editor "" ""))
(check-expect (edit (make-editor "" "a") "left") (make-editor "" "a"))
(check-expect (edit (make-editor "a" "") "left") (make-editor "" "a"))
(check-expect (edit (make-editor "Anir" "udh") "left") (make-editor "Ani" "rudh"))
; tests - cursor moving right
(check-expect (edit (make-editor "" "") "right") (make-editor "" ""))
(check-expect (edit (make-editor "a" "") "right") (make-editor "a" ""))
(check-expect (edit (make-editor "" "a") "right") (make-editor "a" ""))
(check-expect (edit (make-editor "Ani" "rudh") "right") (make-editor "Anir" "udh"))
(define (edit e ke)
  (cond [(string=? ke "left") (make-editor (string-remove-last (editor-pre e))
                                           (string-append (string-last (editor-pre e))
                                                          (editor-post e)))]
        [(string=? ke "right") (make-editor (string-append (editor-pre e)
                                                           (string-first (editor-post e)))
                                            (string-remove-first (editor-post e)))]
        [(string=? ke "\b") (make-editor (string-remove-last (editor-pre e)) (editor-post e))]
        [(and (= (string-length ke) 1)
              (not (or (string=? ke "\t")
                       (string=? ke "\n"))))
         (choose (make-editor (string-append (editor-pre e) ke) (editor-post e)) e)]
        [else e]))

(define (run pre)
  (big-bang (make-editor pre "")
            [on-key edit]
            [to-draw render]))
(run "")