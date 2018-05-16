;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-330) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A File.v1 is a String

; A Dir.v1 is one of:
; - '()
; - (cons File.v1 Dir.v1)
; - (cons Dir.v1 Dir.v1)

; Data Example:
(define Code (cons "hang" (cons "draw" '())))
(define Docs (cons "read!" '()))
(define Libs (cons Code (cons Docs '())))
(define Text (cons "part1" (cons "part2" (cons "part3" '()))))
(define TS (cons Text (cons "read!" (cons Libs '()))))

; Libs could also be represented as (cons Code Docs),
;   according to the data definition. But the result
;   would not read as expected:

;   - (list (list "hang" "draw") "read!"),
;     making it look like Libs contains one Dir and one File

;   instead of

;   - (list (list "hang" "draw") (list "read!")),
;     which makes it clear that Libs contains two Dirs

; This ambiguity is an indication that the data definition
;   can be improved!

; ex 331
; Dir.v1 -> N
; counts the number of files in dir
(check-expect (how-many Code) 2)
(check-expect (how-many TS) 7)
(define (how-many dir)
  (cond [(empty? dir) 0]
        [(string? (first dir))
         (add1 (how-many (rest dir)))]
        [else (+ (how-many (first dir))
                 (how-many (rest dir)))]))



; Dir.v1 ignores directory names, so we need a better
;   data definition

; A Dir.v2 is a structure: (make-dir String LOFD)
(define-struct dir [name content])

; A LOFD (list-of-files-and-directories) is one of:
; - '()
; - (cons File.v2 Dir.v2)
; - (cons Dir.v2 Dir.v2)

; A File.v2 is a String

; ex 332
; Data Examples using Dir.v2
(define Code.v2 (make-dir "code"
                          (cons "hang"
                                (cons "draw" '()))))
(define Docs.v2 (make-dir "docs"
                          (cons "read!" '())))
(define Libs.v2 (make-dir "libs"
                          (cons Code.v2
                                (cons Docs.v2 '()))))
(define Text.v2 (make-dir "text"
                          (cons "part1"
                                (cons "part2"
                                      (cons "part3" '())))))
(define TS.v2 (make-dir "TS"
                        (cons Text.v2
                              (cons "read!"
                                    (cons Libs.v2 '())))))

; ex 333
; Dir.v2 -> N
; counts the number of files in directory d
(check-expect (how-many.v2 Code.v2) 2)
(check-expect (how-many.v2 TS.v2) 7)
(define (how-many.v2 d)
  (cond [(empty? (dir-content d)) 0]
        [(string? (first (dir-content d)))
         (add1 (how-many.v2
                (make-dir (dir-name d)
                          (rest (dir-content d)))))]
        [else (+ (how-many.v2 (first (dir-content d)))
                 (how-many.v2
                  (make-dir (dir-name d)
                            (rest (dir-content d)))))]))


; ex 334
(define-struct dir.ex334 [name content size open?])
; A Dir.ex334 is a structure:
;   (make-dir.ex334 String LOFD N Boolean)

; The above definition for File does not support
;   file attributes, so we introduce the new definition below:
(define-struct file [name size content])
; A File is a structure: (make-file String N String)

; Instead of listing files and subdirectories together,
;   we can split them into two lists
(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure: (make-dir.v3 String Dir* File*)
; A Dir* is a [List-of Dir.v3]
; A File* is a [List-of File]

; ex 335
; Data Examples using Dir.v3
(define Code.v3 (make-dir.v3 "code"
                             empty
                             (list (make-file "hang" 8 "")
                                   (make-file "draw" 2 ""))))
(define Docs.v3 (make-dir.v3 "docs"
                             empty
                             (list (make-file "read!" 19 ""))))
(define Libs.v3 (make-dir.v3 "libs"
                             (list Code.v3 Docs.v3)
                             empty))
(define Text.v3 (make-dir.v3 "text"
                             empty
                             (list (make-file "part1" 99 "")
                                   (make-file "part2" 52 "")
                                   (make-file "part3" 17 ""))))
(define TS.v3 (make-dir.v3 "TS"
                           (list Text.v3 Libs.v3)
                           (list (make-file "read!" 10 ""))))

; ex 336/337
; Dir.v3 -> N
; counts the number of files in directory d
(check-expect (how-many.v3 Code.v3) 2)
(check-expect (how-many.v3 TS.v3) 7)
(define (how-many.v3 d)
  (+ (length (dir.v3-files d))
     (foldr + 0 (map how-many.v3 (dir.v3-dirs d)))))