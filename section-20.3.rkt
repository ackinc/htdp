;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname section-20.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require htdp/dir)

; The <dir> library introduces these two struct definitions:
;(define-struct dir [name dirs files])
;(define-struct file [name size content])

; A data example
(define Code (make-dir 'code
                       empty
                       (list (make-file 'hang 8 "")
                             (make-file 'draw 2 ""))))
(define Docs (make-dir 'docs
                       empty
                       (list (make-file 'read! 19 ""))))
(define Libs (make-dir 'libs
                       (list Code Docs)
                       empty))
(define Text (make-dir 'text
                       empty
                       (list (make-file 'part1 99 "")
                             (make-file 'part2 52 "")
                             (make-file 'part3 17 ""))))
(define TS (make-dir 'ts
                     (list Text Libs)
                     (list (make-file 'read! 10 ""))))

; ex 338
(define DOWNLOADS (create-dir "/home/anirudh/Downloads"))
(define PICTURES (create-dir "/home/anirudh/Pictures"))
(define VIDEOS (create-dir "/home/anirudh/Videos"))

; Dir -> N
; counts the number of files in directory d
(check-expect (how-many DOWNLOADS) 9)
(check-expect (how-many PICTURES) 3)
(check-expect (how-many VIDEOS) 6)
(define (how-many d)
  (+ (length (dir-files d))
     (foldr + 0 (map how-many (dir-dirs d)))))

; ex 339
; String Dir -> Boolean
; returns true if a file with given name occurs in directory d
(check-expect (find? 'Function_Design_Recipe.png PICTURES)
              #true)
(check-expect (find? 'Function_Design_Recipe.png VIDEOS)
              #false)
(define (find? name d)
  (or (member? name (map file-name (dir-files d)))
      (ormap (lambda (d2) (find? name d2)) (dir-dirs d))))

; ex 340
; Dir -> [List-of String]
; lists the names of all files and subdirectories in given dir
(check-expect (length (ls PICTURES)) 3)
(check-expect (length (ls VIDEOS)) 2)
(define (ls d)
  (append (map file-name (dir-files d))
          (map dir-name (dir-dirs d))))

; ex 341
; Dir -> N
; computes the total size over all files in a directory tree
;   directories are assumed to have size equal to 1 unit
(check-expect (du Code) 11)
(check-expect (du Text) 169)
(define (du d)
  (+ 1
     (foldr + 0 (map file-size (dir-files d)))
     (foldr + 0 (map du (dir-dirs d)))))

; A Path is a [List-of String]
; interpretation: path of a file or directory, starting from
;   some other Directory

; ex 342
; returns path of a target file/subdir from given dir
;   or #false if target cannot be found in dir
; String Dir -> [Maybe Path]
(check-expect (find 'a Code) #false)
(check-expect (find 'code Code) (list 'code))
(check-expect (find 'part1 TS) (list 'ts 'text 'part1))
(check-expect (find 'code TS) (list 'ts 'libs 'code))
(define (find name d)
  (cond [(symbol=? name (dir-name d)) (list (dir-name d))]
        [(member? name (map file-name (dir-files d)))
         (list (dir-name d) name)]
        [(member? name (map dir-name (dir-dirs d)))
         (list (dir-name d) name)]
        [else (local ((define result
                        (for/or ((subdir (dir-dirs d)))
                          (find name subdir))))
                (if (false? result)
                    #false
                    (cons (dir-name d) result)))]))

; String Dir -> [List-of Path]
; returns paths to all files/subdirs with target name
;   in directory d
; result includes d itself, if its name is same as target name
(check-expect (find-all 'a Code) empty)
(check-expect (find-all 'part1 TS)
              (list (list 'ts 'text 'part1)))
(check-expect (find-all 'ts TS) (list (list 'ts)))
(check-expect (find-all 'read! TS)
              (list (list 'ts 'read!)
                    (list 'ts 'libs 'docs 'read!)))
(define (find-all name d)
  (append (if (symbol=? name (dir-name d))
              (list (list (dir-name d)))
              empty)
          (if (member? name (map file-name (dir-files d)))
              ; a dir cannot have two files with the same name
              (list (list (dir-name d) name))
              empty)
          (map (lambda (path) (cons (dir-name d) path))
               ; flat list of paths to target from subdirs
               (foldl append empty
                      (filter (lambda (paths)
                                (not (empty? paths)))
                              (map (lambda (sd)
                                     (find-all name sd))
                                   (dir-dirs d)))))))

; ex 343
; Dir -> [List-of Path]
; lists path to all files/subdirs *inside* given directory
(check-expect (ls-R Text) (list (list 'text 'part1)
                                (list 'text 'part2)
                                (list 'text 'part3)))
(check-satisfied (ls-R Libs) contains-all-files-in-lib?)
(define (ls-R d)
  (append (map (lambda (f) (list (dir-name d) f))
               (map file-name (dir-files d)))
          (map (lambda (sd) (list (dir-name d) sd))
               (map dir-name (dir-dirs d)))
          (map (lambda (path) (cons (dir-name d) path))
               (foldl append empty (map ls-R (dir-dirs d))))))

; checks if given list contains paths to all files/subdirs
;   in the "Lib" directory
(define (contains-all-files-in-lib? l)
  (and (= (length l) 5)
       (member? (list 'libs 'code) l)
       (member? (list 'libs 'code 'hang) l)
       (member? (list 'libs 'code 'draw) l)
       (member? (list 'libs 'docs) l)
       (member? (list 'libs 'docs 'read!) l)))

; ex 344
; redesign of find-all from ex 342 using ls-R
(check-expect (find-all.v2 'a Code) empty)
(check-expect (find-all.v2 'part1 TS)
              (list (list 'ts 'text 'part1)))
(check-expect (find-all.v2 'ts TS) (list (list 'ts)))
(check-expect (find-all.v2 'read! TS)
              (list (list 'ts 'read!)
                    (list 'ts 'libs 'docs 'read!)))
(define (find-all.v2 name d)
  (append (if (symbol=? (dir-name d) name)
              (list (list (dir-name d)))
              empty)
          (filter (lambda (path)
                    (symbol=? (last path) name))
                  (ls-R d))))

; returns the last element of list l
(define (last l)
  (if (empty? (rest l)) (first l) (last (rest l))))