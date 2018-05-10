;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname section-12.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)


; An N is one of:
; - 0
; - (add1 N)

; Any -> Boolean
; returns true if x is an N, false otherwise
(check-expect (N? 0) #true)
(check-expect (N? 1) #true)
(check-expect (N? -1) #false)
(check-expect (N? "a") #false)
(check-expect (N? empty) #false)
(define (N? x) (and (integer? x) (not (negative? x))))



;; Representing itunes tracks as structures

; The definition below is provided by the 2htdp/itunes library

;(define-struct date [year month day hour minute second])
; (make-date N N N N N N)
; month -> an N in [1, 12]
; day -> an N in [1, 31]
; hour -> an N in [0, 23]
; minute/second -> Ns in [0, 59]

; The function below is provided by the 2htdp/itunes library
; Implementation below is for reference only

; Any Any Any Any Any Any -> Date or #false
; returns a date for legit inputs, false otherwise
;(define (create-date year month day hour minute second)
;  (if (and (N? year)
;           (and (N? month) (>= 1 month 12))
;           (and (N? day) (>= 1 day 31))
;           (and (N? hour) (>= 0 hour 23))
;           (and (N? minute) (>= 0 minute 59))
;           (and (N? second) (>= 0 second 59)))
;      (make-date year month day hour minute second)
;      #false))

; Examples:
(define example-date-1 (create-date 2000 12 25 13 30 0))
(define example-date-2 (create-date 2007 5 2 18 30 56))
(define example-date-3 (create-date 2017 9 30 8 14 26))
(define example-date-4 (create-date 2018 6 15 23 30 58))
(define example-date-5 (create-date 207 5 2 18 30 56))


; The definition below is provided by the 2htdp/itunes library

;(define-struct track [name artist album time track# added play# played])
; (make-track String String String N N Date N Date)
; time -> length of track in seconds
; track# -> position of track in album
; play# -> # of times track has been played
; played -> track's last-play-time
; other fields are self-explanatory

; The function below is provided by the 2htdp/itunes library
; Implementation is for reference only

; Any Any Any Any Any Any Any -> Track or #false
; returns a track if legit inputs, else returns false
;(define (create-track name artist album time
;                      track# added play# played)
;  (if (and (string? name)
;           (string? artist)
;           (string? album)
;           (N? time)
;           (N? track#)
;           (date? added)
;           (N? play#)
;           (date? played))
;      (make-track name artist album time
;                  track# added play# played)
;      #false))

; Examples:
(define example-track-1
  (create-track "Believer" "Imagine Dragons" "Unknown" 220
                3 example-date-1 14 example-date-3))
(define example-track-2
  (create-track "Radioactive" "Imagine Dragons" "Radioactive" 245
                1 example-date-2 13 example-date-4))
(define example-track-3
  (create-track "Monster" "Imagine Dragons" "Unknown" 250
                2 example-date-3 11 example-date-4))


; An LTracks is one of:
; - empty
; - (cons Track LTracks)

; Examples:
(define example-ltracks-1 (list example-track-1 example-track-2))
(define example-ltracks-2 (list example-track-1 example-track-2 example-track-3))


; Uncomment when an itunes export XML file has been procured
;(define ITUNES-LOCATION "itunes.xml")
;(define itunes-tracks (read-itunes-as-tracks ITUNES-LOCATION))


; ex 200
; LTracks -> N
; returns the total play time of a list of tracks
(check-expect (total-time empty) 0)
(check-expect (total-time example-ltracks-1) 465)
(define (total-time lt)
  (if (empty? lt)
      0
      (+ (track-time (first lt))
         (total-time (rest lt)))))


; ex 201
; LTracks -> List-of-strings
; returns a list of album titles present in track list
(check-expect (select-all-album-titles empty) empty)
(check-expect (select-all-album-titles example-ltracks-1) (list "Unknown" "Radioactive"))
(check-expect (select-all-album-titles example-ltracks-2) (list "Unknown" "Radioactive" "Unknown"))
(define (select-all-album-titles lt)
  (cond [(empty? lt) empty]
        [else (cons (track-album (first lt))
                    (select-all-album-titles (rest lt)))]))

; List-of-strings -> List-of-strings
; removes duplicate strings from given list
(check-expect (create-set empty) empty)
(check-expect (create-set (list "a" "b")) (list "a" "b"))
(check-expect (create-set (list "a" "b" "a")) (list "b" "a"))
(define (create-set ls)
  (cond [(empty? ls) empty]
        [(member? (first ls) (rest ls)) (create-set (rest ls))]
        [else (cons (first ls) (create-set (rest ls)))]))

; LTracks -> List-of-strings
; returns a de-duplicated list of album titles present in track list
(check-expect (select-all-album-titles/unique empty) empty)
(check-expect (select-all-album-titles/unique example-ltracks-1) (list "Unknown" "Radioactive"))
(check-expect (select-all-album-titles/unique example-ltracks-2) (list "Radioactive" "Unknown"))
(define (select-all-album-titles/unique lt)
  (create-set (select-all-album-titles lt)))


; ex 202
; String LTracks -> LTracks
; selects tracks from given list that belong to an album
(check-expect (select-album "Unknown" empty) empty)
(check-expect (select-album "Unknown" example-ltracks-2) (list example-track-1 example-track-3))
(define (select-album album lt)
  (cond [(empty? lt) empty]
        [(string=? (track-album (first lt)) album)
         (cons (first lt) (select-album album (rest lt)))]
        [else (select-album album (rest lt))]))


; ex 203
; String Date LTracks -> LTracks
; select tracks belonging to an album that have been
;   played after specified date
(check-expect (select-album-date "Unknown" example-date-1 empty) empty)
(check-expect (select-album-date "Unknown" example-date-4 example-ltracks-2)
              empty)
(check-expect (select-album-date "Rocknrolla" example-date-1 example-ltracks-2)
              empty)
(check-expect (select-album-date "Unknown" example-date-2 example-ltracks-2)
              (list example-track-1 example-track-3))
(define (select-album-date album dt lt)
  (cond [(empty? lt) empty]
        [(and (string=? (track-album (first lt)) album)
              (date>? (track-played (first lt)) dt))
         (cons (first lt) (select-album-date album dt (rest lt)))]
        [else (select-album-date album dt (rest lt))]))

; Date Date -> Boolean
; checks if first date comes after the second date
(check-expect (date>? example-date-1 example-date-2) #false)
(check-expect (date>? example-date-2 example-date-2) #false)
(check-expect (date>? example-date-2 example-date-1) #true)
(define (date>? dt1 dt2)
  (> (string->number (date->string dt1))
     (string->number (date->string dt2))))

; Date -> String
; converts date to standardized string representation
;   where year is represented using 4 digits and
;   all other fields are represented using two digits
(check-expect (date->string example-date-1) "20001225133000")
(check-expect (date->string example-date-2) "20070502183056")
(check-expect (date->string example-date-5) "02070502183056")
(define (date->string dt)
  (string-append (left-pad (number->string (date-year dt)) 4 "0")
                 (left-pad (number->string (date-month dt)) 2 "0")
                 (left-pad (number->string (date-day dt)) 2 "0")
                 (left-pad (number->string (date-hour dt)) 2 "0")
                 (left-pad (number->string (date-minute dt)) 2 "0")
                 (left-pad (number->string (date-second dt)) 2 "0")))

; String N 1String -> String
; prefixes s with c until its length is at least n
(check-expect (left-pad "anirudh" 10 "a") "aaaanirudh")
(check-expect (left-pad "2" 2 "0") "02")
(check-expect (left-pad "31" 2 "0") "31")
(define (left-pad s n c)
  (if (>= (string-length s) n)
      s
      (left-pad (string-append c s) n c)))


; ex 204
; LTracks -> List-of-LTracks
; splits a LTracks into several LTracks, by album
(check-expect (select-albums empty) empty)
(check-expect (select-albums example-ltracks-2)
              (list (list example-track-2)
                    (list example-track-1 example-track-3)))
(define (select-albums lt)
  (select-tracks-by-albums (select-all-album-titles/unique lt) lt))

; List-of-strings LTracks -> List-of-LTracks
; returns a list containing one LTrack for each album specified in list
(check-expect (select-tracks-by-albums empty empty) empty)
(check-expect (select-tracks-by-albums empty example-ltracks-2) empty)
(check-expect (select-tracks-by-albums (list "Unknown") empty) (list empty))
(check-expect (select-tracks-by-albums (list "Lost And Found") example-ltracks-2) (list empty))
(check-expect (select-tracks-by-albums (list "Lost And Found" "Unknown" "Radioactive") example-ltracks-2)
              (list empty
                    (list example-track-1 example-track-3)
                    (list example-track-2)))
(define (select-tracks-by-albums albums lt)
  (cond [(empty? albums) empty]
        [else (cons (select-album (first albums) lt)
                    (select-tracks-by-albums (rest albums) lt))]))



;; Representing itunes tracks as lists

; An LLists is one of:
; – '()
; – (cons LAssoc LLists)

; An LAssoc is one of: 
; – '()
; – (cons Association LAssoc)

; An Association is a list of two items: 
;   (cons String (cons BSDN '()))

; A BSDN is one of: 
; – Boolean
; – Number
; – String
; – Date


; ex 205
(define example-lassoc-1
  (list (list "name" "Radioactive")
        (list "album" "Radioactive")
        (list "artist" "Imagine Dragons")
        (list "track#" 1)
        (list "time" 220)))
(define example-lassoc-2
  (list (list "name" "Monster")
        (list "album" "Unknown")
        (list "track#" 2)
        (list "added" example-date-3)))
(define example-lassoc-3
  (list (list "name" "Monstrous")
        (list "album" "Unknown")
        (list "track#" 3)
        (list "added" example-date-3)
        (list "time" 255)
        (list "good?" #true)))
(define example-lassoc-4
  (list (list "name" "Even More Monstrous")
        (list "album" "Unknown")
        (list "track#" 4)
        (list "added" example-date-4)
        (list "time" 400)
        (list "good?" #false)
        (list "bad?" #true)))
(define example-lassoc-5
  (list (list "name" "Abra")
        (list "album" "Kadabra")
        (list "artist" "Alakazam")
        (list "track#" 1)
        (list "added" example-date-3)
        (list "time" 400)
        (list "play#" 4)
        (list "played" example-date-4)
        (list "good?" #true)))
(define example-llists-1 (list example-lassoc-1
                               example-lassoc-2))
(define example-llists-2 (list example-lassoc-1
                               example-lassoc-2
                               example-lassoc-3
                               example-lassoc-4))


; ex 206
; returns first Association in list whose first item is equal
;   to key, or default if no such Association is found
(check-expect (find-assoc "album" example-lassoc-1 #false)
              (list "album" "Radioactive"))
(check-expect (find-assoc "albumin" example-lassoc-1 #false)
              #false)
(define (find-assoc key la default)
  (cond [(empty? la) default]
        [(string=? (first (first la)) key) (first la)]
        [else (find-assoc key (rest la) default)]))


; ex 207
; returns total play time of tracks in list
(check-expect (total-time/list empty) 0)
(check-expect (total-time/list example-llists-1) 220)
(check-expect (total-time/list example-llists-2) 875)
(define (total-time/list ll)
  (cond [(empty? ll) 0]
        [else (+ (second (find-assoc "time" (first ll) (list "time" 0)))
                 (total-time/list (rest ll)))]))


; ex 208
; LLists -> List-of-strings
; returns the list of boolean attributes tracks can have
(check-expect (boolean-attributes example-llists-1) empty)
(check-expect (boolean-attributes example-llists-2) (list "good?" "bad?"))
(define (boolean-attributes ll)
  (cond [(empty? ll) empty]
        [else (create-set (append (boolean-attributes-of-lassoc (first ll))
                                  (boolean-attributes (rest ll))))]))


; LAssoc -> List-of-strings
; returns the boolean attrs of an lassoc
(check-expect (boolean-attributes-of-lassoc example-lassoc-1) empty)
(check-expect (boolean-attributes-of-lassoc example-lassoc-3) (list "good?"))
(check-expect (boolean-attributes-of-lassoc example-lassoc-4) (list "good?" "bad?"))
(define (boolean-attributes-of-lassoc la)
  (cond [(empty? la) empty]
        [(boolean? (second (first la)))
         (cons (first (first la))
               (boolean-attributes-of-lassoc (rest la)))]
        [else (boolean-attributes-of-lassoc (rest la))]))

; the above definition of boolean-attributes is inefficient
;   because create-set is called once for every lassoc in ll
; a better way would be to "flatten" ll first, then call
;   boolean-attributes-of-lassoc, and finally, call create-set

; LLists -> LAssoc
; converts an llist into one large lassoc, with repeated keys
(check-expect (flatten example-llists-1)
              (list (list "name" "Radioactive")
                    (list "album" "Radioactive")
                    (list "artist" "Imagine Dragons")
                    (list "track#" 1)
                    (list "time" 220)
                    (list "name" "Monster")
                    (list "album" "Unknown")
                    (list "track#" 2)
                    (list "added" example-date-3)))
(define (flatten ll)
  (cond [(empty? ll) empty]
        [else (append (first ll) (flatten (rest ll)))]))


; LAssoc -> Track or #false
; converts a LAssoc to a Track if all the required info is present
; returns #false otherwise
(check-expect (lassoc->track example-lassoc-1) #false)
(check-expect (lassoc->track example-lassoc-5)
              (create-track "Abra" "Alakazam" "Kadabra" 400
                            1 example-date-3 4 example-date-4))
(define (lassoc->track la)
  (if (or (false? (assoc "name" la))
          (false? (assoc "artist" la))
          (false? (assoc "album" la))
          (false? (assoc "time" la))
          (false? (assoc "track#" la))
          (false? (assoc "added" la))
          (false? (assoc "play#" la))
          (false? (assoc "played" la)))
      #false
      (create-track (second (assoc "name" la))
                    (second (assoc "artist" la))
                    (second (assoc "album" la))
                    (second (assoc "time" la))
                    (second (assoc "track#" la))
                    (second (assoc "added" la))
                    (second (assoc "play#" la))
                    (second (assoc "played" la)))))