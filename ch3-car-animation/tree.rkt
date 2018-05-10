;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tree) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define TREE-WIDTH 15)
(define TREE-HEIGHT (* 4 TREE-WIDTH))

(define TREE-TOP (circle TREE-WIDTH "solid" "green"))
(define TRUNK (rectangle TREE-WIDTH TREE-HEIGHT "solid" "brown"))

(define TREE (overlay/align/offset "middle" "top"
                                   TREE-TOP
                                   0 (/ TREE-WIDTH 2)
                                   TRUNK))