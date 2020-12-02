;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |base for RacketPainter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Libraries
(require 2htdp/universe)
(require 2htdp/image)

; RacketPainter app is one of:
; - An Image of the drawing canvas
; - the current Line being drawn
; - A boolean value 
; interpretation: states of the RacketPainter application

; The background in which we draw
(define INITIAL-CANVAS (rectangle 600 600 "solid" "white"))

; A Line is a strucutre (make-line x y x1 y1) where:
; x1, y1, x2 and y2 are Numbers.
; Interpretation: a segment that has an initial point(x,y) and an end point(x1,y1).
(define-struct Line [x y x1 y1 color])
; Examples of data:
(define LINE1 (make-Line 0 0 0 0 "black"))
(define LINE2 (make-Line 10 10 30 30 "yellow"))
(define LINE3 (make-Line 0 12 21 30 "green"))

; An AppState is a (make-AppState Image Maybe<Line>)
; Interpretation: state of the drawing application.
(define-struct State [Image Line quit])

; Example of data:
(define State1 (make-AppState INITIAL-CANVAS LINE1 #false))
(define State2 (make-AppState INITIAL-CANVAS LINE2 #false))

; quit is a Boolean.
; Interpretation: it represent whether the application has quit or not.

; ====================================================================================