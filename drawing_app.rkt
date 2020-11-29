;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname drawing_app) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct tool [type size color x0 y0])

(define-struct appstate [canvas pre post tool quit])

(define START-CANVAS (rectangle 600 600 "solid" "white"))

(define SS
  (make-appstate
   START-CANVAS
   '()
   '()
   (make-tool "line" 10 "red" 0 0)
   #false))

; Getting tool values

(define (s.tool a)
  (appstate-tool a))

(define (t-type a)
  (tool-type (s.tool a)))

(define (t-size a)
  (tool-size (s.tool a)))

(define (t-color a)
  (tool-color (s.tool a)))

; ====================================================================================
; Rendering

(define (render a)
  (overlay (above (text (number->string (tool-x0 (appstate-tool a))) 24 "black")
                  (text (number->string (tool-y0 (appstate-tool a))) 24 "black"))
           (appstate-canvas a)))

; ====================================================================================
; Line Tool

(define (start-line a x y)
  (make-appstate
   (add-line
    (appstate-canvas a)
    x y x y
    (t-color a))
   (appstate-pre a)
   (appstate-post a)
   (make-tool "line" (t-size a) (t-color a) x y)
   #false))

(define (move-end a x y)
  (make-appstate
   (add-line
    (appstate-canvas a)
    (tool-x0 (s.tool a)) (tool-y0 (s.tool a)) x y
    (t-color a))
    (appstate-pre a)
    (appstate-post a)
    (s.tool a)
    #false))

(define (add-to-canvas a x y)
  (make-appstate
   (add-line
    (appstate-canvas a)
    (tool-x0 (s.tool a)) (tool-y0 (s.tool a)) x y
    (t-color a))
   (cons (appstate-pre a)
         (appstate-canvas a))
   '()
   (make-tool "line" (t-size a) (t-color a) x y)
   #false))

; Line Handler
(define (handle-line a event x y)
  (cond [(string=? event "button-down") (start-line a x y)]
        [(string=? event "drag") (move-end a x y)]
        [(string=? event "button-up") (add-to-canvas a x y)]))

; ====================================================================================
; Key Handler

; ====================================================================================
; Mouse Handler

(define (handle a type event x y)
  (cond [(string=? type "line") (handle-line a event x y)]
        [else a]))

(define (handle-mouse a x y me)
  (cond [(mouse=? me "button-down")
         (handle a (t-type a) "button-down" x y)]
        ;[(mouse=? me "wheel-up")
         ;(+ (t-size a) 1)]
        ;[(mouse=? me "wheel-down")
         ;(- (t-size a) 1)]
        [else a]))

; ====================================================================================
; Final Program

(define (drawing-app initial-state)
  (big-bang initial-state
    [to-draw render]
    [on-mouse handle-mouse]))
    ;[on-key handle-key]
    ;[stop-when quit?]))
  