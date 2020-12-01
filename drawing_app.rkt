; Use (draw-app IS) to run

(require 2htdp/image)
(require 2htdp/universe)

; Constants for now
(define S-SIZE 10)
(define S-COLOR "black")
(define INITIAL-CANVAS (rectangle 600 600 "solid" "white"))

; Tool Structure
; where type: String
;             determines the events linked to the tool
;       size: Number
;       color: Structure (make-color)
;       start: Maybe<posn>
;       status: Boolean
;               is the tool active?
(define-struct tool [type size color start status])

; ====================================================================================
; Line Tool
; [!] could use some abstraction [!]

; Begin line
(define (line-start a x y)
  (make-appstate
   (appstate-canvas a)
   (appstate-pre a)
   (appstate-post a)
   (make-tool
    (tool-type (appstate-tool a))
    (tool-size (appstate-tool a))
    (tool-color (appstate-tool a))
    (make-posn x y) ;start
    #true)
    #false))

; Move end point
; [!] line doesn't appear until button is released [!]
(define (line-drag a x y)
  (cond [(tool-status (appstate-tool a)) a]
        [else
         (make-appstate
          (line-draw a x y)
          (appstate-pre a)
          (appstate-post a)
          (appstate-tool a)
          #false)]))

; Aux
(define (line-draw a x y)
  (add-line
   (appstate-canvas a)
   (posn-x (tool-start (appstate-tool a)))
   (posn-y (tool-start (appstate-tool a)))
   x y
   S-COLOR))

; Place on canvas
(define (line-place a x y)
  (make-appstate
   (line-draw a x y)
   (cons (appstate-canvas a)
         (appstate-pre a))
   '()
   (make-tool
    (tool-type (appstate-tool a))
    (tool-size (appstate-tool a))
    (tool-color (appstate-tool a))
    #false
    #false)
   #false))

; -----------------------------------------------------------------------------------

; Line Actions
; line-a : AppState Number Number String -> [X -> Y]
; performs the corresponding line action depending on the mouse event

(define (line-a a x y me)
    (cond [(string=? me "button-down")
           (line-start a x y)]
          [(string=? me "drag")
           (line-drag a x y)]
          [(string=? me "button-up")
           (line-place a x y)]
          [else a]))

; Premade Tool: Line
(define LINE
  (make-tool
   "line"
   S-SIZE
   S-COLOR
   #false
   #false))

; ====================================================================================

; AppState definition
(define-struct appstate [canvas pre post tool quit])

; Initial State
(define IS
  (make-appstate
   INITIAL-CANVAS
   '()
   '()
   LINE
   #false))

; ====================================================================================
; Mouse Handler

(define (handle-mouse a x y me)
  (local ((define type (tool-type (appstate-tool a))))
    (cond [(string=? type "line")
           (line-a a x y me)]
          [else a])))

; ====================================================================================
; Render Function

(define (render a)
  (appstate-canvas a))

; ====================================================================================
; Big-Bang

(define (draw-app is)
  (big-bang is
    [to-draw render]
    [on-mouse handle-mouse]))
