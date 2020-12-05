;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Figures_and_Rendering) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define EMPTY-CANVAS (empty-scene 500 500))

;; Data types

; a Tool is a structure (make-tool type size color mode x1 y1 x2 y2 status)
; where type : String
;       size : Number
;       color : Color ;(in 2htdp/image there's already a struct for color, we could use that)
;       mode : String or Number
;              represents the mode for the figures ("outline", "solid", ...)        
;              (it can also be a number from 0 to 255 that indicates its transparency, with 0 being fully transparent and 255 being "solid")
;       x1, y1, x2, y2 : Number
;                        represent the coordinates of the current figure/tool
(define-struct tool [type size color mode x1 y1 x2 y2])

; a List<Image> is one of:
; - '()
; - (cons Image List<Image>)
; interpretation: a list of canvases

; an AppState is a structure (make-appstate canvas pre post tool quit)
; where canvas : Image
;       pre, post : List<Image>
;       tool : Tool
;       quit : Boolean
; interpretation: it represents the current state of the app
(define-struct appstate [canvas pre post tool quit])

; Examples of data
(define IS (make-appstate EMPTY-CANVAS
                          '()
                          '()
                          (make-tool "square" 1 "black" "outline" 0 0 0 0)
                          #false))
 
; ==================================================================================================

;; Input/Output
; render : AppState -> Image
; it takes an AppState and returns the current Canvas

;; Code
(define (render app)
  (local ((define type (tool-type (appstate-tool app))))
    (overlay
     (cond
      [(string=? type "square") (draw-square app)]
     ;[(string=? type "rectangle") (draw-rectangle)]
      [else (appstate-canvas app)])
     (appstate-canvas app))))
     

; ==================================================================================================

;; Input/Output
; draw-square : AppState -> Image
; it takes an AppState and returns the Canvas with the currently drawn square

;; Code
(define (draw-square app)
  (local
    ((define S (square-size (appstate-tool app)))
     (define MODE (tool-mode (appstate-tool app)))
     (define COLOR (tool-color (appstate-tool app)))
     (define X1 (tool-x1 (appstate-tool app)))
     (define X2 (tool-x2 (appstate-tool app)))
     (define Y1 (tool-y1 (appstate-tool app)))
     (define Y2 (tool-y2 (appstate-tool app))))
  (cond
    [(and (> X1 X2) (> Y1 Y2))
     (place-image
      (square S MODE COLOR)
      (- X1 (/ (integer-sqrt (* 2 (* S S))) 2)) 
      (- Y1 (/ (integer-sqrt (* 2 (* S S))) 2))
      (appstate-canvas app))]
    [(and (< X1 X2) (< Y1 Y2))
     (place-image
      (square S MODE COLOR)
      (+ X1 (/ (integer-sqrt (* 2 (* S S))) 2)) 
      (+ Y1 (/ (integer-sqrt (* 2 (* S S))) 2))
      (appstate-canvas app))]
    [(and (> X1 X2) (< Y1 Y2))
     (place-image
      (square S MODE COLOR)
      (- X1 (/ (integer-sqrt (* 2 (* S S))) 2)) 
      (+ Y1 (/ (integer-sqrt (* 2 (* S S))) 2))
      (appstate-canvas app))]
    [(and (< X1 X2) (> Y1 Y2))
     (place-image
      (square S MODE COLOR)
      (+ X1 (/ (integer-sqrt (* 2 (* S S))) 2)) 
      (- Y1 (/ (integer-sqrt (* 2 (* S S))) 2))
      (appstate-canvas app))]
    [else (appstate-canvas app)])))

;; Input/Output
; square-size : Tool -> Number
; takes a tool and return the width/height of the currently drawn square

;: Code
(define (square-size t)
  (local ((define X1 (tool-x1 t))
          (define Y1 (tool-y1 t))
          (define X2 (tool-x2 t))
          (define Y2 (tool-y2 t)))
    (cond
      [(<= (abs (- X2 X1)) (abs (- Y2 Y1)))
       (abs (- X2 X1))]
      [else
       (abs (- Y2 Y1))])))

;; Input/Output
; add-figure-to-canvas : AppState -> AppState
; takes an appstate and returns the appstate with the currently drawn figure added to the canvas

;; Code
(define (add-figure-to-canvas app)
  (local ((define type (tool-type (appstate-tool app))))
    (cond
      [(string=? type "square") (add-square-to-canvas app)]
     ;[(string=? type "line") (add-line-to-canvas app)]
     ;[(string=? type "rectangle") (add-rectangle-to-canvas app)]
    
    [else app])))

;; Input/Output
; add-square-to-canvas : AppState -> AppState
; takes an appstate and returns the appstate with the currently drawn square added to the canvas

;; Code
(define (add-square-to-canvas app)
  (local
    ((define S (square-size (appstate-tool app)))
     (define X1 (tool-x1 (appstate-tool app)))
     (define X2 (tool-x2 (appstate-tool app)))
     (define Y1 (tool-y1 (appstate-tool app)))
     (define Y2 (tool-y2 (appstate-tool app))))  
  (make-appstate
   (draw-square app)
   (cons (appstate-canvas app)
         (appstate-pre app))
   '()
   (appstate-tool app)
   (appstate-quit app))))

;; Input/Output
; move-end : AppState New-x New-y -> Appstate
; it takes an AppState and new end coordinates New-x and New-y
; and returns the Appstate where the current tool’s end point is at coordinates New-x and New-y

;; Code
(define (move-end app new-x new-y)
  (if (boolean? (tool-type (appstate-tool app))) app 
      (make-appstate (appstate-canvas app)                                   
                     (appstate-pre app)
                     (appstate-post app)
                     (make-tool (tool-type (appstate-tool app))
                                (tool-size (appstate-tool app))
                                (tool-color (appstate-tool app))
                                (tool-mode (appstate-tool app))
                                (tool-x1 (appstate-tool app))
                                (tool-y1 (appstate-tool app))
                                new-x
                                new-y)
                     (appstate-quit app))))

; ==================================================================================================
; Undo + Render Functions

; Undo ------------------------------------------------------------------------------

(define (undo a)
  (cond [(empty? (appstate-pre a)) a]
        [else
         (make-appstate
          (first (appstate-pre a))
          (rest (appstate-pre a))
          (cons (appstate-canvas a)
                (appstate-post a))
          (appstate-tool a)
          #false)]))

; Redo ------------------------------------------------------------------------------

(define (redo a)
  (cond [(empty? (appstate-post a)) a]
        [else
         (make-appstate
          (first (appstate-post a))
          (cons (appstate-canvas a)
                (appstate-pre a))
          (rest (appstate-post a))
          (appstate-tool a)
          #false)]))

; ==================================================================================================

;; Input/output
; handle-mouse : MouseEvent AppState -> AppState
; it takes a MouseEvent input and returns the updated AppState

;; Code 
(define (handle-mouse a x y me)
  (local ((define type (tool-type (appstate-tool a))))
    (cond
      [(or (string=? type "line")                           ; if the selected tool is one of these figures 
           (string=? type "square")
           (string=? type "rectangle")
           (string=? type "circle")
           (string=? type "ellipse"))
       (cond [(string=? me "button-down")
              (make-appstate (appstate-canvas a)
                             (appstate-pre a)
                             (appstate-post a)
                             (make-tool (tool-type (appstate-tool a))
                                        (tool-size (appstate-tool a))
                                        (tool-color (appstate-tool a))
                                        (tool-mode (appstate-tool a))
                                        x y x y)
                             (appstate-quit a))]
             [(string=? me "drag")
              (move-end a x y)]
             [(string=? me "button-up")
              (add-figure-to-canvas a)]
             [else a])]
;     []                                                    ; for other tools
      [else a])))

; ====================================================================================
; Key Handler

; AUXILIARY FUNCTION
; set-tool: AppState Tool -> AppState
; change tool type to corresponding input
; (define (set-tool a t) a)

(define (set-tool-type a t)
  (make-appstate
   (appstate-canvas a)
   (appstate-pre a)
   (appstate-post a)
   (make-tool t ; set tool-type to "t" input
              (tool-size (appstate-tool a))
              (tool-color (appstate-tool a))
              (tool-mode (appstate-tool a))
              (tool-x1 (appstate-tool a))
              (tool-y1 (appstate-tool a))
              (tool-x2 (appstate-tool a))
              (tool-y2 (appstate-tool a)))
   (appstate-quit a)))

; handle-key: AppState KeyEvent -> AppState
; Takes a key event and calls the function to perform the corresponding change
; to the current appstate/canvas
; (define (handle-key a k) a)

(define (handle-key a k)
  (cond [(string=? k "u") (undo a)]
        [(string=? k "r") (redo a)]
       ;[(string=? k "l") (set-tool-type a "line")] ; behaves weirdly with the rendering function
        [(string=? k "s") (set-tool-type a "square")]
        [else a]))

; ==================================================================================================

;; Input/Output
; quit? : AppState -> Boolean
; it takes an AppState and returns a Boolean indicating whether the app has quit or not.
; header: (define (quit? appstate) #true)

;; Code
(define (quit? app) (appstate-quit app))

; ==================================================================================================

;; Input/Output
; draw-app : AppState -> AppState
; it takes an initial AppState and runs an application to draw lines on a canvas

;; Code
(define (draw-app is)
  (big-bang is
    [to-draw render]
    [on-mouse handle-mouse]
    [on-key handle-key]
    [stop-when quit?]))
