;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Figures_and_Rendering) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

; Start app by using (start-app MAX-W MAX-H BG-COLOR)

; Constants
(define MAX-W 1800)
(define MAX-H 1000)
(define BG-COLOR "gainsboro")
(define EMPTY-CANVAS (rectangle 1800 1000 "solid" "white"))

;; Data types

; a Tool is a structure (make-tool type size color mode x1 y1 x2 y2 status)
; where type : String
;       size : Number
;       color : Color 
;       mode : String or Number
;              represents the mode for the figures ("outline" or "solid")        
;              (it can also be a number from 0 to 255 that indicates its transparency, with 0 being fully transparent and 255 being "solid")
;       x1, y1, x2, y2 : Number
;                        represent the coordinates of the current figure/tool
;       extra: Any
;              optional field used to store temporary values (eg: a list for freehand tool), otherwise it's set to #false
;       status : Boolean
;                represents if the tool is active
(define-struct tool [type size color mode x1 y1 x2 y2 extra status])

; a List<Image> is one of:
; - '()
; - (cons Image List<Image>)
; interpretation: a list of canvases

; a UI is a Structure (make-ui hover click c1 c2 c3 c4 c5 c6 c7 c8 c9)
; where hover, click: String
;       tool/option being hovered/clicked by mouse (if any, otherwise they're set to #false)
;       sel : Structure (make-color) or String
;       saves a selected color in case a tool (eg eraser) is selected
;       c1, c2, c3, c4, c5, ... : Structure (make-color) or String
;       they represent a palette of 9 colors the user has saved
(define-struct ui [hover click sel c1 c2 c3 c4 c5 c6 c7 c8 c9])

; Data Examples
(define START-UI (make-ui #false #false "c9" "lightcoral" "sandy brown" "gold" "aquamarine" "royal blue" "medium orchid" "pink" "white" "black"))

; an AppState is a structure (make-appstate canvas pre post tool quit)
; where canvas : Image
;       pre, post : List<Image>
;       tool : Tool
;       quit : Boolean
; interpretation: it represents the current state of the app
(define-struct appstate [canvas pre post tool ui quit])

; Examples of data
(define IS
  (make-appstate
   EMPTY-CANVAS
   '()
   '()
   (make-tool "free" 1 (ui-c9 START-UI) "outline" 0 0 0 0 #false #false)
   START-UI
   #false))

; ========================================================================================
; Updating Tool and UI Values

; update : AppState String Any Any -> AppState
; updates input field of current tool/ui to the input value
; 'mod' is an optional modifier (eg + or -), otherwise its #false
; (define (update app field new-value mod) a)

; Examples
(check-expect (update IS "type" "free" #false)
              (make-appstate
               EMPTY-CANVAS
               '()
               '()
               (make-tool "free" 1 "black" "outline" 0 0 0 0 #false #false)
               START-UI
               #false))

(define (update a field new-value mod)
  (local (; local values for better readability -----------------
          ; appstate values
          (define TOOL (appstate-tool a))
          (define UI (appstate-ui a))
          (define X1 (tool-x1 TOOL))
          (define Y1 (tool-y1 TOOL))
          (define X2 (tool-x2 TOOL))
          (define Y2 (tool-y2 TOOL))
          (define C1 (ui-c1 (appstate-ui a)))
          (define C2 (ui-c2 (appstate-ui a)))
          (define C3 (ui-c3 (appstate-ui a)))
          (define C4 (ui-c4 (appstate-ui a)))
          (define C5 (ui-c5 (appstate-ui a)))
          (define C6 (ui-c6 (appstate-ui a)))
          (define C7 (ui-c7 (appstate-ui a)))
          (define C8 (ui-c8 (appstate-ui a)))
          (define C9 (ui-c9 (appstate-ui a))))
    ; -----------------------------------------------------------
      (make-appstate
       (appstate-canvas a)
       (appstate-pre a)
       (appstate-post a)
       ; Updating Tool Values
       (make-tool
        (if (string=? field "type") new-value (tool-type TOOL))              ; changing tool type
        (cond [(string=? field "size") (mod (tool-size TOOL) new-value)]     ; changing tool size 
              [(string=? field "size-ui") new-value]                         ;   - set value directly if chosen from ui
              [else (tool-size TOOL)])
        (if (string=? field "sel")
            (cond [(string=? new-value "c1") C1]
                  [(string=? new-value "c2") C2]
                  [(string=? new-value "c3") C3]
                  [(string=? new-value "c4") C4]
                  [(string=? new-value "c5") C5]
                  [(string=? new-value "c6") C6]
                  [(string=? new-value "c7") C7]
                  [(string=? new-value "c8") C8]
                  [(string=? new-value "c9") C9])
            (tool-color TOOL))
        (if (string=? field "mode") new-value (tool-mode TOOL))              ; changing tool mode
        X1 Y1 X2 Y2
        (tool-extra TOOL)
        (tool-status TOOL))
       ; Updating UI Values
       (make-ui
        (if (string=? field "hover") new-value (ui-hover UI)) ; changing ui option mouse is hovering (if any)
        (if (string=? field "click") new-value (ui-hover UI)) ; changing ui option mouse is clicking (if any)
        (if (string=? field "sel") new-value (ui-sel UI))   ; setting selected color (used for eraser)
        C1 C2 C3 C4 C5 C6 C7 C8 C9)
       (appstate-quit a))))
 
; ==================================================================================================

;; Input/Output
; get-color : Image Number Number -> Color
; takes an image and two coordinates and returns the color in that point of the image
; header: (define (get-color img x y) "red")

(define (get-color i x y)
  (first (image->color-list
          (crop x y 1 1 i))))

;; Iput/Output
; get-color-canvas : AppState -> Color
; takes an appstate and returns the initial color of the canvas
; header: (define (get-color-canvas app) "red")

(define (get-color-canvas app)
  (cond
    [(empty? (appstate-pre app))
     (get-color (appstate-canvas app))]
    [(empty? (rest (appstate-pre app)))
     (get-color (first (appstate-pre app)) 1 1)]
    [else
     (get-color-canvas
      (make-appstate
       (appstate-canvas app)
       (rest (appstate-pre app))
       (appstate-post app)
       (appstate-tool app)
       (appstate-ui app)
       (appstate-quit app)))]))

; ===================================================================================
; Changing canvas size

; change-canvas-size a k : AppState KeyEvent -> Image
; enlarges/reduces current canvas depending on arrow keys
; (define (change-canvas-size a k) EMPTY-CANVAS)
(define (change-canvas-size a k)
  (local ((define WIDTH (image-width (appstate-canvas a)))
          (define HEIGHT (image-height (appstate-canvas a))))
          ;(define COLOR (the bg color of the initial canvas)))
    (make-appstate
       (cond [(and (string=? k "right") ; check width isnt at its maximum
                   (<= WIDTH MAX-W))
              (overlay/align "left" "middle"
                             (appstate-canvas a)
                             (rectangle (+ WIDTH 50) HEIGHT
                                        "solid" (get-color (appstate-canvas a) 0 0)))]
             [(and (string=? k "down")  ; check height isnt at its maximum
                   (<= HEIGHT MAX-H))
              (overlay/align "middle" "top"
                             (appstate-canvas a)
                             (rectangle WIDTH (+ HEIGHT 50)
                                        "solid" (get-color (appstate-canvas a) 0 0)))]
             [(and (string=? k "left")
                   (<= UI-W WIDTH)) ; check width isnt at its minimum
              (crop 0 0 (- WIDTH 50) HEIGHT (appstate-canvas a))]
             [(and (string=? k "up")
                   (<= 100 HEIGHT)) ; check height isnt at its minimum
              (crop 0 0 WIDTH (- HEIGHT 50) (appstate-canvas a))]
             [else (appstate-canvas a)])
       (cons (appstate-canvas a) (appstate-pre a))
       '()
       (appstate-tool a)
       (appstate-ui a)
       (appstate-quit a))))

; ==================================================================================================

;; Input/Output
; render : AppState -> Image
; it takes an AppState and returns the current Canvas

(define (render app)
  (local ((define TYPE (tool-type (appstate-tool app))))
    (place-images
     (list ;(draw-selected-ui app) ;[!] uncomment to render select pointer (slow)
           UI-BASE
           (cond [(tool-status (appstate-tool app)) ; tool is active
            (cond [(string=? TYPE "free") (draw-free app)]
                  [(string=? TYPE "eraser") (draw-free app)]
                  [(string=? TYPE "line") (draw-line app)]
                  [(string=? TYPE "square") (draw-figure app)]
                  [(string=? TYPE "rectangle") (draw-figure app)]
                  [(string=? TYPE "circle") (draw-figure app)]
                  [(string=? TYPE "ellipse") (draw-figure app)]
                  [else (appstate-canvas app)])]
            [else (appstate-canvas app)]))
     (list ;(make-posn (/ UI-W 2) (/ UI-H 2))  ;[!] uncomment to render select pointer (slow)
           (make-posn (/ UI-W 2) (/ UI-H 2))
           (make-posn (/ (image-width (appstate-canvas app)) 2)
                      (/ (image-height (appstate-canvas app)) 2)))
     (empty-scene MAX-W MAX-H "gray"))))

; ===================================================================================
; Line Tool

; draw-line: AppState Number Number -> Image
; add current line to canvas
;(define (draw-line a x y) EMPTY-CANVAS)

(define (draw-line a)
  (local ((define X1 (tool-x1 (appstate-tool a)))
          (define Y1 (tool-y1 (appstate-tool a)))
          (define X2 (tool-x2 (appstate-tool a)))
          (define Y2 (tool-y2 (appstate-tool a)))
          (define COLOR (tool-color (appstate-tool a)))
          (define SIZE (tool-size (appstate-tool a))))
  (add-line
   (appstate-canvas a)
   X1 Y1 X2 Y2
   (pen COLOR SIZE "solid" "round" "round"))))

; ==================================================================================================
; Freehand Drawing Tool

; move-end-free a x y : AppState Number Number -> AppState
; updates tool's endpoints and temporarily replaces the "extra" 
; field with a list of coordinates (stored as posn)
(define (move-end-free a x y)
  (local (; tool values for readability
          (define X1 (tool-x1 (appstate-tool a)))
          (define Y1 (tool-y1 (appstate-tool a)))
          ;-------------------------------------------------------------------------------------
          (define LOP (tool-extra (appstate-tool a))) ; extra is temporarily holding a list<posn>
          (define X0 (posn-x (first LOP))) ; coordinates of the 
          (define Y0 (posn-y (first LOP))) ; latest posn in LOP
          ; add-points : AppState Number Number -> List<Posn>
          ; build a list of mouse coordinates
          (define (add-points a x y)
            (cond [(and (<= (abs (- x X0)) 5)  ; if latest coordinates
                        (<= (abs (- y Y0)) 5)) ; are closer than 5 pixels
                   LOP] ; return LOP unchanged
                  [else                            
                   (cons (make-posn x y) LOP)]))) ; else, add new point to list
    ; Creating the new AppState ------------------------------------------------
    (make-appstate
     (appstate-canvas a)
     (appstate-pre a)
     (appstate-post a)
     (make-tool
      (tool-type (appstate-tool a))
      (tool-size (appstate-tool a))
      (tool-color (appstate-tool a))
      (tool-mode (appstate-tool a))
      X1 Y1 x y ; update coordinates
      (add-points a x y) ; update list stored in extra with new x and y coordinates
      #true)
     (appstate-ui a)
     #false)))

; draw-free: AppState -> Image
; get the values of the list<posn> stored in "extra" field
; and draw them by treating them as line coordinates
; (define (draw-free a) EMPTY-CANVAS)
(define (draw-free a)
  (local ((define LOP (tool-extra (appstate-tool a)))
          (define COLOR (tool-color (appstate-tool a)))
          (define SIZE (tool-size (appstate-tool a)))
          (define (draw-point lop)
            (cond [(empty? lop) (appstate-canvas a)] ; termination argument: there are no more posn in lop
                  [(empty? (rest lop))                                    
                   (place-image (circle (/ SIZE 2) "solid" COLOR)         ; if there's only one element in lop (a single point) 
                                (posn-x (first lop)) (posn-y (first lop)) ; add it to canvas using a circle
                                (appstate-canvas a))]
                  [else (add-line                                                ; else (there are >1 posn in list)
                         (draw-point (rest lop))                                 ; use recursion to tackle all points
                         (posn-x (first (rest lop))) (posn-y (first (rest lop))) ; - second-to-last coordinates 
                         (posn-x (first lop)) (posn-y (first lop))               ; - last coordinates
                         (pen COLOR SIZE "solid" "round" "round"))])))
    (draw-point LOP)))

; add-free-to-canvas: AppState -> AppState
; update appstate with freehand line drawn on the canvas
; (define (add-free-to-canvas a) a)
(define (add-free-to-canvas a)
  (local ((define TYPE (tool-type (appstate-tool a))))
  (make-appstate
   (draw-free a) ; use draw-free to get the canvas with the freehand line drawn
   (cons (appstate-canvas a)
         (appstate-pre a))
   '()
   (make-tool (tool-type (appstate-tool a))
              (tool-size (appstate-tool a))
              (if (string=? "eraser" TYPE)
                  "black"
                  (tool-color (appstate-tool a)))
              (tool-mode (appstate-tool a))
              0 0 0 0  ; x1,y1,x2,y2 set to 0
              #false   ; set extra to #false
              #false)  ; tool set to not active
   (appstate-ui a)
   (appstate-quit a))))

; ==================================================================================================
; Rectangle/Square/Circle/Ellipse Tool

;; Input/Output
; draw-figure : AppState -> Image
; it takes an AppState and returns the Canvas with the currently drawn figure (square, rectangle, circle or ellipse)

;; Code
(define (draw-figure app)
  (local
    ((define H (figure-height (appstate-tool app)))
     (define W (figure-width (appstate-tool app)))
     (define MODE (tool-mode (appstate-tool app)))
     (define COLOR (tool-color (appstate-tool app)))
     (define X1 (tool-x1 (appstate-tool app)))
     (define X2 (tool-x2 (appstate-tool app)))
     (define Y1 (tool-y1 (appstate-tool app)))
     (define Y2 (tool-y2 (appstate-tool app)))
     (define FIGURE (cond
                      [(or (string=? "square" (tool-type (appstate-tool app)))
                           (string=? "rectangle" (tool-type (appstate-tool app))))
                       (rectangle W H MODE COLOR)]
                      [(or (string=? "circle" (tool-type (appstate-tool app)))
                           (string=? "ellipse" (tool-type (appstate-tool app))))
                       (ellipse W H MODE COLOR)])))
  (cond
    [(and (> X1 X2) (> Y1 Y2))
     (place-image
      FIGURE
      (- X1 (/ W 2)) 
      (- Y1 (/ H 2))
      (appstate-canvas app))]
    [(and (< X1 X2) (< Y1 Y2))
     (place-image
      FIGURE
      (+ X1 (/ W 2)) 
      (+ Y1 (/ H 2))
      (appstate-canvas app))]
    [(and (> X1 X2) (< Y1 Y2))
     (place-image
      FIGURE
      (- X1 (/ W 2)) 
      (+ Y1 (/ H 2))
      (appstate-canvas app))]
    [(and (< X1 X2) (> Y1 Y2))
     (place-image
      FIGURE
      (+ X1 (/ W 2)) 
      (- Y1 (/ H 2))
      (appstate-canvas app))]
    [else (appstate-canvas app)])))

;; Input/Output
; figure-height : Tool -> Number
; takes a tool and return the height of the currently drawn rectangle

;: Code
(define (figure-height t)
  (local ((define X1 (tool-x1 t))
          (define Y1 (tool-y1 t))
          (define X2 (tool-x2 t))
          (define Y2 (tool-y2 t)))
    (cond
      [(or (string=? "square" (tool-type t)) (string=? "circle" (tool-type t)))
       (cond
         [(<= (abs (- X2 X1)) (abs (- Y2 Y1)))
          (abs (- X2 X1))]
         [else
          (abs (- Y2 Y1))])]
      [else (abs (- Y1 Y2))]))) 

;; Input/Output
; figure-width : Tool -> Number
; takes a tool and return the width of the currently drawn figure

;: Code
(define (figure-width t)
  (local ((define X1 (tool-x1 t))
          (define Y1 (tool-y1 t))
          (define X2 (tool-x2 t))
          (define Y2 (tool-y2 t)))
    (cond
      [(or (string=? "square" (tool-type t)) (string=? "circle" (tool-type t)))
       (cond
         [(<= (abs (- X2 X1)) (abs (- Y2 Y1)))
          (abs (- X2 X1))]
         [else
          (abs (- Y2 Y1))])]
      [else (abs (- X1 X2))]))) 

; ==================================================================================================

;; Input/Output
; add-figure-to-canvas : AppState -> AppState
; takes an appstate and returns the appstate with the currently drawn figure added to the canvas

;; Code
(define (add-figure-to-canvas app)
  (local ((define type (tool-type (appstate-tool app))))
    (make-appstate
     (cond
       [(string=? type "square") (draw-figure app)]
       [(string=? type "line") (draw-line app)]
       [(string=? type "rectangle") (draw-figure app)]
       [(string=? type "circle") (draw-figure app)]
       [(string=? type "ellipse") (draw-figure app)]
       [else (appstate-canvas app)])
     (cons (appstate-canvas app)
         (appstate-pre app))
     '()
     (make-tool (tool-type (appstate-tool app))
                (tool-size (appstate-tool app))
                (tool-color (appstate-tool app))
                (tool-mode (appstate-tool app))
                0 0 0 0 ; x1,y1,x2,y2 set to 0
                #false ; set extra to false
                #false) ; tool set to not active
     (appstate-ui app)
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
                                new-y
                                (tool-extra (appstate-tool app))
                                (tool-status (appstate-tool app)))
                     (appstate-ui app)
                     (appstate-quit app))))

; ==================================================================================================
; Fill Tool

;; Input/Output
; fill : AppState -> AppState

(define (fill app)
  (local ((define COLOR (tool-color (appstate-tool app))))
   (make-appstate 
           (rectangle MAX-W MAX-H "solid" COLOR)
           (cons (appstate-canvas app)
                 (appstate-pre app))
           '()
           (make-tool (tool-type (appstate-tool app))
                (tool-size (appstate-tool app))
                (tool-color (appstate-tool app))
                (tool-mode (appstate-tool app))
                0 0 0 0 ; x1,y1,x2,y2 set to 0
                #false ; set extra to false
                #false) ; tool set to not active
           (appstate-ui app)
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
          (appstate-ui a)
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
          (appstate-ui a)
          #false)]))

; ====================================================================================
; User Interface

; Constants
(define UI-H MAX-H)
(define UI-W 300)
(define UI-BG-COLOR (make-color 95 100 125 255))
(define UI-BG (rectangle UI-W UI-H "solid" UI-BG-COLOR))

; Top Icons
(define TOP-PEN (pen "cornflower blue" 5 "solid" "round" "round"))

(define SAVE (overlay
              (text/font "Save" 30 "cornflower blue"
                         "Gill Sans" "default" "normal" "bold" #f)
              (rectangle (- (/ UI-W 2) 5) 50 "outline" TOP-PEN)
              (rectangle (- (/ UI-W 2) 5) 50 "solid" "white smoke")))

(define UNDO (overlay/offset
              (rotate 90 (isosceles-triangle 17 77 "solid" "cornflower blue"))
              10 7
              (add-curve
               (overlay (rectangle (/ UI-W 4) 50 "outline" TOP-PEN)
                        (rectangle (/ UI-W 4) 50 "solid" "white smoke"))
               25 33 -30 2
               30 17 15 -2
               (pen "cornflower blue" 8 "solid" "butt" "round"))))

(define REDO (flip-horizontal UNDO))

; thick-icon : String -> Image
; create thickness icon of size "s"
(define (thick-icon s)
  (overlay
   (rectangle 60 50 "outline" (pen "black" 5 "solid" "round" "round"))
   (add-line
    (rectangle 50 40 "solid" "white smoke")
    15 20 35 20
    (pen UI-BG-COLOR s "solid" "round" "round"))))

; Base sizes
(define XS (thick-icon 2))
(define S (thick-icon 7))
(define M (thick-icon 13))
(define L (thick-icon 20))
(define XL (thick-icon 26))

; color-icon : Color -> Image
; create palette icon of color "color"
(define (color-icon color)
  (overlay (rectangle 45 45 "outline" (pen "black" 5 "solid" "round" "round"))
           (rectangle 35 35 "outline" "white smoke")
           (rectangle 35 35 "solid" color)))

; Outline and Solid Icons
(define SOLID-BG (rectangle 70 70 "solid" "white smoke"))

(define SOLID-ICON
  (overlay (rectangle 70 70 "outline" (pen "black" 5 "solid" "round" "round"))
           (above (beside (circle 15 "solid" "cornflower blue")
                          (triangle 30 "solid" "cornflower blue"))
                  (rectangle 3 3 "solid" "transparent") ; separator
                  (rectangle 40 20 "solid" "cornflower blue"))
           SOLID-BG))

(define OUTLINE-ICON
  (overlay (rectangle 70 70 "outline" (pen "black" 5 "solid" "round" "round"))
           (above (beside (circle 15 "outline" (pen "cornflower blue" 2 "solid" "round" "round"))
                          (triangle 30 "outline" (pen "cornflower blue" 2 "solid" "round" "round")))
                  (rectangle 3 3 "solid" "transparent") ; separator
                  (rectangle 40 20 "outline" (pen "cornflower blue" 2 "solid" "round" "round")))
           SOLID-BG))

; Figure Icons
(define FIG-OUT (rectangle 70 70 "outline" (pen "black" 5 "solid" "round" "round")))

(define RECTANGLE-ICON
  (overlay
   FIG-OUT
   (rectangle 40 30 "outline" (pen UI-BG-COLOR 3 "solid" "round" "round"))
   SOLID-BG))

(define SQUARE-ICON
  (overlay
   FIG-OUT
   (rectangle 30 30 "outline" (pen UI-BG-COLOR 3 "solid" "round" "round"))
   SOLID-BG))

(define CIRCLE-ICON
  (overlay
   FIG-OUT
   (circle 20 "outline" (pen UI-BG-COLOR 3 "solid" "round" "round"))
   SOLID-BG))

(define ELLIPSE-ICON
  (overlay
   FIG-OUT
   (ellipse 40 25 "outline" (pen UI-BG-COLOR 3 "solid" "round" "round"))
   SOLID-BG))

(define LINE-ICON
  (overlay
   FIG-OUT
   (add-line
    SOLID-BG
    21 50 48 20
    (pen UI-BG-COLOR 3 "solid" "round" "round"))))

(define FREE-ICON
  (overlay
   FIG-OUT
   (add-curve SOLID-BG
              20 50 0 1
              50 20 0 1
              (pen UI-BG-COLOR 3 "solid" "round" "round"))))

; Special Tool Icons
(define ERASER
  (overlay (rectangle 100 100 "outline" (pen "black" 3 "solid" "round" "round")) ; outline
           (overlay
            (above (rectangle 60 30 "solid" "steel blue")  ; blue portion
                   (rectangle 50 10 "solid" "white smoke") ; white separation
                   (rectangle 60 45 "solid" "chocolate"))  ; red portion
            (rectangle 90 90 "solid" "white smoke")))) ; bg

(define FILL
  (overlay (rectangle 100 100 "outline" (pen "black" 3 "solid" "round" "round")) ;outline
           (add-line
            (add-line
             (add-line
              (overlay
               (overlay/xy
                (ellipse 60 20 "solid" "chocolate") ; color top
                0 8
                (overlay/xy ; overlay rectangle and round bottom
                 (rectangle 60 50 "solid" "gray")
                 0 40
                 (ellipse 60 20 "solid" "gray")))
               (rectangle 90 90 "solid" "white smoke")) ;bg
              32 20 32 37 ; 1st line pos
              (pen "chocolate" 8 "solid" "round" "round"))
             40 20 40 55 ; 2nd line pos
             (pen "chocolate" 8 "solid" "round" "round"))
            48 20 48 48 ;3rd line pos
            (pen "chocolate" 8 "solid" "round" "round"))))

; Getting information to draw on UI -----------------------------------------------

; List of icons + their position
(define (make-loic a)
  (local ((define C1 (color-icon (ui-c1 (appstate-ui a))))
          (define C2 (color-icon (ui-c2 (appstate-ui a))))
          (define C3 (color-icon (ui-c3 (appstate-ui a))))
          (define C4 (color-icon (ui-c4 (appstate-ui a))))
          (define C5 (color-icon (ui-c5 (appstate-ui a))))
          (define C6 (color-icon (ui-c6 (appstate-ui a))))
          (define C7 (color-icon (ui-c7 (appstate-ui a))))
          (define C8 (color-icon (ui-c8 (appstate-ui a))))
          (define C9 (color-icon (ui-c9 (appstate-ui a)))))
    (list SAVE (make-posn 75 25)
          UNDO (make-posn 185 25)
          REDO (make-posn 260 25)
          C1 (make-posn 45 100)
          C2 (make-posn 105 100)
          C3 (make-posn 165 100)
          C4 (make-posn 45 160)
          C5 (make-posn 105 160)
          C6 (make-posn 165 160)
          C7 (make-posn 45 220)
          C8 (make-posn 105 220)
          C9 (make-posn 165 220)
          XS (make-posn 255 95)
          S (make-posn 255 155)
          M (make-posn 255 215)
          L (make-posn 255 275)
          XL (make-posn 255 335)
          OUTLINE-ICON (make-posn 60 310)
          SOLID-ICON (make-posn 155 310)
          RECTANGLE-ICON (make-posn 200 450)
          SQUARE-ICON (make-posn 100 450)
          CIRCLE-ICON (make-posn 100 550)
          ELLIPSE-ICON (make-posn 200 550)
          LINE-ICON (make-posn 100 650)
          FREE-ICON (make-posn 200 650)
          ERASER (make-posn 90 800)
          FILL (make-posn 210 800)
          )))
    
(define LOIC (make-loic IS))

; List of Icons
(define LOI
  (filter image? LOIC)) ; tip: type LOI in interactions area to see all icons

; List of Coordinates
(define LOC
  (filter posn? LOIC))

; UI Base --------------
(define UI-BASE
  (place-images
   LOI
   LOC
   UI-BG))

; Getting info from the UI -------------------------------------------------------------------

; Image -> Posn
; return the location (centered) of the icon as a posn
; (define (get-icon-posn icon loi loc) '())
(define (get-icon-posn icon)
  (local (; get-posn : Image List<Image> List<Posn> -> Posn
          ; find image coordinates in list
          (define (get-posn icon loi loc)
            (cond
              [(or (empty? loi)
                   (empty? loc))
               (make-posn 0 0)]
              [(equal? icon (first loi)) ; if first list item agrees with input icon 
               (first loc)] ; return corresponding posn
              [else
               (get-posn icon (rest loi) (rest loc))])))
    (get-posn icon LOI LOC)))

; get-hover : AppState Integer Integer -> Image
; return the tool/option icon the mouse is currently hovering on the UI
; (define (get-hover a x y) #false)
(define (get-hover x y)
  (local ((define (X1 i) (- (posn-x (get-icon-posn i)) (/ (image-width i) 2))) ; left-hand side
          (define (X2 i) (+ (posn-x (get-icon-posn i)) (/ (image-width i) 2))) ; right-hand side
          (define (Y1 i) (- (posn-y (get-icon-posn i)) (/ (image-height i) 2))) ; top of icon
          (define (Y2 i) (+ (posn-y (get-icon-posn i)) (/ (image-height i) 2))) ; bottom of icon
          ; find-hover : List<Image> Integer Integer -> Maybe<Image>
          ; if mouse is hovering an icon, return that image. if not, return #false
          (define (find-hover loi x y)
            (cond
              [(empty? loi) #false]
              [(and (< (X1 (first loi)) x (X2 (first loi)))
                    (< (Y1 (first loi)) y (Y2 (first loi))))
               (first loi)]
              [else (find-hover (rest loi) x y)])))
    (find-hover LOI x y)))
  
; select-from-ui : AppState Image -> AppState
; updates the appstate according to selection in UI (button-down where the mouse is hovering)
; (define (select-from-ui a hover) a)
(define (select-from-ui a hover)
  (local ((define C1 (color-icon (ui-c1 (appstate-ui a))))
          (define C2 (color-icon (ui-c2 (appstate-ui a))))
          (define C3 (color-icon (ui-c3 (appstate-ui a))))
          (define C4 (color-icon (ui-c4 (appstate-ui a))))
          (define C5 (color-icon (ui-c5 (appstate-ui a))))
          (define C6 (color-icon (ui-c6 (appstate-ui a))))
          (define C7 (color-icon (ui-c7 (appstate-ui a))))
          (define C8 (color-icon (ui-c8 (appstate-ui a))))
          (define C9 (color-icon (ui-c9 (appstate-ui a)))))
  (cond
    [(false? hover) a]
    [(equal? hover SAVE)
     (save a)]
    [(equal? hover UNDO)
     (undo a)]
    [(equal? hover REDO)
     (redo a)]
    [(equal? hover C1)
     (update a "sel" "c1" #false)]
    [(equal? hover C2)
     (update a "sel" "c2" #false)]
    [(equal? hover C3)
     (update a "sel" "c3" #false)]
    [(equal? hover C4)
     (update a "sel" "c4" #false)]
    [(equal? hover C5)
     (update a "sel" "c5" #false)]
    [(equal? hover C6)
     (update a "sel" "c6" #false)]
    [(equal? hover C7)
     (update a "sel" "c7" #false)]
    [(equal? hover C8)
     (update a "sel" "c8" #false)]
    [(equal? hover C9)
     (update a "sel" "c9" #false)]
    [(equal? hover XS)
     (update a "size-ui" 1 #false)]
    [(equal? hover S)
     (update a "size-ui" 8 #false)]
    [(equal? hover M)
     (update a "size-ui" 15 #false)]
    [(equal? hover L)
     (update a "size-ui" 22 #false)]
    [(equal? hover XL)
     (update a "size-ui" 30 #false)]
    [(equal? hover OUTLINE-ICON)
     (update a "mode" "outline" #false)]
    [(equal? hover SOLID-ICON)
     (update a "mode" "solid" #false)]
    [(equal? hover ERASER)
     (update a "tool" "eraser" #false)]
    [(equal? hover FILL)
     (update a "tool" "fill" #false)]
    [else a])))

; UI Rendering --------------------------------------------------------------------

; str->icon : String -> Image
; take a string and return the corresponding icon it represents
(define (str->icon s)
  (cond
    [(string=? s "save") SAVE]
    [(string=? s "undo") UNDO]
    [(string=? s "redo") REDO]
    [(string=? s "free") FREE-ICON]
    [(string=? s "line") LINE-ICON]
    [(string=? s "ellipse") ELLIPSE-ICON]
    [(string=? s "circle") CIRCLE-ICON]
    [(string=? s "square") SQUARE-ICON]
    [(string=? s "rectangle") RECTANGLE-ICON]
    [(string=? s "solid") SOLID-ICON]
    [(string=? s "outline") OUTLINE-ICON]
    [(string=? s "eraser") ERASER]
    [(string=? s "fill") FILL]
    [(string=? s "c1") (list-ref LOI 3)]
    [(string=? s "c2") (list-ref LOI 4)]
    [(string=? s "c3") (list-ref LOI 5)]
    [(string=? s "c4") (list-ref LOI 6)]
    [(string=? s "c5") (list-ref LOI 7)]
    [(string=? s "c6") (list-ref LOI 8)]
    [(string=? s "c7") (list-ref LOI 9)]
    [(string=? s "c8") (list-ref LOI 10)]
    [(string=? s "c9") (list-ref LOI 11)]
    [else #false]))

; draw-selected-ui : AppState -> Image
; draws a triangle pointing to the selected tools/tool values
; ie, a type, color, size, and mode
; (define (draw-selected-ui a) empty-image)
(define (draw-selected-ui a)
  (local ((define POINTER
            (rotate 270 (triangle 30 "solid" "light red")))
          ; get positions of selected options
          (define SIZE (tool-size (appstate-tool a)))
          (define TYPE-POSN (get-icon-posn (str->icon (tool-type (appstate-tool a)))))
          (define COLOR-POSN (get-icon-posn (str->icon (ui-sel (appstate-ui a)))))
          (define MODE-POSN (get-icon-posn (str->icon (tool-mode (appstate-tool a))))))
    (place-images
     (list POINTER
           POINTER
           POINTER
           (if (or (= SIZE 1)   ; check if
                   (= SIZE 8)   ; tool size
                   (= SIZE 15)  ; is one of the
                   (= SIZE 22)  ; predetermined
                   (= SIZE 30)) ; values
               POINTER       ; if yes, draw the pointer
               empty-image)) ; else, dont
     (list (make-posn (- (posn-x TYPE-POSN) 30) (posn-y TYPE-POSN))
           (make-posn (- (posn-x COLOR-POSN) 30) (posn-y COLOR-POSN))
           (make-posn (- (posn-x MODE-POSN) 30) (posn-y MODE-POSN))
           (local ((define XS-POSN (get-icon-posn XS))
                   (define S-POSN (get-icon-posn S))
                   (define M-POSN (get-icon-posn M))
                   (define L-POSN (get-icon-posn L))
                   (define XL-POSN (get-icon-posn XL)))
             (cond
               [(= SIZE 1)
                (make-posn (- (posn-x XS-POSN) 30) (posn-y XS-POSN))]
               [(= SIZE 8)
                (make-posn (- (posn-x S-POSN) 30) (posn-y S-POSN))]
               [(= SIZE 15)
                (make-posn (- (posn-x M-POSN) 30) (posn-y M-POSN))]
               [(= SIZE 22)
                (make-posn (- (posn-x L-POSN) 30) (posn-y L-POSN))]
               [(= SIZE 30)
                (make-posn (- (posn-x XL-POSN) 30) (posn-y XL-POSN))]
               [else (make-posn 0 0)])))
     (rectangle UI-W UI-H "solid" "transparent"))))

; ==================================================================================================
; Save Function

;; Input/Output
; save : AppState String -> AppState
; takes an appstate, saves the current canvas as PNG with the name String and returns the appstate unchanged
; header: (define (save app) #true)

(define (save app)
  (make-appstate (appstate-canvas app)                                   
                 (appstate-pre app)
                 (appstate-post app)
                 (appstate-tool app)
                 (appstate-ui app)
                 (not                     ; save-image returns a boolean so
                  (save-image             ; if the image can't be saved it quits the app
                   (appstate-canvas app)
                   (string-append         ; name of the saved image 
                    "img"
                    (saved-image-number 0)
                    ".png")))))

;; Input/Output
; saved-image-number : Number -> String
; returns the next number (represented as string) not already used for saved images
; header: (define (saved-image-number 0) "0")(start-app MAX-W MAX-H BG-COLOR)

(define (saved-image-number n)
  (cond
    [(file-exists? (string-append "img" (number->string n) ".png"))
     (saved-image-number (add1 n))]
    [else (number->string n)]))

; ==================================================================================================
; Mouse Handler

;; Input/output
; handle-mouse : MouseEvent AppState -> AppState
; it takes a MouseEvent input and returns the updated AppState

;; Code
(define (handle-mouse a x y me)
  (local (; appstate values
          (define canvas (appstate-canvas a))
          (define pre (appstate-pre a))
          (define post (appstate-post a))
          (define ui (appstate-ui a))
          (define hover (ui-hover ui))
          ; tool values
          (define type (tool-type (appstate-tool a)))
          (define size (tool-size (appstate-tool a)))
          (define color (tool-color (appstate-tool a)))
          (define mode (tool-mode (appstate-tool a)))
          (define X1 (tool-x1 (appstate-tool a)))
          (define Y1 (tool-y1 (appstate-tool a))))
    (cond
      ; is mouse in UI area?
      [(< 0 x UI-W)
       (cond
         [(string=? me "move")
          (update a "hover" (get-hover x y) #false)]
         [(string=? me "button-down")
          (select-from-ui a hover)] ; select option the mouse is hovering (if any)
         [else a])]
      ; is mouse in canvas area?
      [(and (<= x (image-width canvas))
            (<= y (image-height canvas)))
       (cond
         ; tool-type is a special tool ---------------------------------------------------
         ; freehand
         [(string=? type "free")
          (cond [(string=? me "button-down")
                 (make-appstate
                  canvas
                  pre
                  post
                  (make-tool
                   type
                   size
                   color
                   mode 
                   x y x y ; set coordinates
                   (list (make-posn x y)) ; begin list of coordinates
                   #true)
                  ui
                  (appstate-quit a))] 
                [(string=? me "drag")
                 (move-end-free a x y)] ; update end-points + create list of points
                [(string=? me "button-up")
                 (add-free-to-canvas a)]
                [else a])]
         ; eraser
         [(string=? type "eraser")
          (cond [(string=? me "button-down")
                 (make-appstate
                  canvas
                  pre
                  post
                  (make-tool
                   type
                   size
                   (get-color-canvas a)
                   mode 
                   x y x y 
                   (list (make-posn x y))
                   #true)
                  ui
                  (appstate-quit a))] 
                [(string=? me "drag")
                 (move-end-free a x y)] ; update end-points + create list of points
                [(string=? me "button-up")
                 (add-free-to-canvas a)]
                [else a])]
         ; fill
         [(string=? type "fill")
          (cond [(string=? me "button-down")
                 (fill a)]
                [else a])]
         ; tool-type is a figure ---------------------------------------------------------
         [(or (string=? type "line")
              (string=? type "square")
              (string=? type "rectangle")
              (string=? type "circle")
              (string=? type "ellipse"))
          ; check event
          (cond [(string=? me "button-down")
                 (make-appstate
                  canvas
                  pre
                  post
                  (make-tool type size color mode x y x y #false #true) ; set tool to active
                  ui
                  (appstate-quit a))]
                [(string=? me "drag")
                 (move-end a x y)]
                [(string=? me "button-up")
                 (add-figure-to-canvas a)]
                [else a])]
         [else a])]
      [else a])))

; ====================================================================================
; Key Handler

; handle-key: AppState KeyEvent -> AppState
; takes a key event and calls the function to perform the corresponding change
; to the current appstate/canvas
; (define (handle-key a k) a)

(define (handle-key a k)
  (local ((define SIZE (tool-size (appstate-tool a)))
          (define UI (appstate-ui a)))
    (cond [(string=? k "z") (undo a)]
          [(string=? k "x") (redo a)]
          [(string=? k "l") (update a "type" "line" #false)]
          [(string=? k "s") (update a "type" "square" #false)]
          [(string=? k "r") (update a "type" "rectangle" #false)]  
          [(string=? k "c") (update a "type" "circle" #false)]
          [(string=? k "o") (update a "type" "ellipse" #false)]
          [(string=? k "f") (update a "type" "free" #false)]
          [(string=? k "e") (update a "type" "eraser" #false)]
          [(string=? k "b") (update a "type" "fill" #false)]
          [(string=? k "wheel-up")
           (if (>= SIZE 30)   ; if current tool size is >= 30
               a              ; stop increasing
               (update a "size" 1 +))] ; else, continue
          [(string=? k "wheel-down") ; if current tool size <= 1
           (if (<= SIZE 1)     ; stop decreasing
               a               ; else, continue
               (update a "size" 1 -))]
          [(string=? k "1") (update a "sel" "c1" #false)]
          [(string=? k "2") (update a "sel" "c2" #false)]
          [(string=? k "3") (update a "sel" "c3" #false)]
          [(string=? k "4") (update a "sel" "c4" #false)]
          [(string=? k "5") (update a "sel" "c5" #false)]
          [(string=? k "6") (update a "sel" "c6" #false)]
          [(string=? k "7") (update a "sel" "c7" #false)]
          [(string=? k "8") (update a "sel" "c8" #false)]
          [(string=? k "9") (update a "sel" "c9" #false)]
          [(or (string=? k "up")
               (string=? k "down")
               (string=? k "left")
               (string=? k "right"))
           (change-canvas-size a k)]
          [(string=? k "k") (update a "mode" "solid" #false)]   ; change tool mode
          [(string=? k "j") (update a "mode" "outline" #false)] ; current keys are placeholder
          [(string=? k "a") (save a)]
          [(string=? k "q") (quit-app a)]
          [else a])))

; ==================================================================================================

;; Input/Output
; quit-app : AppState -> AppState
; set quit field of the appstate to #false
; header: (define (quit-app appstate) app)

(define (quit-app app)
  (make-appstate
   (appstate-canvas app)
   (appstate-pre app)
   (appstate-post app)
   (appstate-tool app)
   (appstate-ui app)
   #true))

;; Input/Output
; quit? : AppState -> Boolean
; it takes an AppState and returns a Boolean indicating whether the app has quit or not.
; header: (define (quit? appstate) #true)

;; Code
(define (quit? app) (appstate-quit app))

; ==================================================================================================

;; Input/Output
; start-app : Number Number Color -> AppState
; it takes a number n and starts the application with an initial size n
; header: (define (start-app n) app)

(define (start-app width height color)
  (draw-app (make-appstate
             (rectangle width height "solid" color)
             '()
             '()
             (make-tool "free" 1 "black" "outline" 0 0 0 0 #false #false)
             START-UI
             #false)))

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
