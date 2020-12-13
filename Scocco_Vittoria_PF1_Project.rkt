;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Scocco_Vittoria_PF1_Project) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

; Start app by using (start-app MAX-W MAX-H BG-COLOR START-UI)

; Constants
(define MAX-W 1800)
(define MAX-H 1000)
(define BG-COLOR "white")
(define EMPTY-CANVAS (rectangle MAX-W MAX-H "solid" BG-COLOR))

;; Data types

; a Tool is a structure (make-tool type size color mode x1 y1 x2 y2 status)
; where type : String
;       the tool type (freehand, line, etc)
;       size : Number
;       color : String
;       a color from Racket's Color Database
;       mode : String or Number
;       represents the mode for the figures ("outline" or "solid")
;       x1, y1, x2, y2 : Number
;       represent the coordinates of the current figure/tool
;       extra: Any
;       optional field used to store temporary values (eg: a list for freehand tool), otherwise it's set to #false
;       status : Boolean
;       represents if the tool is active
(define-struct tool [type size color mode x1 y1 x2 y2 extra status])

; a List<Image> is one of:
; - '()
; - (cons Image List<Image>)
; interpretation: a list of canvases

; a UI is a Structure (make-ui hover c1 c2 c3 c4 c5 c6 c7 c8 c9)
; where hover: Image
;       UI tool/option being hovered by mouse (if any, otherwise they're set to #false)
;       sel : String
;       holds latest selected palette color (useful for handling the eraser)
;       c1, c2, c3, c4, c5, c6, c7, c8, c9: Structure (make-color red green blue alpha)
;       they represent a palette of 9 predetermined colors
(define-struct ui [hover sel c1 c2 c3 c4 c5 c6 c7 c8 c9])

; Data Examples
(define START-UI (make-ui #false "c9" "lightcoral" "sandy brown" "gold" "aquamarine" "royal blue" "medium orchid" "pink" "white" "black"))

(define UI1 (make-ui #false "c1" "red" "orange" "yellow" "green" "blue" "violet" "pink" "white" "black"))

(define UI2 (make-ui #false "c1" "brown" "purple" "gray" "light blue" "light green" "light red" "black" "white" "blue"))


; Palette examples (to be used with start-app)
(define REDS (make-ui #false "c1" "orange red" "tomato" "dark red" "red" "firebrick" "crimson" "deep pink" "maroon" "indian red"))

(define BLUES (make-ui #false "c1" "medium blue" "cornflower blue" "dark blue" "midnight blue" "navy" "blue" "indigo" "teal" "royal blue"))

(define GREENS (make-ui #false "c1" "green" "lawn green" "chartreuse" "green yellow" "medium forest green" "olive drab" "lime" "dark green" "pale green"))

(define YELLOWS (make-ui #false "c1" "yellow" "khaki" "wheat" "beige" "light yellow" "moccasin" "papaya whip" "cornsilk" "gold"))

; an AppState is a Structure (make-appstate canvas pre post tool ui quit)
; where canvas : Image
;       a drawing canvas with the latest changes
;       pre, post : List<Image>
;       changes done/undone to the canvas
;       tool : Tool
;       the current tool and its corresponding values (size, color, etc)
;       ui: Structure
;       information to be displayed on the ui (selected options, color palette, etc)
;       quit : Boolean
;       whether the app has quit or not
; interpretation: it represents the current state of the app
(define-struct appstate [canvas pre post tool ui quit])

; Examples of data
; A base AppState should have both pre and post as empty lists, and tool set with standard values like in IS
(define IS
  (make-appstate
   EMPTY-CANVAS
   '()
   '()
   (make-tool "free" 1 (ui-c9 START-UI) "outline" 0 0 0 0 #false #false)
   START-UI
   #false))

(define IS0
  (make-appstate
   (rectangle 900 900 "solid" "white")
   '()
   '()
   (make-tool "free" 1 (ui-c9 UI1) "outline" 0 0 0 0 #false #false)
   UI1
   #false))

(define IS1
  (make-appstate
   (rectangle 1200 700 "solid" "gray")
   '()
   '()
   (make-tool "line" 8 (ui-c1 UI2) "solid" 5 10 90 100 #false #true)
   UI2
   #false))

(define IS2
  (make-appstate
   (rectangle 250 90 "solid" "light gray")
   '()
   '()
   (make-tool "line" 30 (ui-c9 UI2) "solid" 65 80 200 5 #false #false)
   UI2
   #false))

; AppState used for testing freehand
(define ISF
  (make-appstate
   (rectangle 900 900 "solid" "white")
   '()
   '()
   (make-tool "free" 8 "black" "solid"
              90 100 95 105
              (list (make-posn 90 100) (make-posn 85 95))
              #true)
   START-UI
   #true))

; AppState for testing undo/redo
(define ISUR
  (make-appstate
   (rectangle 500 500 "solid" "orange")
   (list (rectangle 500 500 "solid" "red"))
   (list (rectangle 500 500 "solid" "green"))
   (make-tool "free" 8 "black" "solid" 0 0 0 0 #false #false)
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

; Note: Changing tool color has to be done by updating "sel" in ui, not by updating "color" directly
(check-expect (update IS "sel" "c5" #false)
              (make-appstate
               EMPTY-CANVAS
               '()
               '()
               (make-tool "free" 1 "royal blue" "outline" 0 0 0 0 #false #false)
               (make-ui #false "c5" "lightcoral" "sandy brown" "gold" "aquamarine" "royal blue" "medium orchid" "pink" "white" "black")
               #false))

(check-expect (update IS0 "hover" UNDO #false)
              (make-appstate
               (rectangle 900 900 "solid" "white")
               '()
               '()
               (make-tool "free" 1 (ui-c9 UI1) "outline" 0 0 0 0 #false #false)
               (make-ui UNDO "c1" "red" "orange" "yellow" "green" "blue" "violet" "pink" "white" "black")
               #false))

; Status cannot be changed via update, so the resulting appstate doesn't change
(check-expect (update IS0 "status" #true #false)
              (make-appstate
               (rectangle 900 900 "solid" "white")
               '()
               '()
               (make-tool "free" 1 (ui-c9 UI1) "outline" 0 0 0 0 #false #false)
               UI1
               #false))

; Code
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
        (if (string=? field "sel") new-value (ui-sel UI))   ; setting selected color (used for eraser)
        C1 C2 C3 C4 C5 C6 C7 C8 C9)
       (appstate-quit a))))
 
; ==================================================================================================

;; Input/Output
; get-color : Image Number Number -> Color
; takes an image and two coordinates and returns the color in that point of the image
; header: (define (get-color img x y) "red")

;; Examples
(check-expect (get-color (square 1 "solid" "white") 0 0)
              (make-color 255 255 255))
(check-expect (get-color (square 1 "solid" "green") 0 0)
              (make-color 0 255 0))

; Code
(define (get-color i x y)
  (first (image->color-list
          (crop x y 1 1 i))))

;; Iput/Output
; get-color-canvas : AppState -> Color
; takes an appstate and returns the initial color of the canvas
; header: (define (get-color-canvas app) "red")

;; Examples
(check-expect (get-color-canvas IS) (make-color 255 255 255))

;; Code
(define (get-color-canvas app)
  (cond
    [(empty? (appstate-pre app))
     (get-color (appstate-canvas app) 1 1)]
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

; Examples
; IS has canvas size at its maximum, so nothing changes
(check-expect (change-canvas-size IS "right")
              (make-appstate
               EMPTY-CANVAS
               '()
               '()
               (make-tool "free" 1 (ui-c9 START-UI) "outline" 0 0 0 0 #false #false)
               START-UI
               #false))

(check-expect (change-canvas-size IS0 "right")
              (make-appstate
               (overlay/align "left" "middle"
                              (rectangle 900 900 "solid" "white")
                              (rectangle 950 900 "solid" "white"))
               (cons (appstate-canvas IS0) '())
               '()
               (make-tool "free" 1 (ui-c9 UI1) "outline" 0 0 0 0 #false #false)
               UI1
               #false))

(check-expect (change-canvas-size IS1 "up")
              (make-appstate
               (crop 0 0 1200 650 (appstate-canvas IS1))
               (cons (appstate-canvas IS1) '())
               '()
               (appstate-tool IS1)
               UI2
               #false))

; IS2's canvas is already at a minimum, so it isn't reduced
(check-expect (change-canvas-size IS2 "left")
              (make-appstate
               (rectangle (- UI-W 50) 90 "solid" "light gray")
               '()
               '()
               (make-tool "line" 30 (ui-c9 UI2) "solid" 65 80 200 5 #false #false)
               UI2
               #false))

; Code
(define (change-canvas-size a k)
  (local ((define WIDTH (image-width (appstate-canvas a)))
          (define HEIGHT (image-height (appstate-canvas a)))
          (define RIGHT (and (string=? k "right") ; check width isnt at its maximum
                             (< WIDTH MAX-W)))
          (define DOWN (and (string=? k "down")  ; check height isnt at its maximum
                            (< HEIGHT MAX-H)))
          (define LEFT (and (string=? k "left")
                            (< UI-W WIDTH))) ; check width isnt at its minimum
          (define UP (and (string=? k "up")
                          (< 100 HEIGHT)))) ; check height isnt at its minimum
    (make-appstate
       (cond [RIGHT
              (overlay/align "left" "middle"
                             (appstate-canvas a)
                             (rectangle (+ WIDTH 50) HEIGHT
                                        "solid" (get-color (appstate-canvas a) 0 0)))]
             [DOWN
              (overlay/align "middle" "top"
                             (appstate-canvas a)
                             (rectangle WIDTH (+ HEIGHT 50)
                                        "solid" (get-color (appstate-canvas a) 0 0)))]
             [LEFT
              (crop 0 0 (- WIDTH 50) HEIGHT (appstate-canvas a))]
             [UP
              (crop 0 0 WIDTH (- HEIGHT 50) (appstate-canvas a))]
             [else (appstate-canvas a)])
       (if (or RIGHT ; if the canvas
               DOWN  ; was increased
               LEFT ; or decreased
               UP)
           (cons (appstate-canvas a) (appstate-pre a)) ; cons canvas to pre
           (appstate-pre a)) ; else, leave pre as is
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
     (list (draw-selected-ui app)
           (draw-ui app)
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
     (list (make-posn (/ UI-W 2) (/ UI-H 2))
           (make-posn (/ UI-W 2) (/ UI-H 2))
           (make-posn (/ (image-width (appstate-canvas app)) 2)
                      (/ (image-height (appstate-canvas app)) 2)))
     (empty-scene MAX-W MAX-H "gray"))))

; ===================================================================================
; Line Tool

; draw-line: AppState Number Number -> Image
; add current line to canvas
; (define (draw-line a x y) EMPTY-CANVAS)

; Examples
(check-expect (draw-line IS1)
              (add-line
               (rectangle 1200 700 "solid" "gray")
               5 10 90 100
               (pen "brown" 8 "solid" "round" "round")))

(check-expect (draw-line IS2)
              (add-line
               (rectangle 250 90 "solid" "light gray")
               65 80 200 5
               (pen "blue" 30 "solid" "round" "round")))

; Code
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

; Examples
(check-expect (move-end-free ISF 100 110)
              (make-appstate
               (rectangle 900 900 "solid" "white")
               '()
               '()
               (make-tool
                "free" 8 "black" "solid"
                90 100 100 110
                (list (make-posn 100 110) (make-posn 90 100) (make-posn 85 95))
                #true)
               START-UI
               #false))

; case where a new point isnt added to list of points stored in extra
(check-expect (move-end-free ISF 93 103)
              (make-appstate
               (rectangle 900 900 "solid" "white")
               '()
               '()
               (make-tool
                "free" 8 "black" "solid"
                90 100 93 103
                (list (make-posn 90 100) (make-posn 85 95)) ; new x and y are close, so dont add them to list
                #true)
               START-UI
               #false))

(check-expect (move-end-free ISF 150 120)
              (make-appstate
               (rectangle 900 900 "solid" "white")
               '()
               '()
               (make-tool
                "free" 8 "black" "solid"
                90 100 150 120
                (list (make-posn 150 120) (make-posn 90 100) (make-posn 85 95))
                #true)
               START-UI
               #false))

; Code
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

; Examples
(check-expect (draw-free ISF)
              (add-line
               (place-image
                (circle 4 "solid" "black")
                85 95
                (appstate-canvas ISF))
               85 95 90 100
               (pen "black" 8 "solid" "round" "round")))

; Code
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

; Examples
(check-expect (add-free-to-canvas ISF)
              (make-appstate
               (add-line                                 ; the new
                (place-image                             ; canvas is
                 (circle 4 "solid" "black")              ; created
                 85 95                                   ; by using
                 (appstate-canvas ISF))                  ; draw-free
                85 95 90 100                             ; like
                (pen "black" 8 "solid" "round" "round")) ; above
               (cons (rectangle 900 900 "solid" "white") '())
               '()
               (make-tool "free" 8 "black" "solid"
                          0 0 0 0
                          #false  ; set extra to #false
                          #false) ; set tool to not active
               START-UI
               #true))

; Code
(define (add-free-to-canvas a)
  (local ((define TYPE (tool-type (appstate-tool a)))
          (define SEL (ui-sel (appstate-ui a))) 
          (define C1 (ui-c1 (appstate-ui a)))
          (define C2 (ui-c2 (appstate-ui a)))
          (define C3 (ui-c3 (appstate-ui a)))
          (define C4 (ui-c4 (appstate-ui a)))
          (define C5 (ui-c5 (appstate-ui a)))
          (define C6 (ui-c6 (appstate-ui a)))
          (define C7 (ui-c7 (appstate-ui a)))
          (define C8 (ui-c8 (appstate-ui a)))
          (define C9 (ui-c9 (appstate-ui a))))
  (make-appstate
   (draw-free a) ; use draw-free to get the canvas with the freehand line drawn
   (cons (appstate-canvas a)
         (appstate-pre a))
   '()
   (make-tool (tool-type (appstate-tool a))
              (tool-size (appstate-tool a))
              (cond                         ; set the color back to the one selected 
                [(string=? SEL "c1") C1]
                [(string=? SEL "c2") C2]
                [(string=? SEL "c3") C3]
                [(string=? SEL "c4") C4]
                [(string=? SEL "c5") C5]
                [(string=? SEL "c6") C6]
                [(string=? SEL "c7") C7]
                [(string=? SEL "c8") C8]
                [(string=? SEL "c9") C9]
                [else (tool-color (appstate-tool a))])
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

;; Examples
(check-expect (draw-figure
               (make-appstate
                EMPTY-CANVAS
                '()
                '()
                (make-tool "square" 1 (ui-c9 START-UI) "outline" 0 0 20 30 #false #false)
                START-UI
                #false))
               (place-image (square 20 "outline" "black") 10 10 EMPTY-CANVAS))

(check-expect (draw-figure
               (make-appstate
                EMPTY-CANVAS
                '()
                '()
                (make-tool "ellipse" 1 "green" "solid" 50 30 100 150 #false #false)
                START-UI
                #false))
               (place-image (ellipse 50 120 "solid" "green") 75 90 EMPTY-CANVAS))

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

;; Examples
(check-expect (figure-height
               (make-tool "circle" 1 (ui-c9 START-UI) "outline" 0 0 20 30 #false #false))
              20)

(check-expect (figure-height
               (make-tool "rectangle" 1 (ui-c9 START-UI) "outline" 0 0 20 30 #false #false))
              30)

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

;; Examples
(check-expect (figure-width
               (make-tool "square" 1 (ui-c9 START-UI) "outline" 20 30 0 0 #false #false))
              20)
(check-expect (figure-width
               (make-tool "ellipse" 1 (ui-c9 START-UI) "outline" 20 30 0 0 #false #false))
              20)

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

;; Examples
(check-expect (add-figure-to-canvas
               (make-appstate
                EMPTY-CANVAS
                '()
                '()
                (make-tool "square" 1 (ui-c9 START-UI) "outline" 0 0 20 30 #false #false)
                START-UI
                #false))
              (make-appstate
                (place-image (square 20 "outline" "black") 10 10 EMPTY-CANVAS)
                (cons EMPTY-CANVAS '())
                '()
                (make-tool "square" 1 (ui-c9 START-UI) "outline" 0 0 0 0 #false #false)
                START-UI
                #false)) 

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

;; Examples
(check-expect (move-end
               (make-appstate
                EMPTY-CANVAS
                '()
                '()
                (make-tool "square" 1 (ui-c9 START-UI) "outline" 0 0 20 30 #false #false)
                START-UI
                #false)
               50 80)
              (make-appstate
                EMPTY-CANVAS
                '()
                '()
                (make-tool "square" 1 (ui-c9 START-UI) "outline" 0 0 50 80 #false #false)
                START-UI
                #false))  

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

; fill : AppState -> AppState
; it takes an appstate and returns the appstate with the canvas filled with the selected color

;; Examples
(check-expect (fill
               (make-appstate
                EMPTY-CANVAS
                '()
                '()
                (make-tool "square" 1 "blue" "outline" 0 0 0 0 #false #false)
                START-UI
                #false))
              (make-appstate
                (rectangle 1800 1000 "solid" "blue")
                (cons EMPTY-CANVAS '())
                '()
                (make-tool "square" 1 "blue" "outline" 0 0 0 0 #false #false)
                START-UI
                #false))

; Code
(define (fill app)
  (local ((define COLOR (tool-color (appstate-tool app))))
    (make-appstate 
     (rectangle (image-width (appstate-canvas app))
                (image-height (appstate-canvas app))
                "solid" COLOR)
     (cons (appstate-canvas app)
           (appstate-pre app))
     '()
     (make-tool
      (tool-type (appstate-tool app))
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

; undo : AppState -> AppState
; undoes the last action made to the canvas
; ie, sets the first element of pre to be the new canvas and adjusts pre,post accordingly

; Examples
(check-expect (undo ISUR)
              (make-appstate
               (rectangle 500 500 "solid" "red")
               '()
               (list (rectangle 500 500 "solid" "orange")
                     (rectangle 500 500 "solid" "green"))
               (appstate-tool ISUR)
               START-UI
               #false))

(check-expect (undo IS) IS) ; IS has both pre,post empty

; Code
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

; redo : AppState -> AppState
; restores a change made to the canvas that was undone
; ie, sets the first element of post to be the new canvas, and adjusts pre, post accordingly

; Examples
(check-expect (redo ISUR)
              (make-appstate
               (rectangle 500 500 "solid" "green")
               (list (rectangle 500 500 "solid" "orange")
                     (rectangle 500 500 "solid" "red"))
               '()
               (appstate-tool ISUR)
               START-UI
               #false))

(check-expect (redo IS) IS) ; IS has both pre,post empty

; Code
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
              (text/font "Save" 22 "cornflower blue"
                         "Gill Sans" "default" "normal" "bold" #f)
              (rectangle (- (/ UI-W 4) 5) 50 "outline" TOP-PEN)
              (rectangle (- (/ UI-W 4) 5) 50 "solid" "white smoke")))

(define LOAD (overlay
              (text/font "Load" 22 "cornflower blue"
                         "Gill Sans" "default" "normal" "bold" #f)
              (rectangle (- (/ UI-W 4) 5) 50 "outline" TOP-PEN)
              (rectangle (- (/ UI-W 4) 5) 50 "solid" "white smoke")))

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

; Getting information to draw on UI -----389357de7e4164ad8725893d9fa69a3ac283abfa------------------------------------------

; make-loic : AppState -> List<Any>
; returns a list of all icons present, using the given
; appstate to compute the palette icons with their corresponding color

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
    (list SAVE (make-posn 40 25)
          LOAD (make-posn 110 25)
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
          FILL (make-posn 210 800))))

(define (draw-ui a)
  (local ((define LIST (make-loic a)))
    (place-images
     (filter image? LIST)
     (filter posn? LIST)
     UI-BG)))

; Getting info from the UI -------------------------------------------------------------------

; get-icon : AppState Image -> Posn
; return the location (centered) of the icon as a posn

; Examples

(check-expect (get-icon-posn IS SAVE) (make-posn 40 25))

(check-expect (get-icon-posn IS FILL) (make-posn 210 800))

(check-expect (get-icon-posn IS SOLID-ICON) (make-posn 155 310))

; Code
(define (get-icon-posn a icon)
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
    (get-posn icon
              (filter image? (make-loic a))
              (filter posn? (make-loic a)))))

; get-hover : AppState Integer Integer -> Image
; return the tool/option icon the mouse is currently hovering on the UI

; Examples
(check-expect (get-hover IS 10 10) SAVE)

(check-expect (get-hover IS 260 340) XL)

(check-expect (get-hover IS 800 25) #false) ; x value outside UI area

(check-expect (get-hover IS 85 810) ERASER)

(check-expect (get-hover IS 195 445) RECTANGLE-ICON)

(check-expect (get-hover IS 95 455) SQUARE-ICON)

; Code
(define (get-hover a x y)
  (local ((define (X1 i) (- (posn-x (get-icon-posn a i)) (/ (image-width i) 2))) ; left-hand side
          (define (X2 i) (+ (posn-x (get-icon-posn a i)) (/ (image-width i) 2))) ; right-hand side
          (define (Y1 i) (- (posn-y (get-icon-posn a i)) (/ (image-height i) 2))) ; top of icon
          (define (Y2 i) (+ (posn-y (get-icon-posn a i)) (/ (image-height i) 2))) ; bottom of icon
          ; find-hover : List<Image> Integer Integer -> Maybe<Image>
          ; if mouse is hovering an icon, return that image. if not, return #false
          (define (find-hover loi x y)
            (cond
              [(empty? loi) #false]
              [(and (< (X1 (first loi)) x (X2 (first loi)))
                    (< (Y1 (first loi)) y (Y2 (first loi))))
               (first loi)]
              [else (find-hover (rest loi) x y)])))
    (find-hover (filter image? (make-loic a)) x y)))
  
; select-from-ui : AppState Image -> AppState
; updates the appstate according to selection in UI (button-down where the mouse is hovering)

; Examples

(check-expect (select-from-ui
               (make-appstate
                EMPTY-CANVAS
                '() '()
                (make-tool "free" 30 "black" "solid" 0 0 0 0 #false #false)
                (make-ui XS "c9" "red" "orange" "yellow" "green" "blue" "violet" "pink" "white" "black")
                #false))
              (make-appstate ; FILL is a valid hover option, so tool type is set to fill
                EMPTY-CANVAS
                '() '()
                (make-tool "free" 1 "black" "solid" 0 0 0 0 #false #false)
                (make-ui XS "c9" "red" "orange" "yellow" "green" "blue" "violet" "pink" "white" "black")
                #false))

(check-expect (select-from-ui
               (make-appstate
                EMPTY-CANVAS
                '() '()
                (make-tool "free" 30 "black" "solid" 0 0 0 0 #false #false)
                (make-ui CIRCLE-ICON "c9" "red" "orange" "yellow" "green" "blue" "violet" "pink" "white" "black")
                #false))
              (make-appstate ; hover is circle, so set tool to circle
                EMPTY-CANVAS
                '() '()
                (make-tool "circle" 30 "black" "solid" 0 0 0 0 #false #false)
                (make-ui CIRCLE-ICON "c9" "red" "orange" "yellow" "green" "blue" "violet" "pink" "white" "black")
                #false))

(check-expect (select-from-ui
               (make-appstate
                EMPTY-CANVAS
                '() '()
                (make-tool "free" 1 "black" "solid" 0 0 0 0 #false #false)
                (make-ui #false "c9" "red" "orange" "yellow" "green" "blue" "violet" "pink" "white" "black") ; hover is #false
                #false))
              (make-appstate ; hover is #false, so return the appstate unchanged
                EMPTY-CANVAS
                '() '()
                (make-tool "free" 1 "black" "solid" 0 0 0 0 #false #false)
                (make-ui #false "c9" "red" "orange" "yellow" "green" "blue" "violet" "pink" "white" "black") ; hover is #false
                #false))

; Code
(define (select-from-ui a)
  (local ((define C1 (color-icon (ui-c1 (appstate-ui a))))
          (define C2 (color-icon (ui-c2 (appstate-ui a))))
          (define C3 (color-icon (ui-c3 (appstate-ui a))))
          (define C4 (color-icon (ui-c4 (appstate-ui a))))
          (define C5 (color-icon (ui-c5 (appstate-ui a))))
          (define C6 (color-icon (ui-c6 (appstate-ui a))))
          (define C7 (color-icon (ui-c7 (appstate-ui a))))
          (define C8 (color-icon (ui-c8 (appstate-ui a))))
          (define C9 (color-icon (ui-c9 (appstate-ui a))))
          (define hover (ui-hover (appstate-ui a))))
  (cond
    [(false? hover) a]
    [(equal? hover SAVE)
     (save a)]
    [(equal? hover LOAD)
     (load a)]
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
    [(equal? hover RECTANGLE-ICON)
     (update a "type" "rectangle" #false)]
    [(equal? hover SQUARE-ICON)
     (update a "type" "square" #false)]
    [(equal? hover CIRCLE-ICON)
     (update a "type" "circle" #false)]
    [(equal? hover ELLIPSE-ICON)
     (update a "type" "ellipse" #false)]
    [(equal? hover LINE-ICON)
     (update a "type" "line" #false)]
    [(equal? hover FREE-ICON)
     (update a "type" "free" #false)]
    [(equal? hover ERASER)
     (update a "type" "eraser" #false)]
    [(equal? hover FILL)
     (update a "type" "fill" #false)]
    [else a])))

; UI Rendering --------------------------------------------------------------------

; str->icon : AppState String -> Image
; take a string and return the corresponding icon it represents

; Examples
(check-expect (str->icon IS "save") SAVE)

(check-expect (str->icon IS "solid") SOLID-ICON)

(check-expect (str->icon IS "pencil") #false)

(check-expect (str->icon IS "eraser") ERASER)

; Code
(define (str->icon a s)
  (cond
    [(string=? s "save") SAVE]
    [(string=? s "load") LOAD]
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
    [(string=? s "c1") (list-ref (filter image? (make-loic a)) 4)]
    [(string=? s "c2") (list-ref (filter image? (make-loic a)) 5)]
    [(string=? s "c3") (list-ref (filter image? (make-loic a)) 6)]
    [(string=? s "c4") (list-ref (filter image? (make-loic a)) 7)]
    [(string=? s "c5") (list-ref (filter image? (make-loic a)) 8)]
    [(string=? s "c6") (list-ref (filter image? (make-loic a)) 9)]
    [(string=? s "c7") (list-ref (filter image? (make-loic a)) 10)]
    [(string=? s "c8") (list-ref (filter image? (make-loic a)) 11)]
    [(string=? s "c9") (list-ref (filter image? (make-loic a)) 12)]
    [else #false]))


; draw-selected-ui : AppState -> Image
; draws a triangle pointing to the selected tools/tool values
; ie, a type, color, size, and mode

; Examples

(check-expect (draw-selected-ui IS)
              (place-images
               (list (rotate 270 (triangle 30 "solid" "light red"))
                     (rotate 270 (triangle 30 "solid" "light red"))
                     (rotate 270 (triangle 30 "solid" "light red"))
                     (rotate 270 (triangle 30 "solid" "light red")))
               (list
                (make-posn 170 650) ; type
                (make-posn 135 220) ; color
                (make-posn 30 310)  ; mode
                (make-posn 225 95)) ; size
               (rectangle UI-W UI-H "solid" "transparent")))

; Code
(define (draw-selected-ui a)
  (local ((define POINTER
            (rotate 270 (triangle 30 "solid" "light red")))
          ; get positions of selected options
          (define SIZE (tool-size (appstate-tool a)))
          (define TYPE-POSN (get-icon-posn a (str->icon a (tool-type (appstate-tool a)))))
          (define COLOR-POSN (get-icon-posn a (str->icon a (ui-sel (appstate-ui a)))))
          (define MODE-POSN (get-icon-posn a (str->icon a (tool-mode (appstate-tool a))))))
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
           (local ((define XS-POSN (get-icon-posn a XS))
                   (define S-POSN (get-icon-posn a S))
                   (define M-POSN (get-icon-posn a M))
                   (define L-POSN (get-icon-posn a L))
                   (define XL-POSN (get-icon-posn a XL)))
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
; save : AppState -> AppState
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
; Load Function

;; Input/Output
; load : Image -> AppState
; takes an image, loads a PNG image to the current canvas and returns the appstate with the loaded image.
; header: (define (load app) #true)

(define (load app)
  (make-appstate (bitmap/file (string-append "img" (loaded-image-number 0 1) ".png"))                                   
                 (appstate-pre app)
                 (appstate-post app)
                 (appstate-tool app)
                 (appstate-ui app)
                 #false))

;; Input/Output
; loaded-image-number : Number -> String
; returns the last (represented as string) image
; header: (define (loaded-image-number 0 1) )

(define (loaded-image-number m o)
  (cond
    [(file-exists? (string-append "img" (number->string o) ".png"))
     (loaded-image-number (add1 m) (add1 o))]
    [else (number->string m)]))

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
          ; tool values389357de7e4164ad8725893d9fa69a3ac283abfa
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
          (update a "hover" (get-hover a x y) #false)]
         [(string=? me "button-down")
          (select-from-ui a)] ; select option the mouse is hovering (if any)
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
          [(string=? k "d") (load a)]
          [(string=? k "q") (quit-app a)]
          [else a])))

; ==================================================================================================

;; Input/Output
; quit-app : AppState -> AppState
; set quit field of the appstate to #false
; header: (define (quit-app appstate) app)

;; Examples
(check-expect (quit-app IS)
              (make-appstate
               EMPTY-CANVAS
               '()
               '()
               (make-tool "free" 1 (ui-c9 START-UI) "outline" 0 0 0 0 #false #false)
               START-UI
               #true))

;; Code
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

;; Examples
(check-expect (quit?
               (make-appstate
                EMPTY-CANVAS
                '()
                '()
                (make-tool "free" 1 (ui-c9 START-UI) "outline" 0 0 0 0 #false #false)
                START-UI
                #true))
              #true)

;; Code
(define (quit? app) (appstate-quit app))

; ==================================================================================================

;; Input/Output
; start-app : Number Number Color -> AppState
; it takes a number n and starts the application with an initial size n
; header: (define (start-app n) app)

;; Code
(define (start-app width height color palette)
  (draw-app (make-appstate
             (rectangle width height "solid" color)
             '()
             '()
             (make-tool "free" 1 "black" "outline" 0 0 0 0 #false #false)
             palette
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