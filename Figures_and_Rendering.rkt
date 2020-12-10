;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Figures_and_Rendering) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define EMPTY-CANVAS (rectangle 1000 1000 "solid" "white"))

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

; an AppState is a structure (make-appstate canvas pre post tool quit)
; where canvas : Image
;       pre, post : List<Image>
;       tool : Tool
;       quit : Boolean
; interpretation: it represents the current state of the app
(define-struct appstate [canvas pre post tool quit])

; Examples of data
(define IS
  (make-appstate
   EMPTY-CANVAS
   '()
   '()
   (make-tool "free" 1 "black" "outline" 0 0 0 0 #false #false)
   #false))

; ========================================================================================

; update-tool : AppState String Any [X -> Y] -> AppState
; updates input field of current tool to the input value
; 'mod' is used to handle increasing (+) or decreasing (+) values
; (define (update-tool app field new-value mod) a)

(define (update-tool app field new-value mod)
  (local (; local values for better readability --------------
          ; appstate values
          (define CANVAS (appstate-canvas app))
          (define PRE (appstate-pre app))
          (define POST (appstate-post app))
          ; tool values
          (define TYPE (tool-type (appstate-tool app)))
          (define SIZE (tool-size (appstate-tool app)))
          (define COLOR (tool-color (appstate-tool app)))
          (define MODE (tool-mode (appstate-tool app)))
          (define X1 (tool-x1 (appstate-tool app)))
          (define Y1 (tool-y1 (appstate-tool app)))
          (define X2 (tool-x2 (appstate-tool app)))
          (define Y2 (tool-y2 (appstate-tool app)))
          (define EXTRA (tool-extra (appstate-tool app)))
          (define STATUS (tool-status (appstate-tool app))))
    ; --------------------------------------------------------
    (cond [(string=? field "type")
           (make-appstate CANVAS PRE POST (make-tool new-value SIZE COLOR MODE X1 Y1 X2 Y2 EXTRA STATUS) #false)]
          [(string=? field "size")
           (make-appstate CANVAS PRE POST (make-tool TYPE (mod SIZE new-value) COLOR MODE X1 Y1 X2 Y2 EXTRA STATUS) #false)]
          [(string=? field "color")
           (make-appstate CANVAS PRE POST (make-tool TYPE SIZE new-value MODE X1 Y1 X2 Y2 EXTRA STATUS) #false)]
          [(string=? field "mode")
           (make-appstate CANVAS PRE POST (make-tool TYPE SIZE COLOR new-value X1 Y1 X2 Y2 EXTRA STATUS) #false)]
          [else app])))

; Individual Applications of update-tool
; used in key/mouse events and on the User Interface

(define (new-type a nt) (update-tool a "type" nt #false))

(define (inc-size a) (update-tool a "size" 1 +))

(define (dec-size a) (update-tool a "size" 1 -))

(define (set-mode-solid a) (update-tool a "mode" "solid" #false))

(define (set-mode-outline a) (update-tool a "mode" "outline" #false))
 
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
       (appstate-quit app)))]))
                     

; ==================================================================================================

;; Input/Output
; render : AppState -> Image
; it takes an AppState and returns the current Canvas

;; Code
(define (render app)
  (local ((define type (tool-type (appstate-tool app))))
    (cond
      [(tool-status (appstate-tool app))
        (cond
          [(string=? type "free") (draw-free app)]
          [(string=? type "eraser") (draw-free app)]
          [(string=? type "line") (draw-line app)]
          [(string=? type "square") (draw-figure app)]
          [(string=? type "rectangle") (draw-figure app)]
          [(string=? type "circle") (draw-figure app)]
          [(string=? type "ellipse") (draw-figure app)]
          [else (appstate-canvas app)])]
      [else (appstate-canvas app)])))

;(define (render app)
;  (local ((define value (tool-extra (appstate-tool app))))
;    (overlay (cond [(false? value) (text "none" 20 "black")]
;                   [(cons? value)
;                    (text (number->string (length value)) 20 "black")])
;             (appstate-canvas app))))

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
  (local (; appstates values for readability
          (define canvas (appstate-canvas a))
          (define pre (appstate-pre a))
          (define post (appstate-post a))
          (define type (tool-type (appstate-tool a)))
          (define size (tool-size (appstate-tool a)))
          (define color (tool-color (appstate-tool a)))
          (define mode (tool-mode (appstate-tool a)))
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
    (make-appstate     ; "extra" is already holding a list after
            canvas     ; button-down, so update it with the
            pre        ; newest x and y values
            post
            (make-tool
             type
             size
             color
             mode
             X1 Y1 x y ; update coordinates
             (add-points a x y) ; add new points to freehand line
             #true)
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
                     (appstate-quit app))))

; ==================================================================================================
; Fill Tool

;; Input/Output
; add-fill-in : AppState Number Number -> AppState

(define (add-fill app x y)
  (local ((define COLOR1 (tool-color (appstate-tool app)))
          (define COLOR2 (get-color (appstate-canvas app) x y)))
  (cond
    [(color=? COLOR1 COLOR2) app]
    [else (make-appstate
           (fill (appstate-canvas app) x y COLOR1 COLOR2)
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
           (appstate-quit app))])))

;; Input/Output
; fill : Image Number Number Color Color -> Image 

(define (fill i x y c1 c2)
  (local ((define CXY (get-color i x y)))
  (cond
    [(or (> x (image-width i))  ; terminate when:
         (< x 0)                ; current point is out of bounds, 
         (> y (image-height i))
         (< y 0)
         (color=? c1 CXY)) ; the point is already of the right color.
     i]
    [(color=? c2 CXY) ; the color of the current point is the same as the point clicked initially
     (fill
      (fill
       (fill
        (fill
         (place-image (square 1 "solid" c1) (+ x 0.5) (+ y 0.5) i) ; color the current point
         x (- y 1) c1 c2) ; call fill on the point above the current one
        (+ x 1) y c1 c2) ; call fill on the point to the right of the current one
       x (+ y 1) c1 c2) ; call fill on the point below the current one
      (- x 1) y c1 c2)] ; call fill on the point to the left of the current one
    [else i])))


;; Input/Output
; color=? : Color Color -> Boolean
; takes two colors and returns #true if they're equal
; header: (define (color=? c1 c2) #true)

(define (color=? c1 c2)
  (equal? (circle 1 "solid" c1) (circle 1 "solid" c2)))

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
          ; tool values
          (define type (tool-type (appstate-tool a)))
          (define size (tool-size (appstate-tool a)))
          (define color (tool-color (appstate-tool a)))
          (define mode (tool-mode (appstate-tool a)))
          (define X1 (tool-x1 (appstate-tool a)))
          (define Y1 (tool-y1 (appstate-tool a))))
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
                mode ; use mode to temporarily store a list of posn
                x y x y
                (list (make-posn x y))
                #true)
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
                mode ; use mode to temporarily store a list of posn
                x y x y
                (list (make-posn x y))
                #true)
               (appstate-quit a))] 
             [(string=? me "drag")
              (move-end-free a x y)] ; update end-points + create list of points
             [(string=? me "button-up")
              (add-free-to-canvas a)]
             [else a])]
      ; fill
      [(string=? type "fill")
       (cond [(string=? me "button-up")
              (add-fill a x y)]
             [else a])]
     ;[(string=? type ...) (...)] -> for the other special tools
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
               (appstate-quit a))]
             [(string=? me "drag")
              (move-end a x y)]
             [(string=? me "button-up")
              (add-figure-to-canvas a)]
             [else a])]
      [else a])))

; ====================================================================================
; Key Handler

; handle-key: AppState KeyEvent -> AppState
; Takes a key event and calls the function to perform the corresponding change
; to the current appstate/canvas
; (define (handle-key a k) a)

(define (handle-key a k)
  (local ((define SIZE (tool-size (appstate-tool a))))
          (cond [(string=? k "u") (undo a)]
                [(string=? k "r") (redo a)]
                [(string=? k "l") (new-type a "line")]
                [(string=? k "s") (new-type a "square")]
                [(string=? k "t") (new-type a "rectangle")]  
                [(string=? k "c") (new-type a "circle")]
                [(string=? k "p") (new-type a "ellipse")]
                [(string=? k "f") (new-type a "free")]
                [(string=? k "e") (new-type a "eraser")]
                [(string=? k "i") (new-type a "fill")]
                [(string=? k "up")
                 (if (>= SIZE 30)   ; if current tool size is >= 30
                     a              ; stop increasing
                     (inc-size a))] ; else, continue
                [(string=? k "down") ; if current tool size <= 1
                 (if (<= SIZE 1)     ; stop decreasing
                     a               ; else, continuedrw
                     (dec-size a))]
                [(string=? k "1") (set-mode-solid a)]   ; change tool mode
                [(string=? k "2") (set-mode-outline a)] ; current keys are placeholder
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
