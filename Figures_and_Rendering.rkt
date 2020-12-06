;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Figures_and_Rendering) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define EMPTY-CANVAS (empty-scene 1000 1000))

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
   (make-tool "square" 1 "black" "outline" 0 0 0 0 #false #false)
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
; render : AppState -> Image
; it takes an AppState and returns the current Canvas

;; Code
(define (render0 app)
  (local ((define type (tool-type (appstate-tool app))))
    (cond
      [(tool-status (appstate-tool app))
        (cond
          [(string=? type "free") (draw-free app)]
          [(string=? type "line") (draw-line app)]
          [(string=? type "square") (draw-square app)]
          ;[(string=? type "rectangle") (draw-rectangle)]
          [else (appstate-canvas app)])]
      [else (appstate-canvas app)])))

(define (render app)
  (local ((define value (tool-extra (appstate-tool app))))
    (overlay (cond [(false? value) (text "none" 20 "black")]
                   [(cons? value)
                    (text (number->string (length value)) 20 "black")])
             (appstate-canvas app))))

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
             "free"
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
  (make-appstate
   (draw-free a) ; use draw-free to get the canvas with the freehand line drawn
   (cons (appstate-canvas a)
         (appstate-pre a))
   '()
   (make-tool (tool-type (appstate-tool a))
              (tool-size (appstate-tool a))
              (tool-color (appstate-tool a))
              "solid"
              0 0 0 0  ; x1,y1,x2,y2 set to 0
              #false   ; set extra to #false
              #false)  ; tool set to not active
   (appstate-quit a)))

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
    (make-appstate
     (cond
       [(string=? type "square") (draw-square app)]
       [(string=? type "line") (draw-line app)]
       ;[(string=? type "rectangle") (add-rectangle-to-canvas app)]
    
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
                [(string=? k "f") (new-type a "free")]
                [(string=? k "up")
                 (if (>= SIZE 15)   ; if current tool size is >= 10
                     a              ; stop increasing
                     (inc-size a))] ; else, continue
                [(string=? k "down") ; if current tool size <= 1
                 (if (<= SIZE 1)     ; stop decreasing
                     a               ; else, continue
                     (dec-size a))]
                [(string=? k "1") (set-mode-solid a)]   ; change tool mode
                [(string=? k "2") (set-mode-outline a)] ; current keys are placeholder
                [else a])))

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