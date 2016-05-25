#lang racket/gui
(require racket/math)
(require 2htdp/universe)
(require 2htdp/image)
(require "world.rkt")
(require "PlayerClass.rkt")

;; FILE: main.rkt
;;
;; DESCRIPTION: Contains everything on the menu window and is the starting file.
;;              
;; LAST CHANGE: 2016-05-25 (by Jakob).
;;
;; Written by: Jakob Jerrelind.




;; Frame for the menuwindow
(define startwindow
  (new frame%
       [label "Startscreen Garde La Courbe"]
       [width 650]
       [height 800]))

;; Bitmap header to the menuwindow
(define *our-bitmap* (make-object bitmap% "Bild.png"))

;; Draws the bitmap
(define (our-draw canvas dc)
  (send dc draw-bitmap *our-bitmap* 0 0))

;; Canvas on the menuwindow
(define start-screen
  (new canvas%
       [parent startwindow]
       [paint-callback our-draw]))

;; Shows the menuwindow
(send startwindow show #t)

;; Panel in menuwindow 
(define panel (new group-box-panel%
                   [label "Meny"]
                   [parent startwindow]
                   [alignment '(right center)]))
;; Subpanel in menuwindow
(define subpanel (new group-box-panel%
                      [label " "]
                      [parent panel]
                      [alignment '(left center)]))
;; Help frame
(define helpframe (new frame%
                       [label "Help"]
                       [width 550]
                       [height 200]))

;; The "Help" button 
(define (help button event)
  (send helpframe show #t))
(new button%
     [label "Help"]
     [parent panel]
     [callback help])

;; Button to quit the menuwindow
(define (quit-startframe button event)
  (send startwindow show #f))
(new button%
     [label "Quit"]
     [parent panel]
     [callback quit-startframe])

;; Quit button for the game-frame
(define (quit-gameframe button event)
  (send game-frame show #f)
  (send *graphics-timer* stop)
    (send *game-timer* stop))
(new button%
     [label "Quit"]
     [parent game-frame]
     [callback quit-gameframe])



 ;; FUNCTION: one-player
    ;;
    ;; DESCRIPTION: Function called from the "One Snake"-buttons callback, and sets how many snakes to play with.
    ;;              
    ;;
    ;; INPUT: Button and event.
    ;;
    ;; OUTPUT: None.
(define (one-player button event)
  (begin
  (send (car snake-list) include) ;; Shows the first snake
  (send one-snake set-label "You have chosen one player, start game!  ") ;; Label when pressed
  (send button set-label "Marked") ;; New button label
  (send playerwindow show #f) ;; Playerwindow to #f
  (send (cadr snake-list) remove) ;; Set show to #f for player2 
  (send (caddr snake-list) remove) ;; Set show to #f for player3
  (send twosnake set-label "Two Snakes") ;; Change button label
  (send threesnake set-label "Three Snakes") ;; Change button label
  (send two-snakes set-label "Press to play with two Snakes                     ") ;; Revert to origin label
  (send three-snakes set-label "Press to play with three Snakes                     ") ;; Revert to origin label
    (send (cadr snake-list) kill) ;; Set alive to #f
    (send (caddr snake-list) kill))) ;; Set alive to #f

;; FUNCTION: two-player
    ;;
    ;; DESCRIPTION: Function called from the "Two Snakes"-buttons callback, and sets how many snakes to play with.
    ;;              
    ;;
    ;; INPUT: Button and event.
    ;;
    ;; OUTPUT: None.
(define (two-players button event)
  (begin
  (send (car snake-list) include) ;; Show first snake
  (send (cadr snake-list) include) ;; Show second snake
  (send two-snakes set-label "You have chosen two players, start game!  ") ;; Label when pressed
  (send button set-label "Marked") ;; Button label
  (send playerwindow show #f) ;; Playerwindow to #f
  (send (caddr snake-list) remove) ;; Sets show to #f for the third snake
  (send onesnake set-label "One Snake") ;; Change button label
  (send threesnake set-label "Three Snakes") ;; Change button label
  (send one-snake set-label "Press to play with one Snake                     ") ;; Revert to origin label
  (send three-snakes set-label "Press to play with three Snakes                     ") ;; Revert to origin label
    (send (caddr snake-list) kill) ;; Set alive to #f for third snake
    (send (cadr snake-list) revive))) ;; Set alive to #t for second snake


;; FUNCTION: three-player
    ;;
    ;; DESCRIPTION: Function called from the "Three-Snakes"-buttons callback, and sets how many snakes to play with.
    ;;              
    ;;
    ;; INPUT: Button and event.
    ;;
    ;; OUTPUT: None.
(define (three-players button event)
  (begin
  (send (car snake-list) include) ;; Set show to #t
  (send (cadr snake-list) include) ;; Set show to #t 
  (send (caddr snake-list) include);; Set show to #t
  (send three-snakes set-label "You have chosen three players, start game!  ") ;; Label when pressed
  (send button set-label "Marked") ;; Button label
  (send playerwindow show #f) ;; Playerwindow to #f
  (send onesnake set-label "One Snake") ;; Change button label
  (send twosnake set-label "Two Snakes") ;; Change button label
  (send one-snake set-label "Press to play with one Snake                     ") ;; Revert label to origin
  (send two-snakes set-label "Press to play with two Snakes                     ") ;; Revert label to origin
  (send (caddr snake-list) revive) ;; Set alive to #t
    (send (cadr snake-list) revive))) ;; Set alive to #t

;; Error window when not choosing number of players and difficulty
(define playerwindow (new frame%
                          [label "Choose number of players"]
                          [width 100]
                          [height 100]))

;; FUNCTION: start-proc
    ;;
    ;; DESCRIPTION: Function called from the "Start"-buttons callback, and starts the game function in world.rkt.
    ;;              
    ;;
    ;; INPUT: Button and event.
    ;;
    ;; OUTPUT: None.
(define (start-proc button event)
  (if (and (equal? (send (car snake-list) show?) #t)
           (or (equal? (send (car snake-list) get-velocity) 4)
               (equal? (send (car snake-list) get-velocity) 6)
               (equal? (send (car snake-list) get-velocity) 8)))
      (begin
        (send game-frame show #t)
        (send game-canvas show #t)
        (send startwindow show #f)
        (send helpframe show #f)
        (start)
        (send game-canvas focus))
      (send playerwindow show #t)))

;; Error message
(define errormsg
  (new message%
                      [label "You have to choose the number of players and difficulty to start the game                               "]
                      [parent playerwindow]))

;; Label over start button
(define startbutton
  (new message% [label "        Press to start game        "]
       [parent startwindow]))

;; Start button
(new button%
                       [label "Start"]
                       [parent startwindow]
                       [callback start-proc])

;; Help text
(define Helptext1
  (new message% [label "You control the yellow snake with right-key and left-key, the bluw snake with Q and W, and the white snake with V and B          "]
       [parent helpframe]))
;; Help text
(define Helptext2
  (new message% [label "Red power-up sets the velocity to faster, green power-up sets the velocity to slower and the purple changes the steering angle for the other snakes        "]
       [parent helpframe]))

;; Label over "One Snake"-button
(define one-snake (new message% [label "Press to play with one Snake                           "] [parent subpanel]))

;; "One Snake"-button
(define onesnake (new button%
                      [label "One Snake"]
                      [parent subpanel]
                      [callback one-player]))

;; Label over "Two Snakes"-button
(define two-snakes (new message% [label "Press to play with two Snakes                     "] [parent subpanel]))

;; "Two snakes"-button
(define twosnake (new button%
                      [label "Two Snakes"]
                      [parent subpanel]
                      [callback two-players]))
;; Label over "One Snake"-button
(define three-snakes (new message% [label "Press to play with three Snakes                      "] [parent subpanel]))

;; "Three Snakes"-button
(define threesnake (new button%
                        [label "Three Snakes"]
                        [parent subpanel]
                        [callback three-players]))

;;Procedure to set snakes starting velocity to 2."
;; FUNCTION: easy
    ;;
    ;; DESCRIPTION: Procedure to set snakes starting velocity to 4.
    ;;              
    ;;
    ;; INPUT: Button and event.
    ;;
    ;; OUTPUT: None.
(define (easy button event)
  (begin
  (send button set-label "Marked")
  (send (car snake-list) low-speed)
  (send (cadr snake-list) low-speed)
  (send (caddr snake-list) low-speed)
  (send playerwindow show #f)
  (send Medium set-label "Medium")
  (send Hard set-label "Hard")))

;; FUNCTION: medium
    ;;
    ;; DESCRIPTION: Procedure to set snakes starting velocity to 6.
    ;;              
    ;;
    ;; INPUT: Button and event.
    ;;
    ;; OUTPUT: None.

(define (medium button event)
  (begin
  (send button set-label "Marked")
  (send (car snake-list) medium-speed)
  (send (cadr snake-list) medium-speed)
  (send (caddr snake-list) medium-speed)
  (send playerwindow show #f)
  (send Easy set-label "Easy")
  (send Hard set-label "Hard")))

;; FUNCTION: hard
    ;;
    ;; DESCRIPTION: Procedure to set snakes starting velocity to 6.
    ;;              
    ;;
    ;; INPUT: Button and event.
    ;;
    ;; OUTPUT: None.
(define (hard button event)
  (begin
  (send button set-label "Marked")
  (send (car snake-list) high-speed)
  (send (cadr snake-list) high-speed)
  (send (caddr snake-list) high-speed)
  (send playerwindow show #f)
  (send Easy set-label "Easy")
  (send Medium set-label "Medium")))

;; Buttons with different diffiulty for the game
(define speed (new message% [label "Difficulty"] [parent subpanel]))

(define Easy (new button%
     [label "Easy"]
     [parent subpanel]
     [callback easy]))
(define Medium (new button%
     [label "Medium"]
     [parent subpanel]
     [callback medium]))
(define Hard (new button%
     [label "Hard"]
     [parent subpanel]
     [callback hard]))