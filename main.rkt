#lang racket/gui
(require racket/math)
(require 2htdp/universe)
(require 2htdp/image)
(require "world.rkt")
(require "PlayerClass.rkt")

;frame for the menuwindow
(define startwindow
  (new frame%
       [label "Startscreen Garde La Courbe"]
       [width 650]
       [height 800]))

;Bitmap header to the menuwindow
(define *our-bitmap* (make-object bitmap% "Bild.png"))

;Draws the bitmap
(define (our-draw canvas dc)
  (send dc draw-bitmap *our-bitmap* 0 0))

;Canvas on the menuwindow
(define start-screen
  (new canvas%
       [parent startwindow]
       [paint-callback our-draw]))

;shows the menuwindow
(send startwindow show #t)

;Panel in menuwindow 
(define panel (new group-box-panel%
                   [label "Meny"]
                   [parent startwindow]
                   [alignment '(right center)]))
;Subpanel in menuwindow
(define subpanel (new group-box-panel%
                      [label " "]
                      [parent panel]
                      [alignment '(left center)]))
;help frame
(define helpframe (new frame%
                       [label "Help"]
                       [width 500]
                       [height 200]))

;The button "help"
(define (help button event)
  (send helpframe show #t))
(new button%
     [label "Help"]
     [parent panel]
     [callback help])

;button to quit in menuwindow
(define (quitstart button event)
  (send startwindow show #f))
(new button%
     [label "Quit"]
     [parent panel]
     [callback quitstart])

;quit button for the game-frame
(define (quitgame button event)
  (send game-frame show #f)
  (send *graphics-timer* stop)
    (send *game-timer* stop))
(new button%
     [label "Quit"]
     [parent game-frame]
     [callback quitgame])



;Callback for "One Snake"
(define (one-player button event)
  (begin
  (send (car snake-list) include) ;shows the first snake
  (send one-snake set-label "You have chosen one player, start game!  ") ;label when pressed
  (send button set-label "Marked") ;new button label
  (send playerwindow show #f) ;playerwindow to false
  (send (cadr snake-list) remove) ;set show to false for player2 
  (send (caddr snake-list) remove) ;set show to false for player3
  (send twosnake set-label "Two Snakes") ;change button label
  (send threesnake set-label "Three Snakes") ;change button label
  (send two-snakes set-label "Press to play with two Snakes                     ") ;revert to origin label
  (send three-snakes set-label "Press to play with three Snakes                     ") ;revert to origin label
    (send (cadr snake-list) kill) ;set alive to false
    (send (caddr snake-list) kill))) ;set alive to false

;Callback for "Two Snakes"
(define (two-players button event)
  (begin
  (send (car snake-list) include) ;show first snake
  (send (cadr snake-list) include) ;show second snake
  (send two-snakes set-label "You have chosen two players, start game!  ") ;label when pressed
  (send button set-label "Marked") ;button label
  (send playerwindow show #f) ;playerwindow to false
  (send (caddr snake-list) remove) ;sets show to false for the third snake
  (send onesnake set-label "One Snake") ;change button label
  (send threesnake set-label "Three Snakes") ;change button label
  (send one-snake set-label "Press to play with one Snake                     ") ;revert to origin label
  (send three-snakes set-label "Press to play with three Snakes                     ") ;revert to origin label
    (send (caddr snake-list) kill) ;set alive to false for third snake
    (send (cadr snake-list) revive))) ;set alive to true for second snake


;Callback for "Three Snakes"
(define (three-players button event)
  (begin
  (send (car snake-list) include) ;set show to true
  (send (cadr snake-list) include) ;set show to true 
  (send (caddr snake-list) include);set show to true
  (send three-snakes set-label "You have chosen three players, start game!  ") ;label when pressed
  (send button set-label "Marked") ;button label
  (send playerwindow show #f) ;playerwindow to false
  (send onesnake set-label "One Snake") ;change button label
  (send twosnake set-label "Two Snakes") ;change button label
  (send one-snake set-label "Press to play with one Snake                     ") ;revert label to origin
  (send two-snakes set-label "Press to play with two Snakes                     ") ;revert label to origin
  (send (caddr snake-list) revive) ;set alive to true
    (send (cadr snake-list) revive))) ;set alive to true

;Error window when not choosing number of players and difficulty
(define playerwindow (new frame%
                          [label "Choose number of players"]
                          [width 100]
                          [height 100]))

;; Callback for "Start"-knappen
(define (buttproc button event)
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

;Start button
(define our-utton (new button%
                       [label "start"]
                       [parent startwindow]
                       [callback buttproc]))

;Infotext
(define one-playersteering
  (new message% [label "You control the first snake with right-key and left-key, the second snake with Q and W, and the third snake with V and B          "]
       [parent helpframe]))

;infotext om vad som händer när du trycker på knappen
(define one-snake (new message% [label "Press to play with one Snake                           "] [parent subpanel]))

;Knapp för en orm
(define onesnake (new button%
                      [label "One Snake"]
                      [parent subpanel]
                      [callback one-player]))

;infotext om vad som händer när du trycker på knappen
(define two-snakes (new message% [label "Press to play with two Snakes                     "] [parent subpanel]))

;Knapp för två ormar
(define twosnake (new button%
                      [label "Two Snakes"]
                      [parent subpanel]
                      [callback two-players]))
;infotext om vad som händer när du trycker på knappen
(define three-snakes (new message% [label "Press to play with three Snakes                      "] [parent subpanel]))
;Knapp för tre ormar
(define threesnake (new button%
                        [label "Three Snakes"]
                        [parent subpanel]
                        [callback three-players]))

;Procedure to set snakes starting velocity to 2."
(define (easy a b)
  (begin
  (send a set-label "Marked")
  (send (car snake-list) low-speed)
  (send (cadr snake-list) low-speed)
  (send (caddr snake-list) low-speed)
  (send playerwindow show #f)
  (send Medium set-label "Medium")
  (send Hard set-label "Hard")))
;Procedure to set snakes starting velocity to 4."
(define (medium a b)
  (begin
  (send a set-label "Marked")
  (send (car snake-list) medium-speed)
  (send (cadr snake-list) medium-speed)
  (send (caddr snake-list) medium-speed)
  (send playerwindow show #f)
  (send Easy set-label "Easy")
  (send Hard set-label "Hard")))
;Procedure to set snakes starting velocity to 6."
(define (hard a b)
  (begin
  (send a set-label "Marked")
  (send (car snake-list) high-speed)
  (send (cadr snake-list) high-speed)
  (send (caddr snake-list) high-speed)
  (send playerwindow show #f)
  (send Easy set-label "Easy")
  (send Medium set-label "Medium")))
;Knappar med olika hastighet som önskas
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