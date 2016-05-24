#lang racket/gui

(require racket/math)
(provide game-canvas%)
(provide game-frame)
(provide game-canvas)
(provide snake-list)
(require "PlayerClass.rkt")
(require "PowerupClass.rkt")
(provide dc)
(provide start)
(provide *graphics-timer*)
(provide *game-timer*)
(define time 0) ;; Initiates time to 0. 




;; Players added in a list.
(define snake-list
  (list
   (new player%
        [name 'simon]
        [snake (list '(330 . 300)
                     '(329 . 300)
                     '(328 . 300))]
        [player-id "1"]
        [color "yellow"])
   (new player%
        [name 'jakob]
        [snake (list '(330 . 500)
                     '(329 . 500)
                     '(328 . 500))]
        [player-id "2"]
        [color "blue"])
   (new player%
        [name 'hej]
        [snake (list '(330 . 400)
                     '(329 . 400)
                     '(328 . 400))]
        [player-id "3"]
        [color "white"])))

;; The power-ups in a list.
(define power-list
  (list
   (new power-up%
        [name "high-speed"]
        [function 'highspeed]
        [color "red"])
   (new power-up%
        [name "low-speed"]
        [function 'lowspeed]
        [color "green"])
   (new power-up%
        [name "steering-radius"]
        [function 'set-radius]
        [color "purple"])))

; Function called upon from draw. Puts out dots on the canvas and connects those dots with lines.
(define draw-snake
  (lambda (ls)
    (if (or (void? ls) (null?  (cdr ls)))
        '()
        (let* (
               (location-head (first ls))
               (location-second (first (cdr ls)))
               (x (car location-head))
               (y (cdr location-head))
               (x_next (car location-second))
               (y_next (cdr location-second))
               (generator 10))
          (begin
            (send dc draw-line x y x_next y_next)
            (when (not (null? (cdr ls)))
              (draw-snake (cdr ls))))))))

;; Function called upon in on-paint on the canvas. Sets the pen and calls for draw-snake above.
(define draw
  (lambda (lst)
    (if (not (null? lst))
        (if (equal? (send (car lst) show?) #t)
            (begin
              (send dc set-pen (send (car lst) get-color) 10 'solid)
              (draw-snake (send (car lst) get-snake))
              (draw (cdr lst)))
            (draw (cdr lst)))
        (void))))

;; Draws up the playing field.
(define draw-field
  (lambda ()
    (begin
      (send dc set-brush "black" 'solid)
      (send dc set-pen "black" 0 'solid)
      (send dc draw-rectangle 0 0 1024 768)
      (draw-score game-canvas dc))))


;; The function that draws up a powerup. Num decides what powerup
;; that will be.
(define draw-power-up
  (lambda (num)
    (if (and
         (>= num 0)
         (< num 5))
        (begin
          (send dc set-brush (send (car power-list) get-color) 'solid)
          (send dc draw-rectangle (send (car power-list) get-x) (send (car power-list) get-y) 15 15))
        (if (and
             (>= num 5)
             (< num 10))
            (begin
              (send dc set-brush (send (cadr power-list) get-color) 'solid)
              (send dc draw-rectangle (send (car power-list) get-x) (send (car power-list) get-y) 15 15))
            (begin
              (send dc set-brush (send (caddr power-list) get-color) 'solid)
              (send dc draw-rectangle (send (car power-list) get-x) (send (car power-list) get-y) 15 15)))))) 



;; The main game canvas. 
(define game-canvas%
  (class canvas%
    (inherit get-width get-height refresh)
    (field
     [key-events (make-hash)])
    
    (define/override (on-paint)
      (begin
        (draw-field)
        (draw-power-up num)
        (draw snake-list)))
    
    (define/public (get-key-events) key-events)
    
    ;; Runs first to set all the values in the key-events hash to #f.
    (define/public init
      (lambda ()
        (begin
          (hash-set! key-events "l1" #f)
          (hash-set! key-events "r1" #f)
          (hash-set! key-events "l2" #f)
          (hash-set! key-events "r2" #f)
          (hash-set! key-events "l3" #f)
          (hash-set! key-events "r3" #f))))
    
    ;; Sends the key-events, making them accesible from the playerclass.
    (define/public send-key-events
      (lambda (lst)
        (if (null? lst) (void)
            (begin
              (send (car lst) steer key-events)
              (send-key-events (cdr lst))))))
    
    ;; The keyboard listener. I case of key-codes it sets the corresponding
    ;; value in the hash table to #t. In case of release-code of the button
    ;; it sets the value to #f.
    (define/override (on-char ke)
      (begin
        (case (send ke get-key-code)
          [(left)
           (hash-set! key-events "l1" #t)]
          [(right)
           (hash-set! key-events "r1" #t)]
          [(#\q)
           (hash-set! key-events "l2" #t)]
          [(#\w)
           (hash-set! key-events "r2" #t)]
          [(#\v)
           (hash-set! key-events "l3" #t)]
          [(#\b)
           (hash-set! key-events "r3" #t)]
          [else (void)])
        (case (send ke get-key-release-code)
          [(left)
           (hash-set! key-events "l1" #f)]
          [(right)
           (hash-set! key-events "r1" #f)]
          [(#\q)
           (hash-set! key-events "l2" #f)]
          [(#\w)
           (hash-set! key-events "r2" #f)]
          [(#\v)
           (hash-set! key-events "l3" #f)]
          [(#\b)
           (hash-set! key-events "r3" #f)]
          [else (void)])
        (send-key-events snake-list)))
    
    (super-new)))

;; Function that runs when all players are stopped to close down the game window and
;; show the screen-between-rounds.
(define all-dead
  (lambda ()
    (if (equal? (send (car snake-list) get-rounds) 3)
        (begin
          (send *graphics-timer* stop)
          (send *game-timer* stop)
          (send game-frame show #f)
          (send game-canvas show #f)
          (sum-up-score snake-list)
          (send end-screen show #t)
          (send end-screen focus)
          (final-score end-canvas final-dc))
        (begin
          (send *graphics-timer* stop)
          (send *game-timer* stop)
          (send game-frame show #f)
          (send game-canvas show #f)
          (sum-up-score snake-list)
          (send screen-between-rounds show #t)
          (send screen-after-rounds show #t)
          (send screen-after-rounds focus)
          (draw-stats game-canvas dc-between)))))

;; Function thats adds up score to the accumulated-score field in player class aswell as changing
;; the label on the screen between rounds.
(define sum-up-score
  (lambda (lst)
    (begin
      (when (equal? (send (car snake-list) show?) #t)
        (send (car lst) sum-up-score))
      (when (equal? (send (cadr snake-list) show?) #t)
        (send (cadr lst) sum-up-score))
      (when (equal? (send (caddr snake-list) show?) #t)
        (send (caddr lst) sum-up-score)))))

;; Callback for start-round button. Starts round.
(define (start-round)
  (begin
    (when (equal? (send (car snake-list) show?) #t)
      (send (car snake-list) set-snake! (list '(330 . 300)
                                              '(325 . 300)
                                              '(320 . 300))))
    (when (equal? (send (cadr snake-list) show?) #t)
      (send (cadr snake-list) set-snake! (list '(330 . 500)
                                               '(325 . 500)
                                               '(320 . 500))))
    (when (equal? (send (caddr snake-list) show?) #t)
      (send (caddr snake-list) set-snake! (list '(330 . 400)
                                                '(325 . 400)
                                                '(320 . 400))))
    (set! time 0)
    (send (car snake-list) reset-score)
    (send (cadr snake-list) reset-score)
    (send (caddr snake-list) reset-score)
    (send *graphics-timer* start 32 #f)
    (send *game-timer* start 32 #f)))

;; Callback for the next-button round.
(define (next-round a b)
  (begin      
    (send screen-between-rounds show #f)
    (send screen-after-rounds show #f)
    (send game-frame show #t)
    (send game-canvas show #t)
    (send game-canvas focus)
    (send game-canvas init)
    (send (car snake-list) add-round)
    (start-round)
    ))

(define (draw-score canvas dca)
  (begin
  (send dca set-text-mode 'transparent)
  (send dca set-text-foreground "white")
  (send dca draw-text (string-append "Player1: " (number->string (send (car snake-list) get-score))) 20 20)
  (when (equal? (send (cadr snake-list) show?) #t)
  (send dca draw-text (string-append "Player2: " (number->string (send (cadr snake-list) get-score))) 20 40))
  (when (equal? (send (caddr snake-list) show?) #t)
  (send dca draw-text (string-append "Player3: " (number->string (send (caddr snake-list) get-score))) 20 60))))

(define (draw-stats canvas dc)
  (begin
    (send dc set-brush "black" 'solid)
    (send dc draw-rectangle 0 0 1024 768)
  (send dc set-text-foreground "white")
  (send dc draw-text "Score summary" 450 250)
  (send dc draw-text (string-append "Player1: " (number->string (send (car snake-list) get-acc-score))) 450 300)
  (when (equal? (send (cadr snake-list) show?) #t)
    (send dc draw-text (string-append "Player2: " (number->string (send (cadr snake-list) get-acc-score))) 450 350))
  (when (equal? (send (caddr snake-list) show?) #t)
    (send dc draw-text (string-append "Player3: " (number->string (send (caddr snake-list) get-acc-score))) 450 400))))

(define (final-score canvas dc)
  (begin
    (send dc set-brush "black" 'solid)
    (send dc draw-rectangle 0 0 1024 768)
    (send dc set-text-foreground "white")
    (send dc draw-text "Final score" 450 300)
    (send dc draw-text (string-append "Final Score Player1: " (number->string (send (car snake-list) get-acc-score))) 450 350)
    (when (equal? (send (cadr snake-list) show?) #t)
      (send dc draw-text (string-append "Final Score Player2: " (number->string (send (cadr snake-list) get-acc-score))) 450 400))
    (when (equal? (send (caddr snake-list) show?) #t)
      (send dc draw-text (string-append "Final Score Player3: " (number->string (send (caddr snake-list) get-acc-score))) 450 450))))      

(define game-frame (new frame% (label "Garde la courbe") (width 1024) (height 768)))
(define game-canvas (new game-canvas%
                         [parent game-frame]
                         [paint-callback draw-score]))

(define screen-between-rounds (new frame% (label "Good job") (width 1024) (height 768)))
(define end-screen (new frame% (label "Game over") (width 1024) (height 768)))
(define end-canvas (new canvas% [parent end-screen] [paint-callback final-score]))
(define final-dc (send end-canvas get-dc))
(define (quitfinal button event)
  (send end-screen show #f))
(new button%
     [label "Quit"]
     [parent end-screen]
     [callback quitfinal])

;; Canvas with accumulated score after a round.
(define screen-after-rounds
  (new canvas%
       [parent screen-between-rounds]
       [paint-callback draw-stats]))
(define dc-between (send screen-after-rounds get-dc))
(new button%
     [label "Next round!"]
     [parent screen-between-rounds]
     [callback next-round])



;; Defines the drawing context for game canvas.
(define dc (send game-canvas get-dc))

;; Function that checks if a certain value is in the span
;; of two other values. For example (check-range 2 1 3) returns
;; #t while (check-range 1 2 3) returns false.
(define check-range
  (lambda (num low high)
    (if
     (and (>= num low) (<= num high))
     #t
     #f)))


;; The main function responsible for updating the games physics.
;; Executes from the timer with the interval given there.
(define (refresher)
  (begin
    (set! time (+ time 1))
    (send dc set-brush "black" 'solid)   
    (check-collisions snake-list)
    (when (equal? (all-dead? snake-list) #t)
      (all-dead))
    (collisions-power-ups snake-list)
    (send dc set-brush "blue" 'solid)
    (send dc set-brush "red" 'solid)
    (update-players snake-list time)
    (send dc set-brush "yellow" 'solid)
    (when (equal? (remainder time 15) 0)
     (update-score snake-list))
    (when (equal? (remainder time 200) 0)
      (normalize-players snake-list))))

;; Function that checks if the velocity of all snakes are 0, i.e checks
;; if all snakes have collided with a snake or the walls.
(define all-dead?
  (lambda (lst)
    (if (null? lst)
        #t
        (if (equal? (send (car lst) is-alive?) #f)
            (all-dead? (cdr lst))
            #f))))

;; Function that adds score to snakes alive.
(define update-score
  (lambda (lst)
    (begin
      (when
          (equal? (send (car lst) is-alive?) #t)
          (send (car lst) add-score))
      (when
          (equal? (send (cadr lst) is-alive?) #t)
          (send (cadr lst) add-score))   
      (when
          (equal? (send (caddr lst) is-alive?) #t)
          (send (caddr lst) add-score)))))       


;; Used to normalize players parameters after use of a power-up.
(define normalize-players
  (lambda (lst)
    (if (null? lst) (void)
        (begin
          (send (car lst) normalize)
          (normalize-players (cdr lst))))))

;; Function updating players positions by calling the update-function in the player-class.
(define update-players
  (lambda (lst time)
    (when (not (null? lst))
      (if (equal? (send (car lst) show?) #t)
          (begin
            (send (car lst) update time)
            (update-players (cdr lst) time))
          (update-players (cdr lst) time)))))

;; Checks if the snakes are within the game-frame, otherwise stops them.
(define within-frame?
  (lambda (lst)  
    (if (null? lst)
        (void)
        (if (equal? (send (car lst) is-alive?) #f)
            (within-frame? (cdr lst))
            (if (or
                 (or (equal? (<= (send (car lst) get-x) 0) #t)
                     (equal? (>= (send (car lst) get-x) 1024) #t))
                 (or (equal? (<= (send (car lst) get-y) 0) #t)
                     (equal? (>= (send (car lst) get-y) 768) #t)))
                (send (car lst) stop)
                (within-frame? (cdr lst)))))))



;; Checks collisions using distance between a point (the snakes head) and a line drawn up by two other points.
(define collide?
  (lambda (body head)
    (if (null? (cdr body))
        #f
        (let* (
               (x0 (car head))
               (y0 (cdr head))
               (x1 (car (car body)))
               (y1 (cdr (car body)))
               (x2 (car (cadr body)))
               (y2 (cdr (cadr body))))
          (if                                                                 
           (and
            (check-range x0 (- (min x1 x2) 4) (+ (max x1 x2) 4))
            (check-range y0 (- (min y1 y2) 4) (+ (max y1 y2) 4)))
           (<= (/ (abs (+ (- (* (- y2 y1) x0) (* (- x2 x1) y0) (* y2 x1)) (* x2 y1))) (sqrt (+ (expt (- y2 y1) 2) (expt (- x2 x1) 2)))) 100)
           (collide? (cdr body) head))))))

;; Function checking collision between snakes. Sends stop to the colliding snake.
(define check-collisions
  (lambda (lst)
    (begin
      (within-frame? snake-list)
      (when (collide? (send (car lst) get-body) (send (car lst) get-head))
        (send (car lst) stop))       
      (when (collide? (send (car lst) get-body) (send (cadr lst) get-head))
        (send (cadr lst) stop))      
      (when (collide? (send (car lst) get-body) (send (caddr lst) get-head))
        (send (caddr lst) stop))    
      (when (collide? (send (cadr lst) get-body) (send (cadr lst) get-head))
        (send (cadr lst) stop))  
      (when (collide? (send (cadr lst) get-body) (send (car lst) get-head))
        (send (car lst) stop))
      (when (collide? (send (cadr lst) get-body) (send (caddr lst) get-head))
        (send (caddr lst) stop))     
      (when (collide? (send (caddr lst) get-body) (send (caddr lst) get-head))
        (send (caddr lst) stop))      
      (when (collide? (send (caddr lst) get-body) (send (car lst) get-head))
        (send (car lst) stop))      
      (when (collide? (send (caddr lst) get-body) (send (cadr lst) get-head))
        (send (cadr lst) stop)))))

;; Procedure that detects collisions with powerups and executes that power-up
;; to the snake colliding.
(define collisions-power-ups
  (lambda (lst)             
    (when (not (null? lst))
      (if
       ;(and
       (and
        (check-range (car (send (car lst) get-head))
                     (- (send (car power-list) get-x) 20)
                     (+ (send (car power-list) get-x) 20))
        (check-range (cdr (send (car lst) get-head))
                     (- (send (car power-list) get-y) 20)
                     (+ (send (car power-list) get-y) 20)))
       ;(equal? (send (car lst) changed?) #f))
       (begin
         (if (and
              (>= num 0)
              (< num 5))
             (send (car lst) highspeed)
             (if (and
                  (>= num 5)
                  (< num 10))
                 (send (car lst) lowspeed)
                 (begin
                   (power-up-steering snake-list)
                   (send (car lst) normalize))))
         (randomizer)
         (send (car power-list) hit))
       (collisions-power-ups (cdr lst))))))

;; Function that makes your opponents steering radius larger whilst keeps yours at original.
(define (power-up-steering lst)
  (cond
    ((null? lst) (void))
    (else
     (begin
       (send (car lst) set-radius)
       (power-up-steering (cdr lst))))))



;; The timer controlling the graphics.
(define *graphics-timer*
  (new timer%
       [notify-callback (lambda()
                          (send game-canvas refresh))]))

;; The number which decides what powerup is shown.
;; Depending if it randoms a value 0-5, 5-10 or 10-15
;; the powerup will be highspeed, lowspeed or changed
;; steering radius.
(define num (random 15))

;; Runs every time a snake is colliding with a powerup.
;; Randoms which will be the next powerup.
(define (randomizer)
  (set! num (random 15)))

;; The timer controlling the games physics.
(define *game-timer*
  (new timer%
       [notify-callback refresher]))

;; The procedure which is called upon from
;; the start screen and initiates the game.
(define (start)
  (begin
    (send *graphics-timer* start 32 #f)
    (send *game-timer* start 32 #f)
    (send game-frame show #t)
    (send game-canvas show #t)
    (send game-canvas focus)
    (send game-canvas init)
    (send game-canvas show #t)))






;(send *graphics-timer* start 32 #f)
;(send *game-timer* start 32 #f)
;(send game-frame show #t)
;(send game-canvas show #t)
;(send game-canvas focus)
;(send game-canvas init)
;(send game-canvas show #t)


;(send *game-timer* start 32 #f)
;(send *graphics-timer* start 32 #f)

