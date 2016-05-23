#lang racket/gui

(provide player%)

(define player%
  (class object%
    (init-field name
                snake
                player-id
                color
                [show #f]
                [alive #f]
                [changed #f]
                )
    (field
     [x (car (car snake))]
     [y (cdr (car snake))]
     [score 0]
     [accumulated-score 0]
     [starting-velocity 4]
     [velocity 4]
     [angle -0.01]
     [direction (cons velocity 0)]
     [steering-radius 0.1]
     [turn 0]
     [number-of-rounds 1])
    
    (define/public (get-snake) snake) ; gets the whole snake.
    (define/public (get-body) (cddr snake)) ; gets the body of the snake (all coordinate pairs except the first).
    (define/public (get-head) (car snake)) ; gets the head of the snake (the first coordinate pair).
    (define/public (get-x) (car (car snake))) ; gets the x-coordinate of the head.
    (define/public (get-y) (cdr (car snake))) ; gets the y-coordinate of the head.
    (define/public (get-name) name) ; gets the players name.
    (define/public (get-direction) direction) ; gets direction of given snake.
    (define/public (is-alive?) alive) ; parameter that tells if the snake has died or not.
    (define/public (show?) show) ; parameter that decides if the snake is included in the game or not.
    (define/public (get-velocity) velocity) ; gets the snakes velocity.
    (define/public (changed?) changed) ; parameter that shows if the snakes features has changed. i.e if the player is affected by any power-up at the moment.
    (define/public (get-color) color)
    (define/public (get-score) score)
    (define/public (get-acc-score) accumulated-score)
    (define/public (get-rounds) number-of-rounds)

    (define/public (add-round)
      (set! number-of-rounds (+ number-of-rounds 1)))
    
    (define/public (set-snake! nsnake)
      (if (equal? show #t)
          (begin
            (set! snake nsnake)
            (set! alive #t)
            (set! angle -0.01)
            (set! steering-radius 0.1)       
            (set! velocity starting-velocity)
            (set! direction (cons velocity 0))
            (set! turn 0))
          (void)))
    
    
    (define/public (reset-score)
      (set! score 0))
    
    (define/public (add-score)
      (set! score (+ score 1)))
    
    (define/public (sum-up-score)
      (set! accumulated-score (+ score accumulated-score)))
    
    ; Stops the snake by setting the velocity to 0.
    (define/public (stop)
      (set! velocity 0)
      ;(set! starting-velocity 0)
      (set! alive #f))
    
    ;; power-up that changes the speed of the snake.
    (define/public (highspeed)
      (set! velocity (* velocity 2))
      (set! steering-radius 0.15)
      (set! changed #t))
    ;; power-up that changes the speed of the snake.
    (define/public (lowspeed)
      (set! velocity (/ velocity 2))
      (set! changed #t))
    ;; power-up that gives your opponents a larger steering radius.
    (define/public (set-radius)
      (set! steering-radius 0.04))
    
    
    ;; Sets the starting speed. Used by buttons on the main menu.
    (define/public (low-speed)
      (set! velocity 4)
      (set! starting-velocity 4))
    (define/public (medium-speed)
      (set! velocity 6)
      (set! starting-velocity 6))
    (define/public (high-speed)
      (set! velocity 8)
      (set! starting-velocity 8))

    ;; Includes the player in the round by setting show and alive to #t.
    (define/public (include)
      (set! show #t)
      (set! alive #t))

    ;; Removes the player from the round by setting remove 
    (define/public (remove)
      (set! show #f))
    
    (define/public (kill)
      (set! alive #f))
    (define/public (revive)
      (set! alive #t))
    
    ;; Sets snakes values to starting values.
    (define/public (normalize)
      (if (equal? alive #t)
          (begin
      (set! velocity starting-velocity)
      (set! steering-radius 0.08)
      (set! changed #f))
          (void)))
    
    (define (turn-left)
      (begin
        (set! angle (- angle steering-radius))
        (let ((x (* velocity (cos angle)))
              (y (* velocity (sin angle))))
          (set! direction (cons x y)))))
    
    (define (turn-right)
      (begin
        (set! angle (+ angle steering-radius))
        (let ((x (* velocity (cos angle)))
              (y (* velocity (sin angle))))
          (set! direction (cons x y)))))
    
    (define (no-turn)
      (begin
        (let ((x (* velocity (cos angle)))
              (y (* velocity (sin angle))))
          (set! direction (cons x y)))))
    
    
    (define/public steer
      (lambda (key-events)
        (begin
          (cond
            ((equal? (hash-ref key-events (string-append "l" player-id)) #t)
             (set! turn 2))
            ((equal? (hash-ref key-events (string-append "r" player-id)) #t)
             (set! turn 1))
            ((equal? (hash-ref key-events (string-append "l" player-id)) #f)
             (set! turn 0))           
            ((equal? (hash-ref key-events (string-append "r" player-id)) #f)
             (set! turn 0))))))
    
    
    (define/public update
      (lambda (time)
        (let* (
               (head (car snake))
               (x (+ (car direction) (car head)))
               (y (+ (cdr direction) (cdr head))))
          (begin
            (cond
              ((equal? turn 1)
               (turn-right))
              ((equal? turn 2)
               (turn-left))
              ((equal? turn 0)
               (no-turn)))
            (if
             (equal? (remainder time 5) 0)
             (set! snake (cons (cons x y) snake))
             (set! snake (cons (cons x y) (cdr snake))))))))
    
    
    (super-new)))
