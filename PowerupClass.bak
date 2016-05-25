#lang racket/gui

(provide power-up%)

(define power-up%
  (class object%
    [init-field name
                function
                color
                [x (random 900)]
                [y (random 600)]]

    (define/public (get-color)
      color)
    (define/public (get-coordinates)
      (cons x y))
    (define/public (get-name)
      name)
    (define/public (get-x)
      x)
    (define/public (get-y)
      y)
    (define/public (hit)
      (set! x (random 900))
      (set! y (random 600)))
    (define/public (get-function)
      function)
    
    (super-new)))
