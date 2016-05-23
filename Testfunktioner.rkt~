#lang racket
(require racket/math)

(define (distance x0 y0 x1 y1 x2 y2)
  (/ (abs (+ (- (* (- y2 y1) x0) (* (- x2 x1) y0) (* y2 x1)) (* x2 y1))) (sqrt (+ (expt (- y2 y1) 2) (expt (- x2 x1) 2)))))

(define check-range
  (lambda (num low high)
    (cond
      ((and (>= num low) (<= num high)))
      (else #f))))

(define checker
  (lambda (x0 y0 x1 y1 x2 y2)
    (if (>= x2 x1)
             (if (>= y2 y1)
                 (and (check-range x0 x1 x2)
                      (check-range y0 y1 y2))
                 (and (check-range x0 x1 x2)
                      (check-range y0 y2 y2)))
             (if (>= y2 y1)
                 (and (check-range x0 x2 x1)
                      (check-range y0 y1 y2))
                 (and (check-range x0 x2 x1)
                      (check-range x0 y2 y1))))))