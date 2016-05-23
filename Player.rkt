#lang racket
(require math/array)

(define player%
  (class object%
    (init-field name)
    