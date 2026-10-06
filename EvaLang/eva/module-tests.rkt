#lang racket/base

(require rackunit
         "main.rkt")

(check-equal? 
  (eval-global 
    '(module Math
       (begin
          (def abs (x)
             (if (< x 0)
                 (- x)
                 (x)))

          (var MAX_VALUE 1000)))

    '((prop Math abs) -10))
  10 "Math module access abs function")

