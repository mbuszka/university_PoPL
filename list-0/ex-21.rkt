#lang racket

(define (product los1 los2)
  (append-map (lambda (x)
                (map (lambda (y) (cons x y)) los2))
              los1))
