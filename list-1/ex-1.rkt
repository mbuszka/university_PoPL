#lang racket

; Exercise 1.21 from EoPL

(define (product los1 los2)
  (append-map (lambda (x)
                (map (lambda (y) (cons x y)) los2))
              los1))