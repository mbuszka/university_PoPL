#lang racket

(define (remove s ss)
  (if (null? ss)
      '()
      (if (eq? s (car ss))
          (remove s (cdr ss))
          (cons (car ss) (remove s (cdr ss))))))