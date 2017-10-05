#lang racket

(define (list-set lst n x)
  (if (null? lst)
      '()
      (if (eq? n 0)
          (cons x (cdr lst))
          (cons (car lst) (list-set (cdr lst) (- n 1) x)))))