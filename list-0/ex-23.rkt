#lang racket

(define (list-index pred lst)
  (define (aux lst pos)
    (if (null? lst)
      #f
      (if (pred (car lst))
          pos
          (aux (cdr lst) (+ pos 1)))))
  (aux lst 0))