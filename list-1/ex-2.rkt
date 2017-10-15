#lang racket

; Exercise 1.26 from EoPL

(define (up lst)
  (if (null? lst)
      '()
      (if (list? (car lst))
          (append (car lst) (up (cdr lst)))
          (cons (car lst) (up (cdr lst))))))