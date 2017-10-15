#lang racket

(define (filter-in pred lst)
  (if (null? lst)
      '()
      (let ([rest (filter-in pred (cdr lst))])
        (if (pred (car lst))
            (cons (car lst) rest)
            rest))))
          