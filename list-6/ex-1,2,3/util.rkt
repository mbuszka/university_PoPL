#lang racket

(provide zip)

(define (zip lst-1 lst-2)
  (match (cons lst-1 lst-2)
    ((cons '() _) '())
    ((cons _ '()) '())
    ((cons (cons h1 t1) (cons h2 t2)) (cons (cons h1 h2) (zip t1 t2)))))