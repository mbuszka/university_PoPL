#lang eopl

; Exercise B.3 from EoPL

(require rackunit
         "ex-8.rkt")

; Fix this with pattern matching

(define (zip lst-1 lst-2)
  (cond
    (((null? lst1) '())
     ((null? lst2) '())
     (else (cons (cons (car lst-1) (car lst-2)) (zip (cdr lst-2) (cdr lst-2)))))))

(define (interp-expr expr)
  (cases arith-expr
    (an-expr (term ops terms)
             (foldl (lambda (acc t)
                      (cond