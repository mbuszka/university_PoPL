#lang eopl

(provide (all-defined-out))

(define control-continuation? procedure?)

(define (block-cont smts env cont)
  (lambda () (if (null? smts)
                 (apply-c-cont cont)
                 (result-of (car smts) env (block-cont (cdr smts) env cont)))))

(define (apply-c-cont cont) (cont))
