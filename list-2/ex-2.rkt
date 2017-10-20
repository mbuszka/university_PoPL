#lang eopl

; Exercise 2.10 from EoPL

(require rackunit
         "ex-1.rkt")

(define (extend-env* vars vals env)
  (cond
    ((null? vars) env)
    ((null? vals) env)
    (else (extend-env* (cdr vars)
                       (cdr vals)
                       (extend-env (car vars) (car vals) env)))))

(test-begin
 (define example (extend-env* '(a b c) '(10 20 30) (empty-env)))
 (check-equal? (apply-env example 'a) 10)
 (check-equal? (apply-env example 'b) 20)
 (check-equal? (apply-env example 'c) 30)
 (test-begin
  (define env (extend-env* '(a d) '(15 40) example))
  (check-equal? (apply-env env 'a) 15)
  (check-equal? (apply-env env 'd) 40)))
  