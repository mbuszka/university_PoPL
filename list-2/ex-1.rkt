#lang eopl

; Exercise 2.5 from EoPL

(provide
 apply-env
 empty-env
 extend-env)

(require rackunit)

(define empty-env
  (lambda () '()))

(define (extend-env var val env)
  (cons (cons var val) env))

(define (apply-env env var)
  (if (null? env)
      (eopl:error 'apply-env "No binding for ~s" var)
      (if (eqv? (caar env) var)
          (cdar env)
          (apply-env (cdr env) var))))

(test-begin
 (define initial-env (extend-env 'a 10 (empty-env)))
 (define env-a (extend-env 'a 20 initial-env))
 (define env-b (extend-env 'b 30 initial-env))
 
 (check-equal?  initial-env '((a . 10)) "One element env")
 (check-equal? (apply-env env-a 'a) 20  "New binding shadows old one")
 (check-equal? (apply-env env-b 'b) 30  "Different bindings don't interfere")
 (check-equal? (apply-env env-b 'a) 10  "Different bindings don't interfere"))
                