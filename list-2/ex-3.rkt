#lang eopl

; Exercise 2.11 from EoPL

(require rackunit)

(define empty-env
  (lambda () '()))

(define (extend-env var val env)
  (extend-env* (list var) (list val) env))

(define (extend-env* vars vals env)
  (cons (cons vars vals) env))

(define (lookup vars vals var)
  (cond
    ((null? vars) '())
    ((null? vals) '())
    ((eqv? (car vars) var) (list (car vals)))
    (else (lookup (cdr vars) (cdr vals) var))))

(define (apply-env env var)
  (if (null? env)
      (eopl:error 'apply-env "No binding for ~s" var)
      (let*
          ((a-lists (car env))
           (res (lookup (car a-lists) (cdr a-lists) var)))
        (if (null? res)
            (apply-env (cdr env) var)
            (car res)))))

(test-begin
 (define init (extend-env 'a 15 (extend-env* '(a b c) '(10 20 30) (empty-env))))
 (check-equal? (length init) 2)
 (check-equal? (length (caadr init)) 3)
 (check-equal? (apply-env init 'a) 15)
 (check-equal? (apply-env init 'c) 30))