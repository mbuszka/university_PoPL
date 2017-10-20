#lang eopl

; Exercise 2.21

(require rackunit)

(define (any? v) #t)

(define-datatype environment environment?
  (empty-env)
  (extend-env (var symbol?) (val any?) (env environment?)))

(define (apply-env env var)
  (cases environment env
    (empty-env () (eopl:error 'apply-env "No binding for ~s" var))
    (extend-env (saved-var saved-val rest)
                (if (eqv? var saved-var)
                    saved-val
                    (apply-env rest var)))))

(define (has-binding? env var)
  (cases environment env
    (empty-env () #f)
    (extend-env (saved-var _ rest)
                (if (eqv? var saved-var)
                    #t
                    (has-binding? rest var)))))

(test-begin
 (define example
   (extend-env 'a 10
               (extend-env 'b 20
                           (extend-env 'c 30
                                   (extend-env 'a 0 (empty-env))))))
 (check-equal? (apply-env example 'a) 10)
 (check-equal? (apply-env example 'b) 20)
 (check-equal? (apply-env example 'c) 30)
 (check-equal? (has-binding? example 'a) #t))