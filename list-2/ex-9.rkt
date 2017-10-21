#lang eopl

; Exercise B.3 from EoPL

(require rackunit
         racket/match
         (only-in racket foldl)
         "ex-8.rkt")

(sllgen:make-define-datatypes lexical-spec grammar)

(define scan&parse (sllgen:make-string-parser lexical-spec grammar))

(define (zip lst-1 lst-2)
  (match (cons lst-1 lst-2)
    ((cons '() _) '())
    ((cons _ '()) '())
    ((cons (cons h1 t1) (cons h2 t2)) (cons (cons h1 h2) (zip t1 t2)))))

(define (bin-op sym)
  (match sym
    ('+ +)
    ('- -)
    ('* *)
    ('/ /)))

(define (interp-expr expr)
  (cases arith-expr expr
    (an-expr (term ops terms)
             (foldl (lambda (t acc) ((bin-op (car t)) acc (interp-term (cdr t))))
                    (interp-term term)
                    (zip ops terms)))))

(define (interp-term term)
  (cases arith-term term
    (a-term (fact ops facts)
            (foldl (lambda (t acc) ((bin-op (car t)) acc (interp-fact (cdr t))))
                   (interp-fact fact)
                   (zip ops facts)))))

(define (interp-fact fact)
  (cases arith-fact fact
    (fact-num (num) num)
    (fact-expr (expr) (interp-expr expr))))

(test-begin
 (check-equal? (interp-expr (scan&parse "1 + 2 + 3")) 6)
 (check-equal? (interp-expr (scan&parse "1 + 2 - 3")) 0)
 (check-equal? (interp-expr (scan&parse "(2 * 3 - 5) * 2")) 2))


