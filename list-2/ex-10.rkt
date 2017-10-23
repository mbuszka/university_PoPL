#lang eopl

(require rackunit)

;; ZeroOrOne ::= 0 | 1

;; walk : Listof(ZeroOrOne) -> (Listof(ZeroOrOne) -> Bool) -> Bool	
(define (walk xs k)
  (if (and (not (null? xs)) (eqv? (car xs) 0))
      (walk (cdr xs)
            (lambda (ys)
              (if (and (not (null? ys)) (eqv? (car ys) 1))
                  (k (cdr ys))
                  #f)))
      (k xs)))

;; main : Listof(ZeroOrOne) -> Bool
(define (main xs)
  (walk xs (lambda (ys) (null? ys))))

;; This program recognizes grammar S ::= epsilon | 0S1

(define (walk-def xs k)
  (if (and (not (null? xs)) (eqv? (car xs) 0))
      (walk-def (cdr xs)
            (constr-1 k))
      (apply-def k xs)))

(define (main-def xs)
  (walk-def xs (constr-2)))

(define-datatype constr constr?
  (constr-1 (k constr?))
  (constr-2))

(define (apply-def c ys)
  (cases constr c
    (constr-1 (k)
              (if (and (not (null? ys)) (eqv? (car ys) 1))
                  (apply-def k (cdr ys))
                  #f))
    (constr-2 ()
              (null? ys))))

(test-begin
 (let ((xs '())) (check-equal? (main xs) (main-def xs)))
 (let ((xs '(0))) (check-equal? (main xs) (main-def xs)))
 (let ((xs '(1))) (check-equal? (main xs) (main-def xs)))
 (let ((xs '(0 1))) (check-equal? (main xs) (main-def xs)))
 (let ((xs '(1 0))) (check-equal? (main xs) (main-def xs)))
 (let ((xs '(1 1))) (check-equal? (main xs) (main-def xs)))
 (let ((xs '(0 0 1 1))) (check-equal? (main xs) (main-def xs)))
 (let ((xs '(0 0 0 1 1 1))) (check-equal? (main xs) (main-def xs))))