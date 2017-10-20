#lang eopl

; Exercise 2.31 from EoPL

(require rackunit)

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))


; parse prefix of a list and return parsed expression and rest of the list as a pair
(define (parse-p-exp lst)
  (if (null? lst)
      (eopl:error "Empty list is not a valid prefix expression")
      (cond
        ((eqv? (car lst) '-)
         (let*
             ((lhs (parse-p-exp (cdr lst)))
              (rhs (parse-p-exp (cdr lhs))))
           (cons (diff-exp (car lhs) (car rhs)) (cdr rhs))))
        ((integer? (car lst)) (cons (const-exp (car lst)) (cdr lst)))
        (else (eopl:error "Invalid token ~s" (car lst))))))

(define (parser lst) (car (parse-p-exp lst)))

(test-begin
 (check-equal? (parser '(- 1 2))
               (diff-exp
                (const-exp 1)
                (const-exp 2)))
 (check-equal? (parser '(- - 3 2 - 4 - 12 7))
               (diff-exp
                (diff-exp
                 (const-exp 3)
                 (const-exp 2))
                (diff-exp
                 (const-exp 4)
                 (diff-exp
                  (const-exp 12)
                  (const-exp 7))))))