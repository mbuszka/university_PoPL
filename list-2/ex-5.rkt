#lang racket

; Exercise 2.28 from EoPL

(require eopl
         rackunit)

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define parse-lc-exp
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp
            (car (cadr datum))
            (parse-lc-exp (caddr datum)))
           (app-exp
            (parse-lc-exp (car datum))
            (parse-lc-exp (cadr datum)))))
      (else (eopl:error "Invalid concrete syntax ~s" datum)))))

(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (lambda-exp (bound-var body) 
                  (list 'lambda (list bound-var)
                        (unparse-lc-exp body)))
      (app-exp (rator rand)
               (list 
                (unparse-lc-exp rator) (unparse-lc-exp rand))))))

(define (unparse exp)
  (let ([o (open-output-string)])
    (write (unparse-lc-exp exp) o)
    (get-output-string o)))

(define (parse str)
  (let ([i (open-input-string str)])
    (parse-lc-exp (read i))))

(test-begin
 (define (check-expression str)
   (check-equal? (unparse (parse str)) str))
 (check-expression "(lambda (x) x)")
 (check-expression "((lambda (x) x) y)")
 (check-expression "((lambda (x) x) (x y))")
 (check-expression "(lambda (y) (lambda (z) (x (y z))))"))
   