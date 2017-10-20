#lang eopl

; Exercise 2.29 from EoPL

(require rackunit)

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-vars (list-of symbol?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rands (list-of lc-exp?))))

(define (parse-lc-exp datum)
  (cond
    ((symbol? datum) (var-exp datum))
    ((pair? datum)
     (if (eqv? 'lambda (car datum))
         (lambda-exp (cadr datum)
                     (parse-lc-exp (caddr datum)))
         (app-exp (parse-lc-exp (car datum))
                  (map parse-lc-exp (cdr datum)))))))

(test-begin
 (check-equal? (parse-lc-exp 'a) (var-exp 'a))
 (check-equal? (parse-lc-exp '(a b c d))
               (app-exp
                (var-exp 'a)
                (list (var-exp 'b) (var-exp 'c) (var-exp 'd))))
 (check-equal? (parse-lc-exp '(lambda (a b c) a))
               (lambda-exp
                '(a b c)
                (var-exp 'a)))
 (check-equal? (parse-lc-exp '(lambda (x y z) (x y z)))
               (lambda-exp
                '(x y z)
                (app-exp
                 (var-exp 'x)
                 (list (var-exp 'y) (var-exp 'z))))))
 
 
                           