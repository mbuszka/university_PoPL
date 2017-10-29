#lang eopl

(require rackunit
         (only-in racket foldl)
         "../list-2/ex-1.rkt"
         "../util.rkt")

;;; Exercises 1, 2 and 3 (3.9, 3.10 and 3.17 from EoPL)

;;; Data types

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (list-val
   (list (list-of expval?))))

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define (expval->list v)
  (cases expval v
    (list-val (lst) lst)
    (else (expval-extractor-error 'list v))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;; Interpreter
(define (run str) (value-of-program (parse str)))

(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (empty-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp

      ; Exercise 1
      (cons-exp (hd tl)
                (let ((head (value-of hd env))
                      (tail (expval->list (value-of tl env))))
                  (list-val (cons head tail))))
      (head-exp (ex) (car (expval->list (value-of ex env))))
      (tail-exp (ex) (list-val (cdr (expval->list (value-of ex env)))))
      (null?-exp (ex) (bool-val (null? (expval->list (value-of ex env)))))
      (nil-exp () (list-val '()))

      ; Exercise 2
      (list-exp (lit)
                (cases list-literal lit
                  (empty-list () (list-val '()))
                  (non-empty-list
                   (hd tl)
                   (let ((head (value-of hd env))
                         (tail (map (lambda (x) (value-of x env)) tl)))
                     (list-val (cons head tail))))))

      ; Exercise 3
      (let*-exp (var exp1 vars exps exp2)
                (let* ((lets (zip vars exps))
                       (env1 (foldl
                              (lambda (pair acc)
                                (extend-env (car pair) (value-of (cdr pair) acc) acc))
                              env
                              (cons (cons var exp1) lets))))
                  (value-of exp2 env1)))
                  
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env)))))))



;;; Parser and lexer

(define lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
(define grammar
  '((program (expression) a-program)

    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    ; Exercise 1
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("head" "(" expression ")") head-exp)
    (expression ("tail" "(" expression ")") tail-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("nil") nil-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)
    
    ; Exercise 2
    (expression ("list" "(" list-literal) list-exp)
    (list-literal (")") empty-list)
    (list-literal (expression (arbno "," expression) ")") non-empty-list)

    ; Exercise 3
    (expression
     ("let*" identifier "=" expression
             (arbno identifier "=" expression)
             "in" expression)
     let*-exp)
    ))

(sllgen:make-define-datatypes lexical-spec grammar)

(define parse (sllgen:make-string-parser lexical-spec grammar))

;;; Tests

(test-begin
 ; Exercises 1 and 2
 (check-equal?
  (run "list (1, 2, 3)")
  (list-val (list (num-val 1) (num-val 2) (num-val 3))))
 (check-equal?
  (run "let x = cons (1, nil) in list (2, x)")
  (list-val (list (num-val 2) (list-val (list (num-val 1))))))
 (check-equal? (run "null? ( nil )") (bool-val #t))
 (check-equal? (run "null? ( cons (1, nil) )") (bool-val #f))
 (check-equal? (run "nil") (run "list ()"))
 (check-equal?
  (run "let x = - (4, 2) in cons (x, list (3, 4))")
  (list-val (list (num-val 2) (num-val 3) (num-val 4))))
 (check-equal?
  (run "let x = - (4, 2) in head (cons (x, list (3, 4)))")
  (run "let x = 2 in x"))
 (check-equal?
  (run "tail (list (1, 2, 3, 4))")
  (run "list (2, 3, 4)"))

 ;Exercise 3
 (check-equal?
  (run "let x = 30 in let* x = - (x, 1) y = - (x, 2) in - (x, y)")
  (num-val 2))
 (check-equal?
  (run "let* x = 1 x = -(4, x) x = - (x, 8) y = x in y")
  (num-val -5)))
