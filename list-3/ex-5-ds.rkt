#lang eopl

;;; Exercise 3.28 data structure representation

(require rackunit
         "../eopl3/chapter3/proc-lang/ds-rep/lang.scm"
         "../eopl3/chapter3/proc-lang/ds-rep/environments.scm")

(define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?)))

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

(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

(define-datatype proc proc?
  (procedure
   (var symbol?)
   (body expression?)))

(define apply-procedure
    (lambda (proc1 val env)
      (cases proc proc1
        (procedure (var body)
                   (value-of body (extend-env var val env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
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
                           (extend-env var val1 env))))
      ;; We dont save current environment
      (proc-exp (var body)
                (proc-val (procedure var body)))
      ;; Instead we supply it at call time
      (call-exp (rator rand)
                (let ((proc (expval->proc (value-of rator env)))
                      (arg (value-of rand env)))
                  (apply-procedure proc arg env)))
      )))

(define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
                   (value-of exp1 (init-env))))))

(define (run str) (value-of-program (scan&parse str)))

(test-begin
 (check-equal?
  (run "let z = 1 in let f = proc (y) - (x, y) in let x = 4 in (f 1)")
  (num-val 3))
 (check-equal?
  (run "let x = 1 in let f = proc (y) - (x, y) in let x = 4 in (f 1)")
  (num-val 3)))