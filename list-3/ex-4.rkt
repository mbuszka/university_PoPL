#lang eopl

(require "../eopl3/chapter3/proc-lang/ds-rep/data-structures.scm"
         "../eopl3/chapter3/proc-lang/ds-rep/environments.scm"
         "../eopl3/chapter3/proc-lang/ds-rep/lang.scm"
         (only-in racket foldl)
         rackunit)

;;; Exercise 4

(define (find-free expr known)
  (cases expression expr
    (const-exp (n) '())
    (diff-exp (l r) (append (find-free l known) (find-free r known)))
    (zero?-exp (e) (find-free e known))
    (if-exp (i t e)
            (append
             (find-free i known)
             (find-free t known)
             (find-free e known)))
    (var-exp (v)
             (if (member v known)
                 '()
                 (list v)))
    (let-exp (var val body)
             (append (find-free val known) (find-free body (cons var known))))
    (proc-exp (var body) (find-free body (cons var known)))
    (call-exp (l r) (append (find-free l known) (find-free r known)))))
    
(define (free expr) (find-free expr '()))

(define (trim-env env free)
  (foldl (lambda (v acc) (extend-env v (apply-env env v) acc))
        (empty-env)
        free))

(define (unwrap-pgm pgm)
  (cases program pgm
    (a-program (expr) expr)))

(define (parse-unwrap str)
  (unwrap-pgm (scan&parse str)))

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
        
      (proc-exp (var body)
                (let* ((vars (find-free body (list var)))
                       (trimmed (trim-env env vars)))
                  (proc-val (procedure var body trimmed))))

      (call-exp (rator rand)
                (let ((proc (expval->proc (value-of rator env)))
                      (arg (value-of rand env)))
                  (apply-procedure proc arg))))))

(define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env))))))

(define (run str) (value-of-program (scan&parse str)))

(test-begin
 (check-equal?
  (free (parse-unwrap "- (x, let y = - (2, z) in y)"))
  '(x z))
 (check-equal?
  (free (parse-unwrap "let x = 2 in let y = 3 in let z = 4 in proc (a) -(a, b)"))
  '(b))
 (check-equal?
  (run "let x = 2 in let z = 3 in let f = proc (y) -(x, y) in (f 1)")
  (num-val 1)))
