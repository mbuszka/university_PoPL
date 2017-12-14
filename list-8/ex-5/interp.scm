(module interp (lib "eopl.ss" "eopl")

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of/k)
  (provide trace-apply-procedure)

  (define trace-apply-procedure (make-parameter #f))

  ;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (body)
          (value-of/k (translation-of body) (init-env) (end-cont))))))


  (define (translation-of exp)
    (cases expression exp
           (letcc-exp (var exp1)
                      (callcc-exp (proc-exp var (translation-of exp1))))
           (throw-exp (exp1 exp2)
                      (call-exp (translation-of exp2) (translation-of exp1)))
           (diff-exp (e1 e2) (diff-exp (translation-of e1) (translation-of e2)))
           (unop-exp (u e) (unop-exp u (translation-of e)))
           (if-exp (e1 e2 e3)
                   (if-exp (translation-of e1) (translation-of e2) (translation-of e3)))
           (proc-exp (v b) (proc-exp v (translation-of b)))
           (call-exp (e1 e2) (call-exp (translation-of e1) (translation-of e2)))
           (let-exp (v e1 e2) (let-exp v (translation-of e1) (translation-of e2)))
           (letrec-exp (n v e1 e2) (letrec-exp n v (translation-of e1) (translation-of e2)))
           (try-exp (e1 v e2) (try-exp (translation-of e1) v (translation-of e2)))
           (raise-exp (e1) (raise-exp (translation-of e1)))
           (callcc-exp (e1) (callcc-exp (translation-of e1)))
           (else exp)))


  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 173
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp

        (const-exp (num) (apply-cont cont (num-val num)))

        (const-list-exp (nums)
          (apply-cont cont
            (list-val (map num-val nums))))

        (var-exp (var) (apply-cont cont (apply-env env var)))

        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)))

        (unop-exp (unop exp1)
          (value-of/k exp1 env
            (unop-arg-cont unop cont)))

        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)))

        (proc-exp (var body)
          (apply-cont cont
            (proc-val
              (procedure var body env))))

        (call-exp (rator rand)
          (value-of/k rator env
            (rator-cont rand env cont)))

        ;; make let a macro, because I'm too lazy to add the extra
        ;; continuation
        (let-exp (var exp1 body)
          (value-of/k
            (call-exp (proc-exp var body) exp1)
            env
            cont))

        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k
            letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont))

        (try-exp (exp1 var handler-exp)
          (value-of/k exp1 env
                      (try-cont var handler-exp env cont)))

        (raise-exp (exp1)
                   (value-of/k exp1 env
                               (raise-cont cont)))

        (callcc-exp (exp1)
                    (value-of/k exp1 env (callcc-cont cont)))
        (else (eopl:error 'value-of/k "unexpected expression ~s" exp))
        )))

  ;; apply-cont : continuation * expval -> final-expval

  (define (apply-cont cont val) ((car cont) val))
  (define (apply-handler cont val) ((cdr cont) val))

  (define (callcc-cont cont)
    (cons (lambda (val) (apply-procedure (expval->proc val) (proc-val (cont-p cont)) cont))
          (cdr cont)))

  (define (end-cont)
    (cons (lambda (val) val)
          (lambda () (eopl:error 'apply-handler "uncaught exception!"))))

  (define (diff1-cont exp2 env cont)
    (cons (lambda (val)
            (value-of/k exp2 env (diff2-cont val cont)))
          (cdr cont)))

  (define (diff2-cont val1 cont)
    (cons (lambda (val)
            (let ((n1 (expval->num val1))
                  (n2 (expval->num val)))
              (apply-cont cont
                          (num-val (- n1 n2)))))
          (cdr cont)))

  (define (unop-arg-cont unop cont)
    (cons (lambda (val)
            (apply-cont cont (apply-unop unop val)))
          (cdr cont)))

  (define (if-test-cont exp2 exp3 env cont)
    (cons (lambda (val)
            (if (expval->bool val)
                (value-of/k exp2 env cont)
                (value-of/k exp3 env cont)))
          (cdr cont)))

  (define (rator-cont rand env cont)
    (cons (lambda (val)
            (value-of/k rand env (rand-cont val cont)))
          (cdr cont)))

  (define (rand-cont val1 saved-cont)
    (cons (lambda (val)
            (let ((proc (expval->proc val1)))
              (apply-procedure proc val saved-cont)))
          (cdr saved-cont)))

  (define (try-cont var handler-exp env cont)
    (cons (car cont)
          (lambda (val)
            (value-of/k handler-exp (extend-env var val env) cont))))

  (define (raise-cont cont)
    (cons (lambda (val) (apply-handler cont val))
          (cdr cont)))

  ;; apply-procedure : procedure * expval * cont -> final-expval

  (define apply-procedure
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var arg saved-env)
            cont))
        (cont-p (cont1)
                (apply-cont cont1 arg))
        )))


  (define apply-unop
    (lambda (unop val)
      (cases unary-op unop
        (null?-unop ()
          (bool-val
            (null? (expval->list val))))
        (car-unop ()
          (car (expval->list val)))
        (cdr-unop ()
          (list-val (cdr (expval->list val))))
        (zero?-unop ()
          (bool-val
            (zero? (expval->num val)))))))


  ;; to get the detailed trace:
  ;; (trace value-of/k apply-cont apply-handler)

  )
