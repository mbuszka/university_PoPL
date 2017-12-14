(module interp (lib "eopl.ss" "eopl")

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of/k)
  (provide trace-apply-procedure)

  (define trace-apply-procedure (make-parameter #f))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;


  (define-datatype continuation continuation?
    (end-cont)                          ; []
    (diff1-cont                       ; cont[(- [] (value-of e2 env))]
      (exp2 expression?)
      (env environment?)
      (cont continuation?))
    (diff2-cont                         ; cont[(- val1 [])]
      (val1 expval?)
      (cont continuation?))
    (unop-arg-cont
      (unop unary-op?)
      (cont continuation?))
    (if-test-cont
      (exp2 expression?)
      (exp3 expression?)
      (env environment?)
      (cont continuation?))
    (rator-cont            ; cont[(apply-proc [] (value-of rand env))]
      (rand expression?)
      (env environment?)
      (cont continuation?))
    (rand-cont                          ; cont[(apply-proc val1 [])]
      (val1 expval?)
      (cont continuation?))
    (try-cont
      (var symbol?)
      (handler-exp expression?)
      (env environment?)
      (cont continuation?)
      (err continuation?))
    (raise1-cont)
    )

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (body)
          (value-of/k body (init-env) (end-cont) (end-cont))))))

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 173
  (define value-of/k
    (lambda (exp env cont err)
      (cases expression exp

        (const-exp (num) (apply-cont cont (num-val num) err))

        (const-list-exp (nums)
          (apply-cont cont
            (list-val (map num-val nums)) err))

        (var-exp (var) (apply-cont cont (apply-env env var) err))

        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont) err))

        (unop-exp (unop exp1)
          (value-of/k exp1 env
            (unop-arg-cont unop cont) err))

        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont) err))

        (proc-exp (var body)
          (apply-cont cont
            (proc-val
              (procedure var body env)) err))

        (call-exp (rator rand)
          (value-of/k rator env
            (rator-cont rand env cont) err))

        ;; make let a macro, because I'm too lazy to add the extra
        ;; continuation
        (let-exp (var exp1 body)
          (value-of/k
            (call-exp (proc-exp var body) exp1)
            env
            cont err))

        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k
            letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont err))

        (try-exp (exp1 var handler-exp)
          (value-of/k exp1 env
            cont (try-cont var handler-exp env cont err)))

        (raise-exp (exp1)
          (value-of/k exp1 env
            (raise1-cont) err)))))

  ;; apply-cont : continuation * expval -> final-expval

  (define apply-cont
    (lambda (cont val err)
      (cases continuation cont
        (end-cont () val)
        (diff1-cont (exp2 saved-env saved-cont)
          (value-of/k exp2 saved-env (diff2-cont val saved-cont) err))
        (diff2-cont (val1 saved-cont)
          (let ((n1 (expval->num val1))
                (n2 (expval->num val)))
            (apply-cont saved-cont
              (num-val (- n1 n2)) err)))
        (unop-arg-cont (unop cont)
          (apply-cont cont
            (apply-unop unop val) err))
        (if-test-cont (exp2 exp3 env cont)
          (if (expval->bool val)
            (value-of/k exp2 env cont err)
            (value-of/k exp3 env cont err)))
        (rator-cont (rand saved-env saved-cont)
          (value-of/k rand saved-env
            (rand-cont val saved-cont) err))
        (rand-cont (val1 saved-cont)
          (let ((proc (expval->proc val1)))
            (apply-procedure proc val saved-cont err)))
        ;; the body of the try finished normally-- don't evaluate the handler
        (try-cont (var handler-exp saved-env saved-cont saved-err)
                  (eopl:error 'apply-cont "encountered try cont in normal execution"))
                  ;; val is the value of the argument to raise
        (raise1-cont ()
          ;; we put the short argument first to make the trace more readable.
          (apply-handler val err))
        )))

  ;; apply-handler : ExpVal * Cont -> FinalAnswer
  (define apply-handler
    (lambda (val err)
      (cases continuation err
        ;; interesting cases
        (try-cont (var handler-exp saved-env saved-cont saved-err)
          (value-of/k handler-exp
            (extend-env var val saved-env)
            saved-cont saved-err))

        (end-cont () (eopl:error 'apply-handler "uncaught exception!"))
        (else (eopl:error 'apply-handler "unexpected continuation"))
        ;; otherwise, just look for the handler...
;        (diff1-cont (exp2 saved-env saved-cont)
;          (apply-handler val saved-cont))
;        (diff2-cont (val1 saved-cont)
;          (apply-handler val saved-cont))
;        (if-test-cont (exp2 exp3 env saved-cont)
;          (apply-handler val saved-cont))
;        (unop-arg-cont (unop saved-cont)
;          (apply-handler val saved-cont))
;        (rator-cont (rand saved-env saved-cont)
;          (apply-handler val saved-cont))
;        (rand-cont (val1 saved-cont)
;          (apply-handler val saved-cont))
;        (raise1-cont (cont)
;          (apply-handler val cont))
        )))


  ;; apply-procedure : procedure * expval * cont -> final-expval

  (define apply-procedure
    (lambda (proc1 arg cont err)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var arg saved-env)
            cont err)))))


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
