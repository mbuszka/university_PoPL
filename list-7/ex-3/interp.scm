(module interp (lib "eopl.ss" "eopl")
  
  ;; cps interpreter for the LETREC language, using the data structure
  ;; representation of continuations (Figure 5.3).

  ;; exercise: rewrite this using the procedural representation of
  ;; continuations (Figure 5.2).

  ;; exercise: rewrite this using a trampoline (page 159).

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  (require (only-in racket foldl))

  (provide value-of-program value-of/k)

  (define prints '())

  (define (add-print v)
    (set! prints (cons (expval->printable v) prints)))


  (define expval->printable
    (lambda (val)
      (cases expval val
             (proc-val (p)
                       (cases proc p
                              (procedure (var body saved-env)
                                         (list 'procedure var '... ))))
             (bool-val (b) b)
             (num-val (n) n)
             (else val))))


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> FinalAnswer
  ;; Page: 143 and 154
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
             (a-program (exp1)
                        (begin
                          (initialize-store!)
                          (set! prints '())
                          (result-of exp1 (init-env) end-c-cont)
                          (reverse prints))))))

  (define (result-of smt env cont)
    (cases statement smt
           (assign-smt (var exp)
                       (value-of/k exp env (assign-cont (apply-env env var) cont)))
           (print-smt (exp)
                      (value-of/k exp env (print-cont cont)))
           (block-smt (smt1 smts)
                      (result-of smt1 env (block-c-cont smts env cont)))
           (if-smt (exp smt1 smt2)
                   (value-of/k exp (if-smt-cont smt1 smt2 env cont)))
           (while-smt (exp smt1)
                      (value-of/k exp env (while-cont exp smt1 env cont)))
           (var-smt (vars smt1)
                    (let ((new-env (foldl (lambda (v acc) (extend-env v (newref 'undef) acc))
                                          env
                                          vars)))
                      (result-of smt1
                                 new-env
                                 cont)))
           ))

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 143--146, and 154
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        (const-exp (num) (apply-cont cont (num-val num)))
        (var-exp (var) (apply-cont cont (deref (apply-env env var))))
        (proc-exp (var body)
          (apply-cont cont
            (proc-val (procedure var body env))))
        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont))
        (zero?-exp (exp1)
          (value-of/k exp1 env
            (zero1-cont cont)))
        (let-exp (var exp1 body)
          (value-of/k exp1 env
            (let-exp-cont var body env cont)))
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)))
        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)))
        (call-exp (rator rand)
          (value-of/k rator env
                      (rator-cont rand env cont)))
        (assign-exp (var exp1)
                    (value-of/k exp1 env (set-cont (apply-env env var) cont)))
        )))

  (define (block-c-cont smts env cont)
    (lambda () (if (null? smts)
                   (apply-c-cont cont)
                   (result-of (car smts) env (block-c-cont (cdr smts) env cont)))))

  (define (while-c-cont exp env cont)
    (lambda () (value-of/k exp env cont)))

  (define (end-c-cont) (eopl:printf "End of computation.\n"))

  (define (apply-c-cont cont) (cont))

  ;; apply-cont : Cont * ExpVal -> FinalAnswer
  ;; Page: 148
  (define apply-cont
    (lambda (cont val)
      (cases continuation cont
             (end-cont ()
                       (begin
                         (eopl:printf
                          "End of computation.~%")
                         val))
             ;; or (logged-print val)  ; if you use drscheme-init-cps.scm
             (zero1-cont (saved-cont)
                         (apply-cont saved-cont
                                     (bool-val
                                      (zero? (expval->num val)))))
             (let-exp-cont (var body saved-env saved-cont)
                           (value-of/k body
                                       (extend-env var (newref val) saved-env) saved-cont))
             (if-test-cont (exp2 exp3 saved-env saved-cont)
                           (if (expval->bool val)
                               (value-of/k exp2 saved-env saved-cont)
                               (value-of/k exp3 saved-env saved-cont)))
             (diff1-cont (exp2 saved-env saved-cont)
                         (value-of/k exp2
                                     saved-env (diff2-cont val saved-cont)))
             (diff2-cont (val1 saved-cont)
                         (let ((num1 (expval->num val1))
                               (num2 (expval->num val)))
                           (apply-cont saved-cont
                                       (num-val (- num1 num2)))))
             (rator-cont (rand saved-env saved-cont)
                         (value-of/k rand saved-env
                                     (rand-cont val saved-cont)))
             (rand-cont (val1 saved-cont)
                        (let ((proc (expval->proc val1)))
                          (apply-procedure/k proc val saved-cont)))
             (set-cont (var saved-cont)
                       (begin
                         (setref! var val)
                         (apply-cont saved-cont (bool-val #t))))
             (assign-cont (var saved-cont)
                          (begin
                            (setref! var val)
                            (apply-c-cont saved-cont)))
             (print-cont (saved-cont)
                         (begin
                           (add-print val)
                           (apply-c-cont saved-cont)))
             (if-smt-cont (smt1 smt2 env saved-cont)
                          (if (expval->bool val)
                              (result-of smt1 env saved-cont)
                              (result-of smt2 env saved-cont)))
             (while-cont (exp smt env saved-cont)
                         (if (expval->bool val)
                             (result-of smt env (while-c-cont exp env cont))
                             (apply-c-cont saved-cont)))
             )))

  ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
  ;; Page 152 and 155
  (define apply-procedure/k
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var (newref arg) saved-env)
            cont)))))

  )

