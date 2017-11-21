(module translator (lib "eopl.ss" "eopl")

  (require "lang.scm")

  (provide translation-of-program)
  ;;;;;;;;;;;;;;;; lexical address calculator ;;;;;;;;;;;;;;;;

  ;; translation-of-program : Program -> Nameless-program
  ;; Page: 96
  (define translation-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (a-program
            (translation-of exp1 (init-senv) '()))))))

  (define (shift-bindings k ast)
    (cases expression ast
      (const-exp (num) (const-exp num))
      (diff-exp (l r)
        (diff-exp
          (shift-bindings k l)
          (shift-bindings k r)))
      (zero?-exp (exp)
        (zero?-exp (shift-bindings k exp)))
      (if-exp (c t e)
        (if-exp
          (shift-bindings k c)
          (shift-bindings k t)
          (shift-bindings k e)))
      (call-exp (rator rand)
         (call-exp
          (shift-bindings k rator)
          (shift-bindings k rand)))
      (nameless-var-exp (num)
        (nameless-var-exp (if (eqv? num 0)
                              num
                              (+ num k))))
      (nameless-let-exp (exp body)
        (nameless-let-exp
          (shift-bindings k exp)
          (shift-bindings k body)))
      (nameless-proc-exp (body)
        (nameless-proc-exp (shift-bindings k body)))
      (else (report-invalid-source-expression ast))))

  ;; translation-of : Exp * Senv -> Nameless-exp
  ;; Page 97
  (define translation-of
    (lambda (exp senv known)
      (cases expression exp
        (const-exp (num) (const-exp num))
        (diff-exp (exp1 exp2)
          (diff-exp
            (translation-of exp1 senv known)
            (translation-of exp2 senv known)))
        (zero?-exp (exp1)
          (zero?-exp
            (translation-of exp1 senv known)))
        (if-exp (exp1 exp2 exp3)
          (if-exp
            (translation-of exp1 senv known)
            (translation-of exp2 senv known)
            (translation-of exp3 senv known)))
        (var-exp (var)
          (let* ((f-orig (assoc var known))
                 (k (apply-senv senv var)))
            (if f-orig
              (shift-bindings k (cdr f-orig))
              (nameless-var-exp k))))
        (let-exp (var exp1 body)
          (let* ((bound (translation-of exp1 senv known))
                 (known1 (cases expression bound
                           (nameless-proc-exp (b)
                             (cons (cons var bound) known))
                           (else known))))
            (nameless-let-exp
              bound
              (translation-of body
                (extend-senv var senv)
                known1))))
        (proc-exp (var body)
          (nameless-proc-exp
            (translation-of body
              (extend-senv var senv)
              known)))
        (call-exp (rator rand)
          (call-exp
            (translation-of rator senv known)
            (translation-of rand senv known)))
        (else (report-invalid-source-expression exp))
        )))

  (define report-invalid-source-expression
    (lambda (exp)
      (eopl:error 'translation-of
        "Illegal expression in source code: ~s" exp)))

   ;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;

  ;;; Senv = Listof(Sym)
  ;;; Lexaddr = N

  ;; empty-senv : () -> Senv
  ;; Page: 95
  (define empty-senv
    (lambda ()
      '()))

  ;; extend-senv : Var * Senv -> Senv
  ;; Page: 95
  (define extend-senv
    (lambda (var senv)
      (cons var senv)))

  ;; apply-senv : Senv * Var -> Lexaddr
  ;; Page: 95
  (define apply-senv
    (lambda (senv var)
      (cond
        ((null? senv) (report-unbound-var var))
        ((eqv? var (car senv))
         0)
        (else
          (+ 1 (apply-senv (cdr senv) var))))))

  (define report-unbound-var
    (lambda (var)
      (eopl:error 'translation-of "unbound variable in code: ~s" var)))

  ;; init-senv : () -> Senv
  ;; Page: 96
  (define init-senv
    (lambda ()
      (extend-senv 'i
        (extend-senv 'v
          (extend-senv 'x
            (empty-senv))))))

  )
