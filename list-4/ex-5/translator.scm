(module translator (lib "eopl.ss" "eopl")
  
  (require "lang.scm"
           (only-in racket foldr))

  (provide translation-of-program)
  ;;;;;;;;;;;;;;;; lexical address calculator ;;;;;;;;;;;;;;;;

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
      (call-exp (l r) (append (find-free l known) (find-free r known)))
      (else (report-invalid-source-expression expr))))
 

  ;; translation-of-program : Program -> Nameless-program
  ;; Page: 96
  (define translation-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (a-program                    
            (translation-of exp1 (init-senv)))))))

  ;; translation-of : Exp * Senv -> Nameless-exp
  ;; Page 97
  (define translation-of
    (lambda (exp senv)
      (cases expression exp
        (const-exp (num) (const-exp num))
        (diff-exp (exp1 exp2)
          (diff-exp
            (translation-of exp1 senv)
            (translation-of exp2 senv)))
        (zero?-exp (exp1)
          (zero?-exp
            (translation-of exp1 senv)))
        (if-exp (exp1 exp2 exp3)
          (if-exp
            (translation-of exp1 senv)
            (translation-of exp2 senv)
            (translation-of exp3 senv)))
        (var-exp (var)
          (nameless-var-exp
            (apply-senv senv var)))
        (let-exp (var exp1 body)
          (nameless-let-exp
            (translation-of exp1 senv)            
            (translation-of body
              (extend-senv var senv))))
        (proc-exp (var body)
          (let* ((vars (find-free body (list var)))
                 (indices (map (lambda (v) (apply-senv senv v)) vars))
                 (new-env (foldr extend-senv (empty-senv) vars)))
            (nameless-proc-exp
             (translation-of body (extend-senv var new-env))
             indices)))
        (call-exp (rator rand)
          (call-exp
            (translation-of rator senv)
            (translation-of rand senv)))
        (else (report-invalid-source-expression exp))
        )))

  (define report-invalid-source-expression
    (lambda (exp)
      (eopl:error 'value-of 
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
