(module interp (lib "eopl.ss" "eopl")

  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  (require (only-in racket foldl))

  (provide value-of-program value-of)

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (answer->val (value-of exp1 (init-env) (empty-store)))))))

  ;; value-of : Exp * Env * Store -> Answer
  ;; Page: 113
  (define (value-of exp env store)
    (cases expression exp
      (const-exp (num) (an-answer (num-val num) store))
      (var-exp (var) (an-answer (apply-env env var) store))

      (diff-exp (exp1 exp2)
        (let* ((ans1 (value-of exp1 env store))
               (ans2 (value-of exp2 env (answer->store ans1)))
               (num1 (expval->num (answer->val ans1)))
               (num2 (expval->num (answer->val ans2))))
          (an-answer
            (num-val
              (- num1 num2))
              (answer->store ans2))))

      (zero?-exp (exp1)
        (let* ((ans1 (value-of exp1 env store))
               (num1 (expval->num (answer->val ans1)))
               (new-store (answer->store ans1)))
            (if (zero? num1)
              (an-answer (bool-val #t) new-store)
              (an-answer (bool-val #f) new-store))))

      (if-exp (exp1 exp2 exp3)
        (let* ((ans1 (value-of exp1 env store))
               (val1 (answer->val ans1))
               (new-store (answer->store ans1)))
          (if (expval->bool val1)
            (value-of exp2 env new-store)
            (value-of exp3 env new-store))))

      (let-exp (var exp1 body)
        (cases answer (value-of exp1 env store)
          (an-answer (val new-store)
            (value-of body
              (extend-env var val env)
              new-store))))

      (proc-exp (var body)
        (an-answer
          (proc-val (procedure var body env))
          store))

      (call-exp (rator rand)
        (let* ((ans1 (value-of rator env store))
               (proc (expval->proc (answer->val ans1)))
               (ans2 (value-of rand env (answer->store ans1))))
          (apply-procedure proc (answer->val ans2) (answer->store ans2))))

      (letrec-exp (p-names b-vars p-bodies letrec-body)
        (value-of letrec-body
          (extend-env-rec* p-names b-vars p-bodies env)
          store))

      (begin-exp (exp1 exps)
        (foldl
          (lambda (v acc) (value-of v env (answer->store acc)))
          (value-of exp1 env store)
          exps))

      (newref-exp (exp1)
        (cases answer (value-of exp1 env store)
          (an-answer (val new-store)
            (let ((p (newref new-store val)))
              (an-answer
                (ref-val (car p))
                (cdr p))))))

      (deref-exp (exp1)
        (let* ((ans1 (value-of exp1 env store))
               (ref1 (expval->ref (answer->val ans1))))
            (an-answer
              (deref ref1)
              (answer->store ans1))))

      (setref-exp (exp1 exp2)
        (let* ((ans1 (value-of exp1 env store))
               (ref (expval->ref (answer->val ans1)))
               (ans2 (value-of exp2 env (answer->store ans1)))
               (new-store (setref (answer->store ans2) ref (answer->val ans2))))
          (an-answer
            (num-val 23)
            new-store)))
      ))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;;
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define (apply-procedure proc1 arg store)
    (cases proc proc1
      (procedure (var body saved-env)
	      (let ((new-env (extend-env var arg saved-env)))
          (value-of body new-env store)))))

  )
