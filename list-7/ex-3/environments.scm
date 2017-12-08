(module environments (lib "eopl.ss" "eopl")

  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm.

  (require "data-structures.scm")
  (require "store.scm")

  (provide init-env empty-env extend-env apply-env extend-env-rec)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69

  (define init-env
    (lambda ()
      (extend-env
       'i (newref (num-val 1))
       (extend-env
        'v (newref (num-val 5))
        (extend-env
         'x (newref (num-val 10))
         (empty-env))))))


  ;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  (define (extend-env-rec name var body env)
    (let* ((ref (newref 'undef))
           (new-env (extend-env name ref env))
           (proc (procedure var body new-env)))
      (setref! ref (proc-val proc))
      new-env))

  ;; Page: 86
  (define apply-env
    (lambda (env search-sym)
      (cases environment env
             (empty-env ()
                        (eopl:error 'apply-env "No binding for ~s" search-sym))
             (extend-env (var val saved-env)
                         (if (eqv? search-sym var)
                             val
                             (apply-env saved-env search-sym)))
             )))

  )
