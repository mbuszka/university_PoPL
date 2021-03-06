(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")

  (provide (all-defined-out))               ; too many things to list


  ; (define control-continuation? procedure?)

  (define-datatype control-continuation control-continuation?
    (while-c-cont
     (exp expression?)
     (env environment?)
     (cont continuation?))
    (block-c-cont
     (smts (list-of statement?))
     (env environment?)
     (cont control-continuation?))
    (end-c-cont))

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val
      (proc proc?)))

;;; extractors:

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

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

  ;; Page: 148
  (define identifier? symbol?)

  (define-datatype continuation continuation?
    (end-cont)
    (zero1-cont
      (saved-cont continuation?))
    (let-exp-cont
      (var identifier?)
      (body expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (if-test-cont
      (exp2 expression?)
      (exp3 expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (diff1-cont
      (exp2 expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (diff2-cont
      (val1 expval?)
      (saved-cont continuation?))
    (rator-cont
      (rand expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (rand-cont
      (val1 expval?)
      (saved-cont continuation?))
    (set-cont
     (var reference?)
     (saved-cont continuation?))
    (assign-cont
     (ref reference?)
     (saved-cont control-continuation?))
    (print-cont
     (saved-cont control-continuation?))
    (if-smt-cont
     (smt1 statement?)
     (smt2 statement?)
     (saved-env environment?)
     (saved-cont control-continuation?))
    (while-cont
     (exp expression?)
     (smt statement?)
     (saved-env environment?)
     (saved-cont control-continuation?))
    )

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))
  
;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval reference?)
      (saved-env environment?)))

)
