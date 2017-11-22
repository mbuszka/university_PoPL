(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for letrec-lang.

  (require "lang.scm"
           (only-in racket foldl)
           racket/match)

  (provide (all-defined-out))               ; too many things to list

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

  ;; expval->num : ExpVal -> Int
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  ;; expval->proc : ExpVal -> Proc
  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ;; proc? : SchemeVal -> Bool
  ;; procedure : Var * Exp * Env -> Proc
  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))

  ;; Page: 86
  (define-datatype environment environment?
    (empty-env)
    (extend-env
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    ;; Recursively extended environment contains list od identifiers and a vector
    ;; of precedure bodies
    (extended-env-rec
      (p-names (list-of symbol?))
      (procedures vector?)
      (saved-env environment?)))


  ;;; Exercise 3 (3.36 from EoPL)
  ;; Helper function wich zips two lists
  (define (zip lst-1 lst-2)
    (match (cons lst-1 lst-2)
      ((cons '() _) '())
      ((cons _ '()) '())
      ((cons (cons h1 t1) (cons h2 t2)) (cons (cons h1 h2) (zip t1 t2)))))

  (define (extend-env-rec p-names p-vars bodies saved-env)
    (let* ((vec (make-vector (length p-names)))
           ;; first create new environment with uninitialized vector
           (new-env (extended-env-rec p-names vec saved-env))
           (pairs (zip p-vars bodies))
           ;; then create all procedures with this new environment
           (procs (map (lambda (p)
                         (proc-val (procedure (car p) (cdr p) new-env)))
                         pairs)))
      ;; finally put the procedures inside vector
      (foldl (lambda (p n)
               (vector-set! vec n p)
               (+ n 1))
             0
             procs)
      ;; and return this new environment
      new-env))
)
