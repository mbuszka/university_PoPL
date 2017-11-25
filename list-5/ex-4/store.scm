(module store (lib "eopl.ss" "eopl")

  (require "drscheme-init.scm"
           (only-in racket vector-append))

  (provide reference? newref deref setref store? empty-store)

  ;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;


  (define store? vector?)

  ;; empty-store : () -> Sto
  ;; Page: 111
  (define empty-store
    (lambda () (make-vector 0)))

  ;; reference? : SchemeVal -> Bool
  ;; Page: 111
  (define reference?
    (lambda (v)
      (integer? v)))

  ;; newref : ExpVal -> Ref
  ;; Page: 111
  (define (newref store val)
    (let ((next-ref (vector-length store))
          (new-store (vector-append store (vector val))))
      (cons next-ref store)))

  ;; deref : Ref -> ExpVal
  ;; Page 111
  (define (deref store ref)
      (vector-ref store ref))

  ;; setref! : Ref * ExpVal -> Unspecified
  ;; Page: 112
  (define (setref store ref val)
    (vector-set! store ref val)
    store)

  )
