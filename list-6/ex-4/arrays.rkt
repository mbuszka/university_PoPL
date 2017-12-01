#lang eopl

(provide (all-defined-out))

(require "store.scm")

(define (array? a) (reference? a))

(define (new-array cnt val) (newrefs cnt val))

(define (array-ref arr idx) (deref (+ arr idx)))

(define (array-set arr idx val) (setref! (+ arr idx) val))