#lang racket

(define (subst new old slist)
  (map (lambda (sexp)
         (if (symbol? sexp)
             (if (eq? sexp old) new sexp)
             (subst new old sexp))) slist))