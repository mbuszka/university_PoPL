#lang racket

(define (leaf n) n)

(define (node s left right)
  (list s left right))

(define (leaf? t) (number? t))

(define (lson t) (cadr t))

(define (rson t) (caddr t))

(define (contents-of t)
  (if (number? t)
      t
      (car t)))

(define example
  (node 'a
        (node 'l (leaf 3) (leaf 4))
        (leaf 5)))