#lang racket

(define (self-app f) (f f))

(define (prepare-cont f)
  (lambda (self)
      (f (lambda (x) ((self-app self) x)))))

(define (fix f)
  (self-app (prepare-cont f)))

(define (fact-maker f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1))))))

(define (fact n)
  ((fix fact-maker) n))
