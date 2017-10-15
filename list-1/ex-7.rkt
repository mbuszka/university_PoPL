#lang racket

(define fact
  (let ((f (lambda (h)
             (lambda (n)
               (if (= n 0)
                   1
                   (* n ((h h) (- n 1))))))))
    (f f)))