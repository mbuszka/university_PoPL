#lang racket

(define (exp k)
  (if (= k 0)
      (lambda (n) 1)
      (if (= k 1)
          (lambda (n) n)
          (let* ([m (quotient k 2)]
                 [r (remainder k 2)]
                 [f (exp m)])
            (lambda (n) (let ([res (f n)])
                           (if (= r 1)
                               (* res res n)
                               (* res res))))))))
            