#lang racket

; Exercise 1.34 from EoPL

(define (value t) (car t))

(define (lson t) (cadr t))

(define (rson t) (caddr t))

(define (path n bst)
  (if (eq? n (value bst))
      '()
      (if (< n (value bst))
          (cons 'left (path n (lson bst)))
          (cons 'right (path n (rson bst))))))

(define example
  '(14 (7 ()
          (12 ()
              ()))
       (26 (20 (17 ()
                   ())
               ())
           (31 ()
               ()))))