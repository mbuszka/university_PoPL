#lang racket

; Exercise 1.28 from EoPL

(define (merge loi1 loi2)
  (if (null? loi1)
      loi2
      (if (null? loi2)
          loi1
          (if (< (car loi1) (car loi2))
              (cons (car loi1) (merge (cdr loi1) loi2))
              (cons (car loi2) (merge loi1 (cdr loi2)))))))
     