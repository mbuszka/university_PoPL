#lang racket

; Exercise 1.30 from EoPL

(define (split-acc acc lst)
  (if (null? lst)
      acc
      (if (null? (cdr lst))
          (cons (cons (car lst) (car acc)) (cdr acc))
          (split-acc (cons (cons (car lst) (car acc)) (cons (cadr lst) (cdr acc))) (cddr lst)))))

(define (split lst) (split-acc '(()) lst))

(define (merge/pred pred loi1 loi2)
  (if (null? loi1)
      loi2
      (if (null? loi2)
          loi1
          (if (pred (car loi1) (car loi2))
              (cons (car loi1) (merge/pred pred (cdr loi1) loi2))
              (cons (car loi2) (merge/pred pred loi1 (cdr loi2)))))))

(define (sort/pred pred loi)
  (if (null? loi)
      loi
      (if (null? (cdr loi))
          loi
          (let ([pair (split loi)])
            (merge/pred pred (sort/pred pred (car pair)) (sort/pred pred (cdr pair)))))))