#lang eopl

; Exercise B.1 from EoPL

(provide lexical-spec
         grammar)

(require rackunit)

(define lexical-spec
  '((white-sp (whitespace) skip)
    (mul-op ((or "*" "/")) symbol)
    (add-op ((or "+" "-")) symbol)
    (number (digit (arbno digit)) number)))

(define grammar
  '((arith-expr (arith-term (arbno add-op arith-term)) an-expr)
    (arith-term (arith-fact (arbno mul-op arith-fact)) a-term)
    (arith-fact (number) fact-num)
    (arith-fact ("(" arith-expr ")") fact-expr)))

(sllgen:make-define-datatypes lexical-spec grammar)

(define scan&parse (sllgen:make-string-parser lexical-spec grammar))

(test-begin
 (check-equal? (scan&parse "1 + 2")
               (an-expr
                (a-term (fact-num 1) '() '())
                '(+)
                (list (a-term (fact-num 2) '() '()))))
 (check-equal? (scan&parse "1 * 2 + 4 + 6 / 7")
               (an-expr
                (a-term (fact-num 1) '(*) (list (fact-num 2)))
                '(+ +)
                (list (a-term (fact-num 4) '() '())
                      (a-term (fact-num 6) '(/) (list (fact-num 7))))))
 (check-equal? (scan&parse "6 / (2 + 4)")
               (an-expr
                (a-term (fact-num 6)
                        '(/)
                        (list
                         (fact-expr
                          (an-expr
                           (a-term (fact-num 2)
                                   '()
                                   '())
                           '(+)
                           (list
                            (a-term (fact-num 4) '() '()))))))
               '()
               '())))