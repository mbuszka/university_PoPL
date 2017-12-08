(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
       ;; simple arithmetic
      (positive-const "print 11" 11)
      (negative-const "print -33" -33)
      (simple-arith-1 "print -(44,33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "print -(-(44,33),22)" -11)
      (nested-arith-right "print -(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "print x" 10)
      (test-var-2 "print -(x,1)" 9)
      (test-var-3 "print -(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "print foo" error)
      (test-unbound-var-2 "print -(x,foo)" error)
  
      ;; simple conditionals
      (if-true "print if zero?(0) then 3 else 4" 3)
      (if-false "print if zero?(1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "print -(zero?(0),1)" error)
      (no-bool-to-diff-2 "print -(1,zero?(0))" error)
      (no-int-to-if "print if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "print if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "print if zero?(-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "print if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "print if zero?(-(11,12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "print let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "print let x = 3 in -(x,1)" 2)
      (eval-let-rhs "print let x = -(4,1) in -(x,1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "print let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "print let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "print let x = 3 in let x = -(x,1) in x" 2)

      ;; simple applications
      (apply-proc-in-rator-pos "print (proc(x) -(x,1)  30)" 29)
      (apply-simple-proc "print let f = proc (x) -(x,1) in (f 30)" 29)
      (let-to-proc-1 "print (proc(f)(f 30)  proc(x)-(x,1))" 29)


      (nested-procs "print ((proc (x) proc (y) -(x,y)  5) 6)" -1)
      (nested-procs2 "print let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)"
        -1)
      
       (y-combinator-1 "print
let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)" 12)
      
       ;; simple letrecs
      (simple-letrec-1 "print letrec f(x) = -(x,1) in (f 33)" 32)
      (simple-letrec-2
        "print letrec f(x) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)"
        8)

      (simple-letrec-3
        "print let m = -5 
 in letrec f(x) = if zero?(x) then 0 else -((f -(x,1)), m) in (f 4)"
        20)
      
;      (fact-of-6  "letrec
;  fact(x) = if zero?(x) then 1 else *(x, (fact sub1(x)))
;in (fact 6)" 
;                  720)
      
      (HO-nested-letrecs
"print letrec even(odd)  = proc(x) if zero?(x) then 1 else (odd -(x,1))
   in letrec  odd(x)  = if zero?(x) then 0 else ((even odd) -(x,1))
   in (odd 13)" 1)

      (assignment-test
       "print let x = 17
        in let d = set x = 27
        in x"
       27)

      (gensym-test
       "print let g = let count = 0
                in proc(d)
                   let d = set count = -(count,-1)
                   in count
        in -((g 11), (g 22))"
       -1)

      (letrec-mult
       "print let s = 0
        in let mult =
           proc (x)
             letrec inner (y) = if zero?(y) then s
                                else let d = set s = -(s, -(0, x))
                                     in (inner -(y, 1))
             in inner
        in ((mult 4) 5)"
       20)

      (book-example-1 "var x, y; { x = 3; y = 4; print -(x, y) }" -1)

      (book-example-2 "
var x, y, z, nz;
{ x = 3
; y = 4
; z = 0
; nz = proc (x) if zero?(x) then 1 else 0
; while zero?((nz x))
{ z = -(z,y)
    ; x = -(x,1) }
    ; print z }" -12)

        (book-example-3 "
var x;
{ x = 3
; print x
; var x;
    { x = 4
    ; print x }
; print x }" (3 4 3))

        (book-example-4 "
var f, x;
{ f = proc(x) proc(y) -(x,y)
    ; x = 3
    ; print ((f 4) x) }" 1)


      ))
  )
