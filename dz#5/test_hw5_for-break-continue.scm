(load "artem.rkt")
(load "unit-test.scm")


(display "\n-=ИНТЕРПРЕТАТОР: FOR И BREAK/CONTINUE=-\n\n")


(define tests
  (list (test (interpret #(1 40 for
                               i 3 mod 0 = if continue endif
                               i 22 > if break endif
                               i
                           next)
                         '())
              '(22 20 19 17 16 14 13 11 10 8 7 5 4 2 1))))

(define **test-succeed-6817** (run-tests tests))
