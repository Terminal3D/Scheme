(load "artem.rkt")
(load "unit-test.scm")


(display "\n-=ИНТЕРПРЕТАТОР: WHILE И BREAK/CONTINUE=-\n\n")


(define tests
  (list (test (interpret #(0 swap dup
                           while
                               dup 3 = if break endif
                               swap 1000 * over +
                               swap 2 -
                               dup 0 >
                           wend
                           drop)
                         '(15))
              '(15013011009007005))
        (test (interpret #(0 swap dup
                           while
                               dup 3 = if break endif
                               swap 1000 * over +
                               swap 2 -
                               dup
                           wend
                           drop)
                         '(14))
              '(14012010008006004002))
        (test (interpret #(0 swap dup
                           while
                               dup 2 mod if 1 - dup continue endif
                               swap 1000 * over +
                               swap 1 -
                               dup
                           wend
                           drop)
                         '(15))
              '(14012010008006004002))
        (test (interpret #(0 swap dup
                           while
                               dup 2 mod if 1 - dup continue endif
                               swap 1000 * over +
                               swap 1 -
                               dup
                           wend
                           drop)
                         '(14))
              '(14012010008006004002))))

(define **test-succeed-26291** (run-tests tests))
