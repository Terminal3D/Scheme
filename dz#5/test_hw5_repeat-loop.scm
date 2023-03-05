(load "artem.rkt")
(load "unit-test.scm")


(display "\n-=ИНТЕРПРЕТАТОР: ЦИКЛ С ПОСТУСЛОВИЕМ=-\n\n")


(define binary
  #(define =0? dup 0 = end
    repeat
        dup 2 mod swap
        2 /
        =0? until
        drop))

(display "binary: ")
(write binary)
(newline)
(newline)


(define tests
  (list (test (interpret #(define =0? dup 0 = end
                           5 repeat 1 swap 1 - =0? until drop)
                         '())
              '(1 1 1 1 1))
        (test (interpret binary '(0)) '(0))
        (test (interpret binary '(1)) '(1))
        (test (interpret binary '(2)) '(1 0))
        (test (interpret binary '(13)) '(1 1 0 1))
        (test (interpret #(define =0? dup 0 = end
                           define -- 1 - end
                           define factorial
                               =0? if 1 + exit endif
                               1
                               repeat             ; n prod
                                   over *         ; n prod×n
                                   swap -- swap   ; n−1 prod×n
                               over 0 = until
                               +
                           end
                           0 factorial
                           1 factorial
                           2 factorial
                           3 factorial
                           4 factorial)
                         '())
              '(24 6 2 1 1))))


(define **test-succeed-5523** (run-tests tests))
