(load "artem.rkt")
(load "unit-test.scm")


(display "\n-=ИНТЕРПРЕТАТОР: ЦИКЛ С ПРЕДУСЛОВИЕМ=-\n\n")


(define tests
  (list (test (interpret #(while wend) '(3 7 4 0 5 9)) '(5 9))
        (test (interpret #(while wend) '(0 7 4 0 5 9)) '(7 4 0 5 9))

        (test (interpret #(define sum
                              dup
                              while + swap dup wend
                              drop
                           end
                           1 2 3 0 4 5 6 sum)
                         '())
              '(15 3 2 1))

        (test (interpret #(define sum
                               0 swap dup         ; … xn … x2 0 x1 x1
                               while              ; … xn … xi+1 sum xi xi
                                   + swap dup
                               wend               ; … sum xn
                               drop
                           end
                           sum)
                         '(6 5 4 0 3 2 1))
              '(15 3 2 1))

        (test (interpret #(define sum
                               0 swap dup         ; … 0 xn … x2 0 x1 x1
                               while              ; … 0 xn … xi+1 sum xi
                                   + swap dup
                               wend               ; … sum 0
                               drop
                           end
                           sum)
                         '(0))
              '(0))

        (test (interpret #(define power2
                               1 swap dup
                               while
                                   swap 2 * swap 1 - dup
                               wend
                               drop
                           end
                           5 power2
                           3 power2 power2)
                         '())
              '(256 32))))


(define **test-succeed-1809** (run-tests tests))
