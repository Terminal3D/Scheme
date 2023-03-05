(load "artem.rkt")
(load "unit-test.scm")


(display "\n-=ИНТЕРПРЕТАТОР: ВЛОЖЕННЫЕ IF С АЛЬТЕРНАТИВНОЙ ВЕТКОЙ=-\n\n")


(define tests
  (list (test (interpret #(if
                               if 55 else 66 endif
                           else
                               if 77 else 88 endif
                           endif)
                         '( 0  0))
              '(88))
        (test (interpret #(if
                               if 55 else 66 endif
                           else
                               if 77 else 88 endif
                           endif)
                         '(10  0))
              '(66))
        (test (interpret #(if
                               if 55 else 66 endif
                           else
                               if 77 else 88 endif
                           endif)
                         '( 0 10))
              '(77))
        (test (interpret #(if
                               if 55 else 66 endif
                           else
                               if 77 else 88 endif
                           endif)
                         '(10 10))
              '(55))
        (test (interpret #(define select
                               dup 3 mod 0 = if
                                   dup 5 mod 0 = if
                                       drop -777888  ;; fizz-buzz
                                   else
                                       drop -777     ;; fizz
                                   endif
                               else
                                   dup 5 mod 0 = if
                                       drop -888     ;; buzz
                                   endif
                               endif
                           end
                           define fizzbuzz
                               dup 100 > if
                                   drop
                               else
                                   dup select swap 1 + fizzbuzz
                               endif
                           end
                           1 fizzbuzz)
                         '())
              '(-888    -777   98   97 -777    -888   94 -777   92   91
                -777888   89   88 -777   86    -888 -777   83   82 -777
                -888      79 -777   77   76 -777888   74   73 -777   71
                -888    -777   68   67 -777    -888   64 -777   62   61
                -777888   59   58 -777   56    -888 -777   53   52 -777
                -888      49 -777   47   46 -777888   44   43 -777   41
                -888    -777   38   37 -777    -888   34 -777   32   31
                -777888   29   28 -777   26    -888 -777   23   22 -777
                -888      19 -777   17   16 -777888   14   13 -777   11
                -888    -777    8    7 -777    -888    4 -777    2    1))))


(define **test-succeed-106** (run-tests tests))
