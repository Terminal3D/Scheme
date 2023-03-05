(load "artem.rkt")
(load "unit-test.scm")


(display "\n-=ИНТЕРПРЕТАТОР: ЦИКЛ СО СЧЁТЧИКОМ=-\n\n")


(define tests
  (list (test (interpret #(define fact
                               1 1 rot for i * next
                           end
                           0 fact
                           1 fact
                           6 fact
                           10 fact)
                         '())
              '(3628800 720 1 1))
        (test (interpret #(1 100 for
                               ;; просто число
                               i 3 mod 0 = not i 5 mod 0 = not and if i endif
                               ;; fizz
                               i 3 mod 0 = i 5 mod 0 = not and if -777 endif
                               ;; buzz
                               i 3 mod 0 = not i 5 mod 0 = and if -888 endif
                               ;; fizz-buzz
                               i 3 mod 0 = i 5 mod 0 = and if -777888 endif
                           next)
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


(define **test-succeed-29304** (run-tests tests))
