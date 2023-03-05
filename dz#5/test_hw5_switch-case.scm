(load "artem.rkt")
(load "unit-test.scm")


(display "\n-=ИНТЕРПРЕТАТОР: КОНСТРУКЦИЯ SWITCH - CASE=-\n\n")


(define tests
  (list (test (interpret #(switch
                               case 1 10 exitcase
                               case 2 20 ;; проваливаемся
                               case 3 30 exitcase
                               case 4 case 5 45 exitcase
                               case 6 60 ;; здесь exitcase не нужен
                           endswitch)
                         '(1))
              '(10))
        (test (interpret #(switch
                               case 1 10 exitcase
                               case 2 20 ;; проваливаемся
                               case 3 30 exitcase
                               case 4 case 5 45 exitcase
                               case 6 60 ;; здесь exitcase не нужен
                           endswitch)
                         '(2))
              '(30 20))
        (test (interpret #(switch
                               case 1 10 exitcase
                               case 2 20 ;; проваливаемся
                               case 3 30 exitcase
                               case 4 case 5 45 exitcase
                               case 6 60 ;; здесь exitcase не нужен
                           endswitch)
                         '(3))
              '(30))
        (test (interpret #(switch
                               case 1 10 exitcase
                               case 2 20 ;; проваливаемся
                               case 3 30 exitcase
                               case 4 case 5 45 exitcase
                               case 6 60 ;; здесь exitcase не нужен
                           endswitch)
                         '(4))
              '(45))
        (test (interpret #(switch
                               case 1 10 exitcase
                               case 2 20 ;; проваливаемся
                               case 3 30 exitcase
                               case 4 case 5 45 exitcase
                               case 6 60 ;; здесь exitcase не нужен
                           endswitch)
                         '(5))
              '(45))
        (test (interpret #(switch
                               case 1 10 exitcase
                               case 2 20 ;; проваливаемся
                               case 3 30 exitcase
                               case 4 case 5 45 exitcase
                               case 6 60 ;; здесь exitcase не нужен
                           endswitch)
                         '(6))
              '(60))
        (test (interpret #(switch
                               case 1 10 exitcase
                               case 2 20 ;; проваливаемся
                               case 3 30 exitcase
                               case 4 case 5 45 exitcase
                               case 6 60 ;; здесь exitcase не нужен
                           endswitch)
                         '(7))
              '())

        (test (interpret #(define select
                               dup 15 mod
                               switch
                                   case 0
                                       -777888  ;; fizz-buzz
                                   exitcase
                                   case 3 case 6 case 9 case 12
                                       -777     ;; fizz
                                   exitcase
                                   case 5 case 10
                                       -888     ;; buzz
                                   exitcase
                               endswitch
                               dup 0 < if swap drop exit endif
                           end
                           define fizzbuzz
                               dup 100 > if drop exit endif
                               dup select swap 1 + fizzbuzz
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


(define **test-succeed-5752** (run-tests tests))
