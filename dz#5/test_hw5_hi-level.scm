(load "artem.rkt")
(load "unit-test.scm")


(display "\n-=ИНТЕРПРЕТАТОР: ФУНКЦИИ ВЫСШИХ ПОРЯДКОВ=-\n\n")


(define tests
  (list (test (interpret #(define power
                               ; power : x λ n ≡ λ(λ(λ…λ(x)…)) (n раз)
                               dup 0 = if drop drop exit endif
                               rot                               ; n  λ  x
                               over                              ; n  λ  x  λ
                               apply                             ; n  λ  x′
                               rot                               ; x′ λ  n
                               1 -                               ; x′ λ  n−1
                               power                             ; рекурсивный вызов
                           end
                           define square dup * end
                           3 & square 3 power                  ; ((3²)²)² = 6561
                           2 lam dup dup * * endlam 2 power)   ; (2³)³ = 512
                         '())
              '(512 6561))

        ; Тест на вложенные лямбды. Да, по заданию они могут быть вложенными.
        (test (interpret #(define if-else
                               ; if-else : … value λ-then λ-else  →
                               ;                  → λ-then apply, если value ≠ 0
                               ;                  → λ-else apply, если value = 0
                               rot if swap endif drop apply
                           end
                           define select
                               dup 3 mod 0 =
                               lam
                                   dup 5 mod 0 =
                                   lam drop -777888 endlam ;; fizz-buzz
                                   lam drop -777 endlam    ;; fizz
                                   if-else
                               endlam
                               lam
                                   dup 5 mod 0 =
                                   lam drop -888 endlam    ;; buzz
                                   lam endlam              ;; просто число
                                   if-else
                               endlam
                               if-else
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


(define **test-succeed-25363** (run-tests tests))
