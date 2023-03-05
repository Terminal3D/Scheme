(load "artem.rkt")
(load "unit-test.scm")


(display "\n-=ИНТЕРПРЕТАТОР: ВЛОЖЕННЫЙ ЦИКЛ С ПОСТУСЛОВИЕМ=-\n\n")


(define tests
  (list (test (interpret #(define -- 1 - end
                           define fact-sum   ; 1! + 2! + … + n!
                               0 over                       ; n 0 n
                               if repeat                    ; n sum
                                   swap 1 over dup          ; sum n 1 n n
                                   if repeat                ; sum n prod k
                                       swap over *          ; sum n k prod×k
                                       swap --              ; sum n prod×k k−1
                                       dup                  ; sum n prod×k k−1 k−1
                                   0 = until endif          ; sum n n! 0
                                   drop                     ; sum n n!
                                   swap                     ; sum n! n
                                   rot                      ; n n! sum
                                   +                        ; n n!+sum
                                   swap -- swap over        ; n−1 sum’ n−1
                               0 = until endif              ; 0 sum
                               +
                           end
                           0 fact-sum
                           1 fact-sum
                           2 fact-sum
                           3 fact-sum
                           4 fact-sum)
                         '())
              '(33 9 3 1 0))))


(define **test-succeed-7202** (run-tests tests))
