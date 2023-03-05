(load "artem.rkt")
(load "unit-test.scm")


(display "\n-=ИНТЕРПРЕТАТОР: ХВОСТОВЫЕ ВЫЗОВЫ=-\n\n")


(define tests
  (list (test (interpret #(define F 11 22 33 tail G 44 55 end
                           define G 77 88 99 end
                           F)
                         '())
              '(99 88 77 33 22 11))
        (test (interpret #(define =0? dup 0 = end
                           define gcd
                               =0? if drop exit endif
                               swap over mod
                               tail gcd
                           end
                           90 99 gcd
                           234 8100 gcd)
                         '())
              '(18 9))))


(define **test-succeed-21604** (run-tests tests))
