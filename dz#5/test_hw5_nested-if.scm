(load "artem.rkt")
(load "unit-test.scm")


(display "\n-=ИНТЕРПРЕТАТОР: ВЛОЖЕННЫЕ IF=-\n\n")


(define tests
  (list (test (interpret #(if if 33 endif 44 endif 55) '(0 9 2)) '(55 9 2))
        (test (interpret #(if if 33 endif 44 endif 55) '(7 0 2)) '(55 44 2))
        (test (interpret #(if if 33 endif 44 endif 55) '(7 9 2))
              '(55 44 33 2))
        (test (interpret #(if if 33 endif 44 endif) '(0)) '())
        (test (interpret #(if if 33 endif 44 endif) '(7 0)) '(44))
        (test (interpret #(if if 33 endif 44 endif) '(7 9)) '(44 33))))


(define **test-succeed-17791** (run-tests tests))
