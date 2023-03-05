(load "artem.rkt")
(load "unit-test.scm")


(display "\n-=ИНТЕРПРЕТАТОР: IF С АЛЬТЕРНАТИВНОЙ ВЕТКОЙ=-\n\n")


(define tests
  (list (test (interpret #(if 111 else 222 endif 7) '(1 3)) '(7 111 3))
        (test (interpret #(if 111 else 222 endif 7) '(0 3)) '(7 222 3))
        (test (interpret #(if 111 else 222 endif) '(1)) '(111))
        (test (interpret #(if 111 else 222 endif) '(0)) '(222))))


(define **test-succeed-12111** (run-tests tests))
