(load "artem.rkt")
(load "unit-test.scm")


(display "\n-=ИНТЕРПРЕТАТОР: ГЛОБАЛЬНЫЕ ПЕРЕМЕННЫЕ=-\n\n")


(define tests
  (list (test (interpret #(defvar counter 0
                           define nextnum
                               counter dup 1 + set counter
                           end
                           nextnum nextnum
                           nextnum nextnum +
                           nextnum nextnum *)
                         '())
              '(20 5 1 0))))


(define **test-succeed-14454** (run-tests tests))
