(load "homework61.scm")
(load "unit-test.scm")


(display "\n-=РОБОТ-ПРОВЕРЯЛЬЩИК ВЫПОЛНЯЕТ ТЕСТЫ ДЛЯ ТОКЕНИЗАТОРА=-\n\n")


(define tests
  (list (test (tree->scheme (parse (tokenize "x^(a + 1)"))) '(expt x (+ a 1)))
        (test (eval (tree->scheme (parse (tokenize "2^2^2^2")))
                    (interaction-environment))
              '65536)

        (test (let ((f (eval `(lambda (a b c d)
                                ,(tree->scheme (parse (tokenize "a*b + c/d"))))
                             (interaction-environment))))
                (f 5 6 7 8))
              (+ (* 5 6) (/ 7 8)))))


(define **test-succeed-24398** (run-tests tests))
