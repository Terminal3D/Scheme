(load "homework3.scm")
(load "unit-test.scm")

(display "\n-=РОБОТ-ПРОВЕРЯЛЬЩИК ПРОВЕРЯЕТ МАКРОС flatten=-\n\n")

(define (f x y z)
  (flatten (((+ x) 3 (4 y) (((z)))) (7 y x 8))))  ;; (+ x 3 4 y z 7 y x 8)


(define tests
  (list (test (f 1 1 1) 27)
        (test (let ((a 5)
                    (b 7)
                    (c 1))
                (flatten ((* (((a) (b)) c))))) ;; (* a b c)
              35)))


(define **test-succeed-6694** (run-tests tests))
