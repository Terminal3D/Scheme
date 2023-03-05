(load "homework61.scm")
(load "unit-test.scm")


(display "\n-=РОБОТ-ПРОВЕРЯЛЬЩИК ВЫПОЛНЯЕТ ТЕСТЫ ДЛЯ ПАРСЕРА=-\n\n")


(define tests
        ; Ассоциативность левая
  (list (test (parse (tokenize "a/b/c/d")) '(((a / b) / c) / d))
        ; Ассоциативность правая
        (test (parse (tokenize "a^b^c^d")) '(a ^ (b ^ (c ^ d))))
        ; Порядок вычислений задан скобками
        (test (parse (tokenize "a/(b/c)")) '(a / (b / c)))
        ; Порядок вычислений определен только приоритетом операций
        (test (parse (tokenize "a + b/c^2 - d")) '((a + (b / (c ^ 2))) - d))

        (test (parse '(- a + b * x ^ 2 + dy)) '(((- a) + (b * (x ^ 2))) + dy))
        (test (parse '(a * "(" b + c ")")) '(a * (b + c)))
        (test (parse '(a * "(" "(" b ")" + c ")")) '(a * (b + c)))
        (test (parse '(a)) 'a)
        (test (parse '(1)) 1)
        (test (parse '(- a)) '(- a))
        (test (parse '("(" "(" "(" "(" 1 + 1 ")" ")" ")" ")")) '(1 + 1))
        (test (parse '("(" "(" "(" "(" 0 ")" ")" ")" ")")) 0)
        
        (test (parse '(a * "(" b + c)) #f)
        (test (parse '(a * b + c ")")) #f)
        (test (parse '(a * b + )) #f)
        (test (parse '(a * "(" b + + c ")")) #f)
        (test (parse '(* "(" b + + c ")")) #f)
        (test (parse '(a * "(" b + c ")" ")")) #f)
        (test (parse '(")" a + b "(")) #f)
        (test (parse '("(" a + b ")" "(" c - d ")")) #f)
        (test (parse '(1 2)) #f)
        (test (parse '(a 1)) #f)
        (test (parse '()) #f)
        (test (parse '("(")) #f)
        (test (parse '(")")) #f)
        (test (parse '("(" ")")) #f)))


(define **test-succeed-20548** (run-tests tests))