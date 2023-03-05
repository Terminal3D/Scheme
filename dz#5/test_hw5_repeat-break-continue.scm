(load "artem.rkt")
(load "unit-test.scm")


(display "\n-=ИНТЕРПРЕТАТОР: REPEAT И BREAK/CONTINUE=-\n\n")


(define tests
  (list (test (interpret #(define factorize
                             2
                             repeat     ; ... n d
                               over over = if drop break endif
                               over over mod if 1 + 0 continue endif
                               dup      ; ... n d d
                               rot      ; ... d d n
                               swap     ; ... d n d
                               /        ; ... d n/d
                               over     ; ... d n/d d
                             0 until
                           end
                           12 factorize)
                         '())
              '(3 2 2))
        (test (interpret #(define factorize
                             2
                             repeat     ; ... n d
                               over over = if drop break endif
                               over over mod if 1 + 0 continue endif
                               dup      ; ... n d d
                               rot      ; ... d d n
                               swap     ; ... d n d
                               /        ; ... d n/d
                               over     ; ... d n/d d
                             0 until
                           end
                           20 factorize)
                         '())
              '(5 2 2))
        (test (interpret #(define factorize
                             2
                             repeat     ; ... n d
                               over over = if drop break endif
                               over over mod if 1 + 0 continue endif
                               dup      ; ... n d d
                               rot      ; ... d d n
                               swap     ; ... d n d
                               /        ; ... d n/d
                               over     ; ... d n/d d
                             0 until
                           end
                           16 factorize)
                         '())
              '(2 2 2 2))
        (test (interpret #(define factorize
                             2
                             repeat     ; ... n d
                               over over = if drop break endif
                               over over mod if 1 + 0 continue endif
                               dup      ; ... n d d
                               rot      ; ... d d n
                               swap     ; ... d n d
                               /        ; ... d n/d
                               over     ; ... d n/d d
                             0 until
                           end
                           120 factorize)
                         '())
              '(5 3 2 2 2))
        (test (interpret #(define factorize
                             2
                             repeat     ; ... n d
                               over over = if drop break endif
                               over over mod if 1 + 0 continue endif
                               dup      ; ... n d d
                               rot      ; ... d d n
                               swap     ; ... d n d
                               /        ; ... d n/d
                               over     ; ... d n/d d
                             0 until
                           end
                           49 factorize)
                         '())
              '(7 7))))

(define **test-succeed-2145** (run-tests tests))
