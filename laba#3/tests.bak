(define-syntax test
  (syntax-rules ()
    ((test x y)
     (begin
       (write 'x)
       '(x y)
       ))))



(define (run-tests txs)
  (if (not(null? txs))
      (if (equal? (car (car txs)) "FAIL")
          (begin
            (write (cadr (car txs)))
            (write (car (car txs)))
            (newline)
            (write 'Expected:)
            (write (caddr (car txs)))
            (newline)
            (write 'Returned:)
            (write (car (eval `(list ,(cadr (car txs))) (interaction-environment))))
            (newline)
            (run-tests (cdr txs))
            #f)
          (begin
            (write (cadr (car txs)))
            (write (car (car txs)))
            (newline)
            (run-tests (cdr txs))))
      #t))

(define (run-test tst)
  (run-tests (cons tst '())))