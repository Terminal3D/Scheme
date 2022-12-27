(define-syntax test
  (syntax-rules ()
    ((test x y)
     (begin
       '(x y)
       ))))


(define (run-tests txs)
  (if (not(null? txs))
      (write (car (car txs)))
      #t)
  (if (not(null? txs))
      (if (equal? (eval `,(car (car txs)) (interaction-environment)) (cadr (car txs)))
          (begin
            (display "ok")
            (newline)
            (run-tests (cdr txs)))
          (begin
            (display "FAIL")
            (newline)
            (write 'Expected:)
            (write (cadr (car txs)))
            (newline)
            (write 'Returned:)
            (write (eval `,(car (car txs)) (interaction-environment)))
            (newline)
            (run-tests (cdr txs))
            #f))
      #t))
          

(define (run-test tst)
  (run-tests (cons tst '())))